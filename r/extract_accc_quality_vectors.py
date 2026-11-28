"""Extract ACCC quality ratings from the vector paths in two specific PDFs.
Usage: python extract_accc_quality_vectors.py --input-dir data/australia --output-dir output
Dependency: pip install pymupdf
The chart-specific configuration is intentionally explicit; this is not a generic PDF digitizer.
"""
import argparse
import csv
import re
from pathlib import Path
import fitz

AIRPORTS = ('BNE', 'MEL', 'PER', 'SYD')
OLD = [(0.232, .188, .572), (.190, .749, .779), (.603, .793, .235), (1., .773, .008)]
NEW = [(.206, .191, .546), (.190, .749, .779), (.261, .586, .223), (.831, .657, .163)]
# PDF page numbers are one-based; vertical axis path index is zero-based.
# Minimum horizontal segment length excludes legend strokes and small drawing stubs.
SPECS = [
    (2021, '4.2', 67, 58, 'aeronautical_quality', 15, OLD, 21, 10),
    (2025, '3.5', 34, 23, 'overall_quality', 10, [(.166,.207,.528),(.110,.812,.788),NEW[2],NEW[3]], 5, 20),
    (2025, '3.6', 36, 25, 'passenger_quality', 5, NEW, 5, 20),
    (2025, '3.7', 38, 27, 'airline_quality', 8, NEW, 5, 20),
]

def same_colour(a,b):
    return a is not None and max(abs(x-y) for x,y in zip(a,b)) < .003

def extract(doc, spec):
    edition, fig, pdf_page, printed, metric, axis_idx, colours, expected_segments, min_dx = spec
    page = doc[pdf_page-1]
    if not re.search(r'Figure\s+'+re.escape(fig)+r':',page.get_text()):
        raise ValueError(f'Figure {fig} not found on page {pdf_page}')
    paths = page.get_drawings()
    # Axis endpoints: score 5 at the top, score 1 at the bottom.
    axis = paths[axis_idx]
    verticals = [i for i in axis['items'] if i[0]=='l' and abs(i[1].x-i[2].x)<.01 and abs(i[1].y-i[2].y)>100]
    if len(verticals)!=1:
        raise ValueError(f'Unexpected axis geometry: {fig}')
    _, a, b = verticals[0]
    top, bottom = sorted([a.y,b.y])
    labels = dict(re.findall(r'\b(BNE|MEL|PER|SYD):\s*([1-5]\.\d{2})\b',page.get_text())) if edition==2025 else {}
    if edition==2025 and set(labels)!=set(AIRPORTS):
        raise ValueError(f'Missing final-year labels: {fig}')
    result = []
    for airport, colour in zip(AIRPORTS,colours):
        count = expected_segments - int(edition==2021 and airport=='SYD')
        candidates=[]
        for path in paths:
            if not same_colour(path['color'],colour) or path['dashes']!='[] 0':
                continue
            segs=[i for i in path['items'] if i[0]=='l' and abs(i[1].x-i[2].x)>min_dx and all(top<=p.y<=bottom for p in i[1:])]
            if len(segs)==count:
                candidates.append(segs)
        if len(candidates)!=1:
            raise ValueError(f'Expected unique series: {fig} {airport}, got {len(candidates)}')
        pts=sorted(set((round(p.x,3),round(p.y,3)) for seg in candidates[0] for p in seg[1:]))
        years = list(range(1999 if airport=='SYD' else 1998,2020)) if edition==2021 else [2016,2017,2018,2019,2023,2024,2025]
        if len(pts)!=len(years):
            raise ValueError(f'Unexpected vertex count: {fig} {airport}')
        # Validate x spacing against elapsed financial years (allow minor artwork shifts).
        dx=(pts[-1][0]-pts[0][0])/(years[-1]-years[0])
        if any(abs(x-(pts[0][0]+dx*(year-years[0])))>2.0 for (x,y),year in zip(pts,years)):
            raise ValueError(f'Unexpected year spacing: {fig} {airport}')
        for (x,y),year in zip(pts,years):
            score=5-4*(y-top)/(bottom-top)
            printed_value=float(labels[airport]) if year==2025 else None
            error=score-printed_value if printed_value is not None else None
            if error is not None and abs(error)>.025:
                raise ValueError(f'Validation error exceeds 0.025: {fig} {airport}: {error}')
            result.append(dict(airport=airport,year=year,fyear=f'{year-1}-{year%100:02d}',metric=metric,
                rating=printed_value if printed_value is not None else round(score,2),
                rating_vector=round(score,6),rating_printed=printed_value,
                validation_error=round(error,6) if error is not None else None,
                value_method='published_label' if printed_value is not None else 'pdf_vector',
                report=f'accc_monitor_{edition}.pdf',figure=fig,pdf_page=pdf_page,printed_page=printed,
                x_pdf_pt=x,y_pdf_pt=y,axis_top_pdf_pt=round(top,6),axis_bottom_pdf_pt=round(bottom,6)))
    return result

def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--input-dir',type=Path,default=Path('.'))
    parser.add_argument('--output-dir',type=Path,default=Path('.'))
    args=parser.parse_args(); rows=[]
    for spec in SPECS:
        with fitz.open(args.input_dir/f'accc_monitor_{spec[0]}.pdf') as doc:
            rows.extend(extract(doc,spec))
    if len(rows)!=171 or len({(r['airport'],r['year'],r['metric']) for r in rows})!=171:
        raise ValueError('Unexpected total or duplicate keys')
    args.output_dir.mkdir(parents=True,exist_ok=True)
    path=args.output_dir/'accc_quality_vector_ratings.csv'
    with path.open('w',newline='',encoding='utf-8') as f:
        writer=csv.DictWriter(f,fieldnames=list(rows[0])); writer.writeheader(); writer.writerows(rows)
    errors=[abs(r['validation_error']) for r in rows if r['validation_error'] is not None]
    print(f'{len(rows)} observations; {len(errors)} label checks; max absolute error {max(errors):.4f}; wrote {path}')

if __name__=='__main__':
    main()
