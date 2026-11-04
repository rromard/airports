#!/usr/bin/env python3
"""Extract five ACCC 2006-07 comparative bar charts from the original vector PDF.

Usage: python extract_comparative_charts.py REPORT.pdf OUTPUT.csv
Requires: python -m pip install pymupdf

Outputs one observation per drawn bar, keeping native units. Financial-year `year`
is the year ending the Australian financial year (e.g. 2007 = 2006-07).
Missing bars are not fabricated. Geometry, calibration, and bar segment counts
are preserved for checking. This is specific to the 2006-07 ACCC report.
"""
import csv
import re
import sys
from collections import defaultdict
from pathlib import Path
import pymupdf
from types import SimpleNamespace

# PDF page numbers are 1-based. Plotting-area y bounds exclude the legend bars.
CHARTS = {
    32: dict(chart='2.1', measure='passengers', adjustment='not_applicable', units='passengers', top=440, bottom=681),
    33: dict(chart='2.2', measure='aero_revenue_per_pax', adjustment='ansett_adjusted', units='AUD per passenger', top=330, bottom=560),
    35: dict(chart='2.4', measure='aero_revenue_total', adjustment='reported', units='AUD thousands', top=337, bottom=571),
    38: dict(chart='2.7', measure='aero_expenditure_per_pax', adjustment='reported', units='AUD per passenger', top=115, bottom=337),
    39: dict(chart='2.8', measure='aero_operating_profit_per_pax', adjustment='reported', units='AUD per passenger', top=115, bottom=337),
}
AIRPORTS = ('MEL', 'PER', 'BNE', 'SYD', 'ADL', 'CBR', 'DRW')

# The source charts cover 1997-98 through 2006-07. Some airports have
# shorter histories, but their final plotted bar must be 2006-07.
FIRST_YEAR = 1998
LAST_YEAR = 2007


def axis_ticks(page, number, spec):
    ticks = []
    for word in page.get_text('words'):
        x0, y0, x1, y1, s = word[:5]
        if not (79 < x0 < 115 and spec['top']-16 < (y0+y1)/2 < spec['bottom']+6):
            continue
        token = s.strip().replace(',', '').replace('−', '-').replace('–', '-')
        if token in ('$-', '$0'):
            value = 0.0
        else:
            # Report uses -$4, $5, 50000, 5,000,000, and 0.
            match = re.fullmatch(r'(-?)\$?(\d+(?:\.\d+)?)', token)
            if not match:
                continue
            value = float(match.group(2)) * (-1 if match.group(1) else 1)
        ticks.append((value, (y0+y1)/2))
    # Deduplicate ticks by value in case nearby document text overlaps.
    unique = {}
    for value, y in ticks:
        unique.setdefault(value, y)
    ticks = [(value, y) for value, y in unique.items()]
    if len(ticks) < 4:
        raise ValueError(f'Page {number}: expected >=4 axis ticks, found {ticks}')
    av = sum(v for v, _ in ticks) / len(ticks)
    ay = sum(y for _, y in ticks) / len(ticks)
    slope = sum((v-av)*(y-ay) for v, y in ticks) / sum((v-av)**2 for v, y in ticks)
    zero = ay - slope*av
    worst_error = max(abs(y-(zero+slope*v)) for v, y in ticks)
    if worst_error > 3:
        raise ValueError(f'Page {number}: suspicious y-axis calibration, max tick residual={worst_error:.2f} PDF points')
    return ticks, zero, slope


def bar_groups(page, number, spec):
    bars = defaultdict(list)
    for draw in page.get_drawings():
        rect = draw['rect']
        if not (100 < rect.x0 < (505 if number == 32 else 485)):
            continue
        # Chart 2.1 has some outline-only bars, including a combined
        # 2-bar outline at x=215.2..224.7. The right half is the final
        # Perth bar. On all other charts we rely on filled geometry.
        if number == 32 and draw['fill'] is None and 9.2 < rect.width < 9.8:
            right_bar = SimpleNamespace(
                x0=rect.x0 + rect.width / 2, y0=rect.y0, y1=rect.y1
            )
            if spec['top'] < right_bar.y1 < spec['bottom']:
                bars[round(right_bar.x0, 1)].append(right_bar)
            continue
        if not (4.3 < rect.width < 5.2):
            continue
        if number != 32 and draw['fill'] is None:
            continue
        # Exclude legends below plots and other out-of-area decorations.
        if not (spec['top'] < rect.y1 < spec['bottom']):
            continue
        if not (spec['top']-3 < rect.y0 < spec['bottom']):
            continue
        bars[round(rect.x0, 1)].append(rect)
    xs = sorted(bars)
    groups = []
    for x in xs:
        if not groups or x - groups[-1][-1] > 11:
            groups.append([x])
        else:
            groups[-1].append(x)
    if len(groups) != 7 or any(not (7 <= len(group) <= 10) for group in groups):
        raise ValueError(f'Page {number}: unexpected bar groups {[(g[0], len(g)) for g in groups]}')
    return bars, groups


def validate_year_sequences(rows):
    """Catch missing, duplicated, or misassigned financial years within each chart."""
    for spec in CHARTS.values():
        for airport in AIRPORTS:
            years = sorted(
                row['year'] for row in rows
                if row['chart'] == spec['chart'] and row['airport'] == airport
            )
            if (
                not years
                or years[-1] != LAST_YEAR
                or years[0] < FIRST_YEAR
                or years != list(range(years[0], LAST_YEAR + 1))
            ):
                raise ValueError(
                    f'Unexpected year sequence for {airport}, '
                    f'Chart {spec["chart"]}: {years}'
                )


def extract(path):
    # Reconstruct bar values from PDF geometry using the calibrated y-axis.
    # Bar spacing assigns each observation to a financial year; this relies
    # on the fixed graphical layout of the 2006-07 report and is not a
    # general-purpose PDF chart extractor. Keep geometry for auditing.
    rows = []
    with pymupdf.open(path) as pdf:
        for number, spec in CHARTS.items():
            page = pdf[number - 1]
            ticks, zero, slope = axis_ticks(page, number, spec)
            bars, groups = bar_groups(page, number, spec)
            for airport, group in zip(AIRPORTS, groups):
                last_x = group[-1]  # final visible bar in each group is 2006-07
                for x in group:
                    year = 2007 - round((last_x - x) / 4.73)
                    rects = bars[x]
                    top = min(r.y0 for r in rects)
                    bottom = max(r.y1 for r in rects)
                    tip = bottom if abs(bottom-zero) > abs(top-zero) else top
                    value = (tip-zero) / slope
                    rows.append(dict(
                        airport=airport, year=year, fyear=f'{year-1}-{year}',
                        chart=spec['chart'], pdf_page=number,
                        measure=spec['measure'], adjustment=spec['adjustment'],
                        units=spec['units'], value=round(value, 5),
                        bar_x=x, bar_tip_pdf_y=round(tip, 3),
                        calibration_zero_y=round(zero, 3),
                        calibration_slope=round(slope, 8),
                        segments=len(rects)
                    ))
            counts = {a: len(g) for a, g in zip(AIRPORTS, groups)}
            print(f'Page {number}, Chart {spec["chart"]}: {sum(counts.values())} bars; '
                  f'counts={counts}; tick labels={[v for v, _ in ticks]}', file=sys.stderr)
    keys = [(r['pdf_page'], r['airport'], r['year']) for r in rows]
    if len(keys) != len(set(keys)):
        raise ValueError('Duplicate page-airport-year keys detected; inspect bar positions')
    validate_year_sequences(rows)
    return rows


def main():
    if len(sys.argv) != 3:
        sys.exit('Usage: python extract_comparative_charts.py REPORT.pdf OUTPUT.csv')
    rows = extract(sys.argv[1])
    output = Path(sys.argv[2])
    output.parent.mkdir(parents=True, exist_ok=True)
    with output.open('w', newline='', encoding='utf-8') as out:
        writer = csv.DictWriter(out, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
    print(f'Wrote {len(rows)} bars to {output}')

if __name__ == '__main__':
    main()
