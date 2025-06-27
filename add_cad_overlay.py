import os
import tkinter as tk
from tkinter import filedialog, simpledialog
try:
    from pyproj import Transformer
except ImportError as exc:  # pragma: no cover - simple user feedback
    raise SystemExit(
        "pyproj is required. Install it with `pip install pyproj` and rerun the script"
    ) from exc


def inject_html(html_path: str, overlay_code: str, output_path: str) -> str:
    """Insert *overlay_code* before ``</body>`` of *html_path* and write to
    ``output_path``.
    """
    with open(html_path, 'r', encoding='utf-8') as f:
        html = f.read()
    if '</body>' in html:
        new_html = html.replace('</body>', overlay_code + '\n</body>')
    else:
        new_html = html + '\n' + overlay_code
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(new_html)
    return output_path


def load_cad_points(path: str):
    """Read CAD coordinate pairs from *path* as a list of ``(x, y)`` tuples."""
    points = []
    with open(path, 'r', encoding='utf-8') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            try:
                x_str, y_str = line.split(',')
                points.append((float(x_str), float(y_str)))
            except ValueError:
                continue
    return points


def transform_points(points, from_epsg: int, to_epsg: int = 4326):
    """Transform ``points`` from ``from_epsg`` CRS to ``to_epsg`` using ``pyproj``."""
    transformer = Transformer.from_crs(from_epsg, to_epsg, always_xy=True)
    return [transformer.transform(x, y) for x, y in points]


def create_overlay_js(latlon_points, color: str = 'red') -> str:
    """Return a Leaflet polyline script for *latlon_points*."""
    coord_lines = ',\n        '.join(f'[{lat}, {lon}]' for lon, lat in latlon_points)
    return f"""
<script>
  var cadOverlay = L.polyline([
        {coord_lines}
    ], {{color: '{color}'}}).addTo(map);
</script>
"""


def main():
    root = tk.Tk()
    root.withdraw()

    html_path = filedialog.askopenfilename(
        title='기존 HTML 파일을 선택하세요',
        filetypes=[('HTML files', '*.html'), ('All files', '*.*')]
    )
    if not html_path:
        print('HTML 파일이 선택되지 않았습니다.')
        return

    cad_path = filedialog.askopenfilename(
        title='CAD 좌표 파일을 선택하세요',
        filetypes=[('CSV files', '*.csv'), ('All files', '*.*')]
    )
    if not cad_path:
        print('CAD 좌표 파일이 선택되지 않았습니다.')
        return

    options = [
        'grs80 서부', 'grs80 중부', 'grs80 동부', 'grs80 동해', 'grs80 utmk',
        'bessel 서부', 'bessel 중부', 'bessel 동부', 'bessel 동해', 'bessel 제주',
        'wgs84 wgs84', 'wgs84 googleTm',
    ]
    prompt = '좌표계를 선택하세요:\n' + '\n'.join(f'{i+1}. {o}' for i, o in enumerate(options))
    choice = simpledialog.askinteger('Coordinate System', prompt)
    if not choice or choice < 1 or choice > len(options):
        print('좌표계가 올바르게 선택되지 않았습니다.')
        return

    epsg_map = {
        'grs80 서부': 5186,
        'grs80 중부': 5187,
        'grs80 동부': 5188,
        'grs80 동해': 5189,
        'grs80 utmk': 5179,
        'bessel 서부': 2096,
        'bessel 중부': 2097,
        'bessel 동부': 2098,
        'bessel 동해': 2099,
        'bessel 제주': 2095,
        'wgs84 wgs84': 4326,
        'wgs84 googleTm': 3857,
    }

    from_epsg = epsg_map[options[choice - 1]]

    points = load_cad_points(cad_path)
    transformed = transform_points(points, from_epsg, 4326)
    overlay_code = create_overlay_js(transformed)

    base, ext = os.path.splitext(html_path)
    default_output = f'{base}_overlay{ext or ".html"}'
    output_path = filedialog.asksaveasfilename(
        title='저장 경로를 선택하세요',
        defaultextension=ext or '.html',
        initialfile=os.path.basename(default_output),
        filetypes=[('HTML files', '*.html'), ('All files', '*.*')],
    ) or default_output

    inject_html(html_path, overlay_code, output_path)
    print(f'생성된 파일: {output_path}')


if __name__ == '__main__':
    main()
