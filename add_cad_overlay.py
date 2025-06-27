import os
import tkinter as tk
from tkinter import filedialog


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

    overlay_path = filedialog.askopenfilename(
        title='오버레이 HTML 파일을 선택하세요',
        filetypes=[('HTML files', '*.html'), ('All files', '*.*')]
    )
    if not overlay_path:
        print('오버레이 파일이 선택되지 않았습니다.')
        return

    with open(overlay_path, 'r', encoding='utf-8') as f:
        overlay_code = f.read()

    base, ext = os.path.splitext(html_path)
    output_path = f'{base}_반영{ext or ".html"}'

    inject_html(html_path, overlay_code, output_path)
    print(f'생성된 파일: {output_path}')


if __name__ == '__main__':
    main()
