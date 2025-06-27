import os


def inject_html(html_path: str, overlay_code: str, output_path: str) -> str:
    """Insert *overlay_code* before ``</body>`` of *html_path* and write to *output_path*."""
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
    html_path = input('기존 HTML 파일 경로를 입력하세요: ').strip() or 'index.html'
    overlay_path = input('오버레이 HTML 파일 경로를 입력하세요: ').strip()

    with open(overlay_path, 'r', encoding='utf-8') as f:
        overlay_code = f.read()

    output_path = input('저장할 파일 경로를 입력하세요 (기본값: *_overlay.html): ').strip()
    if not output_path:
        base, ext = os.path.splitext(html_path)
        output_path = f'{base}_overlay{ext or ".html"}'

    inject_html(html_path, overlay_code, output_path)
    print(f'생성된 파일: {output_path}')


if __name__ == '__main__':
    main()
