import os
from PIL import Image, ExifTags
from PIL.ExifTags import TAGS, GPSTAGS
import json
import uuid
import sys
import base64
import io
import tkinter as tk
from tkinter import filedialog

AUTHORIZED_MACS = {
    # 80-E8-2C-EF-97-E0 belongs to 124507
    "80-E8-2C-EF-97-E0": "124507",
    # 6C-0B-5E-42-EC-0A belongs to 1258732
    "6C-0B-5E-42-EC-0A": "1258732",
}


def get_mac_address() -> str:
    mac = uuid.getnode()
    return "-".join(format(mac, "012X")[i:i+2] for i in range(0, 12, 2))


current_mac = get_mac_address().upper()
if current_mac not in AUTHORIZED_MACS:
    print("해당 장비에서만 실행 가능한 프로그램입니다.")
    sys.exit()


def select_folder():
    root = tk.Tk()
    root.withdraw()
    folder_selected = filedialog.askdirectory(title="사진 폴더를 선택하세요")
    return folder_selected


def get_exif_gps(image_path):
    try:
        image = Image.open(image_path)
        exif_data = image._getexif()
        if not exif_data:
            return None
        exif = {TAGS.get(tag, tag): value for tag, value in exif_data.items()}
        gps_info = exif.get('GPSInfo', None)
        if not gps_info:
            return None
        gps_data = {GPSTAGS.get(key, key): gps_info[key] for key in gps_info.keys()}
        return gps_data
    except Exception:
        return None


def dms_to_dd(dms, ref):
    try:
        degrees = float(dms[0].numerator) / float(dms[0].denominator)
        minutes = float(dms[1].numerator) / float(dms[1].denominator)
        seconds = float(dms[2].numerator) / float(dms[2].denominator)
    except AttributeError:
        degrees = float(dms[0])
        minutes = float(dms[1])
        seconds = float(dms[2])
    dd = degrees + minutes / 60 + seconds / 3600
    if ref in ['S', 'W']:
        dd *= -1
    return dd


def auto_rotate_image(img):
    try:
        exif = img._getexif()
        if exif is not None:
            for orientation in ExifTags.TAGS.keys():
                if ExifTags.TAGS[orientation] == 'Orientation':
                    break
            orientation_value = exif.get(orientation, None)
            if orientation_value == 3:
                img = img.rotate(180, expand=True)
            elif orientation_value == 6:
                img = img.rotate(270, expand=True)
            elif orientation_value == 8:
                img = img.rotate(90, expand=True)
    except Exception:
        pass
    return img


def get_thumbnail_base64_and_size(image_path):
    try:
        img = Image.open(image_path)
        img = auto_rotate_image(img)
        orig_width, orig_height = img.size
        if orig_height > 500:
            img.thumbnail((img.width * 10, 500), Image.LANCZOS)
        width, height = img.size
        buffer = io.BytesIO()
        img.save(buffer, format="JPEG")
        thumb_b64 = base64.b64encode(buffer.getvalue()).decode()
        return thumb_b64, width, height
    except Exception:
        return None, None, None


def collect_image_files(folder: str) -> list[str]:
    """Recursively gather JPEG files under the given folder."""
    image_files = []
    for root, _, files in os.walk(folder):
        for file in files:
            if file.lower().endswith((".jpg", ".jpeg")):
                image_files.append(os.path.join(root, file))
    return image_files


# 폴더 선택
folder_path = select_folder()
if not folder_path:
    print('폴더를 선택하지 않았습니다.')
    exit()

# 사진 파일 반복 처리 및 좌표 추출
points = []
for file_path in collect_image_files(folder_path):
    file = os.path.basename(file_path)
    gps_data = get_exif_gps(file_path)
    if gps_data and 'GPSLatitude' in gps_data and 'GPSLongitude' in gps_data:
        try:
            lat = dms_to_dd(gps_data['GPSLatitude'], gps_data['GPSLatitudeRef'])
            lon = dms_to_dd(gps_data['GPSLongitude'], gps_data['GPSLongitudeRef'])
            thumb_b64, width, height = get_thumbnail_base64_and_size(file_path)
            points.append({'file': file, 'lat': lat, 'lon': lon, 'thumb_b64': thumb_b64, 'width': width, 'height': height})
        except Exception as e:
            print(f"{file}의 GPS 정보 변환 중 오류 발생: {e}")

if not points:
    print('GPS 정보가 있는 사진이 없습니다.')
    exit()

# 지도와 사진을 나란히 보여줄 HTML 생성
center_lat = sum([p['lat'] for p in points]) / len(points)
center_lon = sum([p['lon'] for p in points]) / len(points)

esri_satellite_tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"

data_json = json.dumps(points)

html_content = f"""
<!DOCTYPE html>
<html lang=\"ko\">
<head>
    <meta charset=\"utf-8\">
    <title>Photo Map</title>
    <link rel=\"stylesheet\" href=\"https://unpkg.com/leaflet@1.9.3/dist/leaflet.css\" />
    <link rel=\"stylesheet\" href=\"https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.css\" />
    <link rel=\"stylesheet\" href=\"https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.Default.css\" />
    <style>
        body {{ margin: 0; display: flex; height: 100vh; }}
        #map {{ width: 50%; }}
        #photo-container {{ width: 50%; overflow-y: auto; padding: 10px; box-sizing: border-box; }}
        #photo-container img {{ max-width: 100%; height: auto; }}
    </style>
</head>
<body>
    <div id=\"map\"></div>
    <div id=\"photo-container\"><p>사진을 클릭하면 여기에 표시됩니다.</p></div>

    <script src=\"https://unpkg.com/leaflet@1.9.3/dist/leaflet.js\"></script>
    <script src=\"https://unpkg.com/leaflet.markercluster@1.5.3/dist/leaflet.markercluster.js\"></script>
    <script>
        var map = L.map('map').setView([{center_lat}, {center_lon}], 16);
        L.tileLayer('{esri_satellite_tiles}', {{ attribution: 'Esri' }}).addTo(map);
        var markers = L.markerClusterGroup({{ maxClusterRadius: 40, spiderfyOnMaxZoom: true }});
        var data = {data_json};
        data.forEach(function(p) {{
            var marker = L.marker([p.lat, p.lon], {{ title: p.file }});
            marker.on('click', function() {{
                var div = document.getElementById('photo-container');
                var img = '<img src="data:image/jpeg;base64,' + p.thumb_b64 + '" />';
                div.innerHTML = '<h4>' + p.file + '</h4>' + img;
            }});
            markers.addLayer(marker);
        }});
        map.addLayer(markers);
    </script>
</body>
</html>
"""

# 저장 경로
def get_unique_path(directory: str, filename: str) -> str:
    """Return a unique file path in directory for filename."""
    base, ext = os.path.splitext(filename)
    candidate = filename
    counter = 1
    while os.path.exists(os.path.join(directory, candidate)):
        candidate = f"{base}_{counter}{ext}"
        counter += 1
    return os.path.join(directory, candidate)


save_path = get_unique_path(folder_path, "photo_map.html")
with open(save_path, "w", encoding="utf-8") as f:
    f.write(html_content)

print(f'{os.path.basename(save_path)} 파일이 "{folder_path}" 폴더에 생성되었습니다!')
