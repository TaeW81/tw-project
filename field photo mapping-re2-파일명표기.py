import os
from PIL import Image, ExifTags
from PIL.ExifTags import TAGS, GPSTAGS
import folium
from folium.plugins import MarkerCluster  # 클러스터링 기능 추가
import base64
import io
import tkinter as tk
from tkinter import filedialog
from folium import IFrame

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

# 폴더 선택
folder_path = select_folder()
if not folder_path:
    print('폴더를 선택하지 않았습니다.')
    exit()

# 사진 파일 반복 처리 및 좌표 추출
points = []
for root, dirs, files in os.walk(folder_path):
    for file in files:
        if file.lower().endswith(('.jpg', '.jpeg')):
            file_path = os.path.join(root, file)
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

# Folium 지도 생성 (ESRI Satellite 타일 사용)
center_lat = sum([p['lat'] for p in points]) / len(points)
center_lon = sum([p['lon'] for p in points]) / len(points)
m = folium.Map(location=[center_lat, center_lon], zoom_start=16, tiles=None)

# ESRI Satellite 타일 추가
esri_satellite_tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
folium.TileLayer(
    tiles=esri_satellite_tiles,
    attr='Esri',
    name='Esri Satellite',
    overlay=False,
    control=True
).add_to(m)

# 마커 클러스터 생성
marker_cluster = MarkerCluster(
    name="사진 마커",
    overlay=True,
    control=True,
    options={
        'maxClusterRadius': 40,  # 클러스터링 민감도 조정 (값이 작을수록 더 세분화)
        'spiderfyOnMaxZoom': True,  # 최대 줌 시 마커 분산
    }
).add_to(m)

# 마커 추가 (클러스터에 추가)
for p in points:
    if p['thumb_b64'] and p['width'] and p['height']:
        html = f'<img src="data:image/jpeg;base64,{p["thumb_b64"]}" width="{p["width"]}" height="{p["height"]}">'
        iframe = IFrame(html, width=p["width"]+20, height=p["height"]+20)
        popup = folium.Popup(iframe, max_width=p["width"]+40)
    else:
        html = f'<b>(썸네일 없음)</b>'
        iframe = IFrame(html, width=200, height=40)
        popup = folium.Popup(iframe, max_width=220)
    
    folium.Marker(
        [p['lat'], p['lon']],
        popup=popup,
        tooltip=p["file"]
    ).add_to(marker_cluster)  # 마커를 클러스터에 추가

# 저장 경로
save_path = os.path.join(folder_path, "photo_map.html")
m.save(save_path)
print(f'photo_map.html 파일이 "{folder_path}" 폴더에 생성되었습니다!')
