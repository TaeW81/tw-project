import os
import json
import base64
import io
from PIL import Image, ExifTags
from PIL.ExifTags import TAGS, GPSTAGS
import tkinter as tk
from tkinter import filedialog, messagebox


def select_folder():
    """Show a folder selection dialog and return the chosen path."""
    root = tk.Tk()
    root.withdraw()
    return filedialog.askdirectory(title="사진 폴더를 선택하세요")


def show_complete_message(path: str) -> None:
    """Display a message indicating where the HTML was saved."""
    root = tk.Tk()
    root.withdraw()
    messagebox.showinfo("생성 완료", f'"{path}" 생성을 완료하였습니다.')
    root.destroy()


def get_exif_gps(image_path: str):
    """Extract GPS information from an image if available."""
    try:
        image = Image.open(image_path)
        exif_data = image._getexif()
        if not exif_data:
            return None
        exif = {TAGS.get(tag, tag): value for tag, value in exif_data.items()}
        gps_info = exif.get("GPSInfo")
        return {GPSTAGS.get(k, k): gps_info[k] for k in gps_info} if gps_info else None
    except Exception:
        return None


def get_photo_datetime(image_path: str) -> str:
    """Return the photo's taken time as a formatted string."""
    try:
        img = Image.open(image_path)
        exif_data = img._getexif()
        if exif_data and 36867 in exif_data:
            raw = exif_data[36867]
            return (
                raw.replace(":", "년", 1)
                .replace(":", "월", 1)
                .replace(" ", "일 ")
                .replace(":", "시", 1)
                .replace(":", "분")
                + "초"
            )
    except Exception:
        pass
    return "촬영일자 없음"


def dms_to_dd(dms, ref):
    """Convert degrees/minutes/seconds to decimal degrees."""
    try:
        degrees = float(dms[0].numerator) / dms[0].denominator
        minutes = float(dms[1].numerator) / dms[1].denominator
        seconds = float(dms[2].numerator) / dms[2].denominator
    except AttributeError:
        degrees = float(dms[0])
        minutes = float(dms[1])
        seconds = float(dms[2])
    dd = degrees + minutes / 60 + seconds / 3600
    if ref in ["S", "W"]:
        dd *= -1
    return dd


def auto_rotate_image(img: Image.Image) -> Image.Image:
    """Rotate an image according to its EXIF orientation tag."""
    try:
        exif = img._getexif()
        if exif:
            for orientation in ExifTags.TAGS:
                if ExifTags.TAGS[orientation] == "Orientation":
                    break
            value = exif.get(orientation)
            if value == 3:
                img = img.rotate(180, expand=True)
            elif value == 6:
                img = img.rotate(270, expand=True)
            elif value == 8:
                img = img.rotate(90, expand=True)
    except Exception:
        pass
    return img


def get_thumbnail_base64_and_size(image_path: str):
    """Return a base64 thumbnail for the image along with its size."""
    try:
        img = Image.open(image_path)
        img = auto_rotate_image(img)

        max_height = 900  # fit nicely on a 1080p display
        if img.height > max_height:
            scale = max_height / img.height
            img = img.resize((int(img.width * scale), max_height), Image.LANCZOS)

        width, height = img.size
        buffer = io.BytesIO()
        img.save(buffer, format="JPEG")
        thumb_b64 = base64.b64encode(buffer.getvalue()).decode()
        return thumb_b64, width, height
    except Exception:
        return None, None, None


def collect_image_files(folder: str):
    """Return a list of image file paths within the given folder."""
    images = []
    for root, _dirs, files in os.walk(folder):
        for file in files:
            if file.lower().endswith((".jpg", ".jpeg")):
                images.append(os.path.join(root, file))
    return images


def get_unique_path(directory: str, filename: str) -> str:
    """Return a file path that doesn't clash with existing files."""
    base, ext = os.path.splitext(filename)
    candidate = filename
    counter = 1
    while os.path.exists(os.path.join(directory, candidate)):
        candidate = f"{base}_{counter}{ext}"
        counter += 1
    return os.path.join(directory, candidate)


def main() -> None:
    folder_path = select_folder()
    if not folder_path:
        print("폴더를 선택하지 않았습니다.")
        return

    points = []
    for file_path in collect_image_files(folder_path):
        rel_path = os.path.relpath(file_path, folder_path)
        gps = get_exif_gps(file_path)
        if gps and "GPSLatitude" in gps and "GPSLongitude" in gps:
            try:
                lat = dms_to_dd(gps["GPSLatitude"], gps["GPSLatitudeRef"])
                lon = dms_to_dd(gps["GPSLongitude"], gps["GPSLongitudeRef"])
                thumb_b64, width, height = get_thumbnail_base64_and_size(file_path)
                taken_time = get_photo_datetime(file_path)
                points.append(
                    {
                        "path": rel_path,
                        "lat": lat,
                        "lon": lon,
                        "thumb_b64": thumb_b64,
                        "width": width,
                        "height": height,
                        "taken_time": taken_time,
                    }
                )
            except Exception as e:
                print(f"{rel_path} 오류: {e}")

    if not points:
        print("GPS 정보가 있는 사진이 없습니다.")
        return

    center_lat = sum(p["lat"] for p in points) / len(points)
    center_lon = sum(p["lon"] for p in points) / len(points)
    data_json = json.dumps(points, ensure_ascii=False)

    # Map tile sources
    esri = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
    vworld_base = "http://xdworld.vworld.kr:8080/2d/Base/202002/{z}/{x}/{y}.png"

    html = f"""
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
#photo-container {{
    width: 50%;
    overflow-y: auto;
    padding: 10px;
    box-sizing: border-box;
    display: flex;
    flex-direction: column;
    align-items: center;
}}
#photo-container img {{
    max-width: 100%;
    max-height: 85vh;
    object-fit: contain;
}}
h4, p {{
    margin: 4px 0;
    text-align: center;
}}
</style>
</head>
<body>
<div id=\"map\"></div>
<div id=\"photo-container\"><p>사진을 클릭하면 여기에 표시됩니다.</p></div>
<script src=\"https://unpkg.com/leaflet@1.9.3/dist/leaflet.js\"></script>
<script src=\"https://unpkg.com/leaflet.markercluster@1.5.3/dist/leaflet.markercluster.js\"></script>
<script>
var baseLayers = {{
    'Esri Satellite': L.tileLayer('{esri}', {{ attribution: 'Esri' }}),
    'VWorld Road': L.tileLayer('{vworld_base}', {{ attribution: 'VWorld' }})
}};
var map = L.map('map', {{
    center: [{center_lat}, {center_lon}],
    zoom: 16,
    layers: [baseLayers['Esri Satellite']]
}});
L.control.layers(baseLayers, null, {{ collapsed: false }}).addTo(map);
var markers = L.markerClusterGroup({{ maxClusterRadius: 40 }});
var data = {data_json};
data.forEach(function(p) {{
    var marker = L.marker([p.lat, p.lon], {{ title: p.path }});
    marker.on('click', function() {{
        var div = document.getElementById('photo-container');
        div.innerHTML = '<h4>' + p.path + '</h4><p>[' + p.taken_time + ']</p>' +
                        '<img src="data:image/jpeg;base64,' + p.thumb_b64 + '" />';
    }});
    markers.addLayer(marker);
}});
map.addLayer(markers);
</script>
</body>
</html>
"""

    folder_name = os.path.basename(os.path.normpath(folder_path))
    save_path = get_unique_path(folder_path, f"{folder_name}.html")
    with open(save_path, "w", encoding="utf-8") as f:
        f.write(html)

    show_complete_message(save_path)


if __name__ == "__main__":
    main()

