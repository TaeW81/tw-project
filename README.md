# CAD overlay utility

This repository contains a script `add_cad_overlay.py` that injects CAD coordinate overlays into an existing Leaflet-based HTML map.

## Requirements

- Python 3.8+
- `pyproj` for coordinate transformations
- Tkinter (usually comes with standard Python installs)

Install the required Python packages with:

```sh
pip install pyproj
```

## Usage

Run the script and follow the dialogs to select your HTML file, CAD coordinate file and coordinate system. The script writes a new HTML file with `_overlay` appended to the original filename unless you choose another location.
