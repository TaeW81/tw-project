import os
import re
import math
from tkinter import Tk, filedialog, simpledialog
import ezdxf
from ezdxf.math import Vec2


def parse_inp_file(inp_path):
    """EPANET INP \ud30c\uc77c\uc744 \ud30c\uc2f1\ud558\uc5ec \uc694\uc18c\uc640 VERTICES \ubc18\ud658"""
    junctions, reservoirs, tanks, pipes, pumps, valves, coordinates, vertices = {}, {}, {}, {}, {}, {}, {}, {}
    section = None
    with open(inp_path, 'r', encoding='utf-8', errors='ignore') as f:
        for raw in f:
            line = raw.strip()
            if ';' in line:
                line = line.split(';', 1)[0].strip()
            if not line:
                continue
            m = re.match(r'^\[(\w+)\]$', line)
            if m:
                section = m.group(1).lower()
                continue
            parts = re.split(r'\s+', line)
            if section == 'junctions' and len(parts) >= 3:
                junctions[parts[0]] = {
                    "ID": parts[0],
                    "Elevation": float(parts[1]),
                    "Demand":    float(parts[2])
                }
            elif section == 'reservoirs' and len(parts) >= 2:
                reservoirs[parts[0]] = {
                    "ID": parts[0],
                    "Head": float(parts[1])
                }
            elif section == 'tanks' and len(parts) >= 6:
                tanks[parts[0]] = {
                    "ID": parts[0],
                    "Elevation": float(parts[1]),
                    "InitLevel": float(parts[2]),
                    "MinLevel":  float(parts[3]),
                    "MaxLevel":  float(parts[4]),
                    "Diameter":  float(parts[5])
                }
            elif section == 'pipes' and len(parts) >= 5:
                pipes[parts[0]] = {
                    "ID": parts[0],
                    "Start": parts[1],
                    "End":   parts[2],
                    "Length":   float(parts[3]),
                    "Diameter": float(parts[4])
                }
            elif section == 'pumps' and len(parts) >= 3:
                pumps[parts[0]] = {
                    "ID": parts[0],
                    "Start": parts[1],
                    "End":   parts[2]
                }
            elif section == 'valves' and len(parts) >= 4:
                valves[parts[0]] = {
                    "ID": parts[0],
                    "Start": parts[1],
                    "End":   parts[2],
                    "Diameter": float(parts[3])
                }
            elif section == 'coordinates' and len(parts) >= 3:
                coordinates[parts[0]] = {
                    "X": float(parts[1]),
                    "Y": float(parts[2])
                }
            elif section == 'vertices' and len(parts) >= 3:
                pid = parts[0]
                x, y = float(parts[1]), float(parts[2])
                vertices.setdefault(pid, []).append((x, y))
    for ID, coord in coordinates.items():
        for coll in (junctions, reservoirs, tanks, pumps, valves):
            if ID in coll:
                coll[ID].update(coord)
    return junctions, reservoirs, tanks, pipes, pumps, valves, vertices


def get_polyline_midpoint_angle(pts):
    """Polyline \uae38\uc774 \uae30\ubc18 \uc911\uac04 \uc810 \uad6c\ud558\uace0 \uac19\uc740 \ub3d9\ud589 \uc0c9\uacc4 \ubc18\ud658"""
    if len(pts) < 2:
        return (pts[0][0], pts[0][1], 0) if pts else (0, 0, 0)
    lengths = [math.hypot(pts[i+1][0]-pts[i][0], pts[i+1][1]-pts[i][1]) for i in range(len(pts)-1)]
    total = sum(lengths)
    half = total / 2.0
    accum = 0.0
    for i, seglen in enumerate(lengths):
        if accum + seglen >= half:
            ratio = (half - accum) / seglen
            x1, y1 = pts[i]
            x2, y2 = pts[i+1]
            x = x1 + (x2-x1)*ratio
            y = y1 + (y2-y1)*ratio
            angle = math.degrees(math.atan2(y2-y1, x2-x1)) % 360
            return x, y, angle
        accum += seglen
    xs, ys = zip(*pts)
    angle = math.degrees(math.atan2(pts[-1][1]-pts[0][1], pts[-1][0]-pts[0][0])) % 360
    return sum(xs)/len(xs), sum(ys)/len(ys), angle


def get_safe_output_path(base_path):
    """\ub3d9\uc77c \uc774\ub984 \ud30c\uc77c \uc874\uc7ac \uc2dc \uc22b\uc790 \ubd99\uc5ec \uc0c8\ub85c\uc6b4 \uacbd\ub85c \ubc18\ud658"""
    if not os.path.exists(base_path):
        return base_path
    base, ext = os.path.splitext(base_path)
    i = 1
    while True:
        new = f"{base}_{i}{ext}"
        if not os.path.exists(new):
            return new
        i += 1


def create_dxf(junctions, reservoirs, tanks, pipes, pumps, valves, vertices, out_path, text_height=0.5):
    """DXF \uc0dd\uc131: VERTICES \uae30\ubc18 Polyline, \ube14\ub85d \uc18d\uc131, \ub808\uc774\uc5b4 \uc815\ub9ac"""
    doc = ezdxf.new(dxfversion='R2018')
    doc.layers.new('EPANET2-PIPE',    dxfattribs={'color': 3})
    doc.layers.new('EPANET2-JUNCTION')
    doc.layers.new('EPANET2-PIPE_no')
    msp = doc.modelspace()

    def define_block(name, draw_func, attribs, center=False):
        blk = doc.blocks.new(name=name)
        draw_func(blk)
        for tag, value in attribs.items():
            if isinstance(value, tuple):
                pos = value[:2]
                hgt = value[2] if len(value) >= 3 else text_height
                width = value[3] if len(value) >= 4 else 0.8
                color = value[4] if len(value) >= 5 else None
            else:
                pos = value
                hgt = text_height
                width = 0.8
                color = None
            att = blk.add_attdef(tag=tag, insert=Vec2(*pos), height=hgt)
            att.dxf.width = width
            if color is not None:
                att.dxf.color = color
            if center:
                if hasattr(att, "set_pos"):
                    att.set_pos(Vec2(*pos), align="MIDDLE_CENTER")
                else:
                    # fallback for older ezdxf versions
                    if hasattr(att.dxf, "halign"):
                        att.dxf.halign = 1  # center
                    if hasattr(att.dxf, "valign"):
                        att.dxf.valign = 2  # middle
                    if hasattr(att.dxf, "align_point"):
                        att.dxf.align_point = Vec2(*pos)

    def draw_junction(b):
        b.add_circle((0, 0), radius=3, dxfattribs={"color": 5})
        hatch = b.add_hatch(color=255, dxfattribs={"pattern_name": "SOLID"})
        path = hatch.paths.add_edge_path()
        path.add_arc((0, 0), radius=3, start_angle=0, end_angle=360)

    define_block(
        "JUNCTION_BLOCK",
        draw_junction,
        {"ID": (0, 0, 3, 0.7, 5)},
        center=True,
    )
    offset = text_height * 0.75
    define_block(
        "PIPE_BLOCK",
        lambda b: None,
        {
            "ID": (0, offset),
            "INFO": (0, -offset),
        },
        center=True,
    )
    define_block("RESERVOIR_BLOCK",
                 lambda b: b.add_circle((0,0), radius=2),
                 {"ID": (2,0), "HEAD": (2,-0.5)})
    define_block("TANK_BLOCK",
                 lambda b: b.add_lwpolyline([(-1,-1),(1,-1),(1,1),(-1,1),(-1,-1)]),
                 {"ID": (2,0)})
    define_block("PUMP_BLOCK",
                 lambda b: b.add_circle((0,0), radius=1),
                 {"ID": (1.5,0.5)})
    define_block("VALVE_BLOCK",
                 lambda b: b.add_lwpolyline([(0,0),(1,1),(2,0)]),
                 {"ID": (1.5,0.5), "DIAM": (1.5,-0.5)})

    for p in pipes.values():
        start = junctions.get(p["Start"]) or reservoirs.get(p["Start"]) or tanks.get(p["Start"])
        end   = junctions.get(p["End"])   or reservoirs.get(p["End"])   or tanks.get(p["End"])
        if not (start and end and "X" in start and "Y" in start and "X" in end and "Y" in end):
            continue
        pts = [(start["X"],start["Y"])] + vertices.get(p["ID"],[]) + [(end["X"],end["Y"])]
        msp.add_lwpolyline(pts, dxfattribs={"layer":"EPANET2-PIPE", "closed": False})
        cx, cy, ang = get_polyline_midpoint_angle(pts)
        info = f"D{int(p['Diameter'])}  L={p['Length']:.2f}"
        ref = msp.add_blockref("PIPE_BLOCK", (cx, cy), dxfattribs={"rotation": ang})
        ref.dxf.layer = "EPANET2-PIPE_no"
        ref.add_auto_attribs({"ID": p["ID"], "INFO": info})

    for j in junctions.values():
        if "X" in j and "Y" in j:
            ref = msp.add_blockref("JUNCTION_BLOCK", (j["X"], j["Y"]))
            ref.dxf.layer = "EPANET2-JUNCTION"
            ref.add_auto_attribs({"ID": j["ID"]})

    for coll, blk_name, tagmap in [
        (reservoirs, "RESERVOIR_BLOCK", {"ID":"ID","HEAD":"Head"}),
        (tanks,      "TANK_BLOCK",      {"ID":"ID"}),
        (pumps,      "PUMP_BLOCK",      {"ID":"ID"}),
        (valves,     "VALVE_BLOCK",     {"ID":"ID","DIAM":"Diameter"}),
    ]:
        for e in coll.values():
            if "X" in e and "Y" in e:
                ref = msp.add_blockref(blk_name, (e["X"], e["Y"]))
                ref.add_auto_attribs({tag: str(e[field]) for tag, field in tagmap.items()})

    safe_path = get_safe_output_path(out_path)
    doc.saveas(safe_path)
    print(f"\u2705 DXF saved: {safe_path}")


if __name__ == "__main__":
    root = Tk()
    root.withdraw()
    inp = filedialog.askopenfilename(title="EPANET INP 파일 선택", filetypes=[("INP", "*.inp")])
    if inp:
        base_out = os.path.splitext(inp)[0] + "_with_vertices.dxf"
        junc, res, tank, pipe, pump, valve, verts = parse_inp_file(inp)
        th = simpledialog.askfloat("Text Height", "링크 텍스트 높이(기본 0.5):", initialvalue=0.5)
        if th is None:
            th = 0.5
        create_dxf(junc, res, tank, pipe, pump, valve, verts, base_out, text_height=th)
    else:
        print("❗ INP 파일이 선택되지 않았습니다.")
