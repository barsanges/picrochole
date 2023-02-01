"Crée une image de carte à partir d'un fichier d'atlas pour Picrochole."

import argparse
import json
import cairo

CELL_HEIGHT = 8
NROWS = 60
HEIGHT = CELL_HEIGHT * NROWS

HALF_CELL_WIDTH = 4
CELL_WIDTH = 2 * HALF_CELL_WIDTH
NCOLS = 100
WIDTH = NCOLS * CELL_WIDTH + HALF_CELL_WIDTH

def load_atlas(fname: str) -> dict:
    "Lit un fichier d'atlas pour Picrochole."
    with open(fname, 'r') as fin:
        atlas = json.load(fin)
    total = NCOLS * NROWS
    if atlas["ncols"] != NCOLS or atlas ["nrows"] != NROWS\
       or len(atlas["content"]) != total:
        msg = f"the atlas should have {NCOLS} columns * {NROWS} rows "\
              f"= {total} cells"
        raise ValueError(msg)
    return atlas

def paint_cell(ctx: cairo.Context, data: dict, idx: int, jdx: int) -> None:
    """
    Peint dans le contexte `ctx` la cellule à la colonne `idx` et à la ligne
    `jdx` en fonction du contenu de `data`.
    """
    if data["topography"] == "water":
        ctx.set_source_rgb(224. / 255., 255. / 255., 255. / 255.) # LightCyan
    elif data["topography"] == "road":
        ctx.set_source_rgb(220. / 255., 220. / 255., 220. / 255.) # Gainsboro
    elif data["topography"] == "land":
        if data["capacity"] <= 3:
            ctx.set_source_rgb(139. / 255., 69. / 255., 19. / 255.) # SaddleBrown
        if 3 < data["capacity"] and data["capacity"] <= 6:
            ctx.set_source_rgb(210. / 255., 180. / 255., 140. / 255.) # Tan
        if 6 < data["capacity"] and data["capacity"] <= 9:
            ctx.set_source_rgb(245. / 255., 222. / 255., 179. / 255.) # Wheat
        if 9 < data["capacity"]:
            ctx.set_source_rgb(255. / 255., 255. / 255., 240. / 255.) # Ivory
    else:
        raise ValueError("unknown terrain: %s" % data["topography"])
    if jdx % 2 == 0:
        x = idx * CELL_WIDTH
    else:
        x = idx * CELL_WIDTH + HALF_CELL_WIDTH
    y = jdx * CELL_HEIGHT
    ctx.rectangle(x, y, CELL_WIDTH, CELL_HEIGHT)
    ctx.fill()

def paint(atlas: dict) -> cairo.ImageSurface:
    "Crée une image de carte à partir de l'atlas pour Picrochole `atlas`."
    surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, WIDTH, HEIGHT)
    ctx = cairo.Context(surface)
    for idx in range(NCOLS):
        for jdx in range(NROWS):
            value = atlas["content"][jdx * NCOLS + idx]
            paint_cell(ctx, value, idx, jdx)
    return surface

def main(fin: str, fout: str) -> None:
    """
    Crée une image de carte dans le fichier `fout` à partir du fichier d'atlas
    pour Picrochole `fin`.
    """
    atlas = load_atlas(fin)
    img = paint(atlas)
    if fout.lower().endswith(".png"):
        _fout = fout
    else:
        _fout = fout + ".png"
    img.write_to_png(_fout)

if __name__ == "__main__":
    DESCR = "Create the image of a map based on an atlas file for Picrochole"
    PARSER = argparse.ArgumentParser(description=DESCR)
    PARSER.add_argument("fin", help="the JSON atlas file to use as input")
    PARSER.add_argument("fout", help="the image file produced as output")
    ARGS = PARSER.parse_args()
    main(ARGS.fin, ARGS.fout)
