"Lance une interface web pour Picrochole."

import argparse
import os
import os.path as osp
import base64
import json
import plotly.graph_objects as go
import dash
import dash_core_components as dcc
import dash_html_components as html
from collections import namedtuple
from dash.dependencies import Input, Output

CELL_HEIGHT = 8
NROWS = 60
HEIGHT = CELL_HEIGHT * NROWS

HALF_CELL_WIDTH = 4
CELL_WIDTH = 2 * HALF_CELL_WIDTH
NCOLS = 100
WIDTH = NCOLS * CELL_WIDTH + HALF_CELL_WIDTH

CellInfo = namedtuple("CellInfo", ("sent", "marker", "blue", "red"))

class Data:
    "Ensemble des données d'une partie de Picrochole."

    def __init__(self, game_dir: str):
        "Initialise une nouvelle instance de la classe `Data`."
        self._dir = game_dir
        self.img = b64_image(osp.join(game_dir, "map.png"))
        self.data = load_game_dir(game_dir)
        if self.data["config"]["ia-faction"] == "red":
            self.faction = "blue"
            self.player_hq = self.data["config"]["hq-blue"]
        else:
            self.faction = "red"
            self.player_hq = self.data["config"]["hq-red"]
        self.infos = mk_infos(self.player_hq, self.data["current turn"],
                              self.data["reports"])

    def display_map(self, turn: int):
        "Affiche la carte vue du tour `turn`."
        fig = display_base_map(self.img)
        display_infos(fig, self.infos, turn)
        return fig

class Infos:
    "Ensemble organisé des rapports reçus par un QG."

    def __init__(self, current_turn: int):
        "Initialise une nouvelle instance de la classe reports."
        self._current_turn = current_turn
        self._data = [{} for _ in range(NCOLS * NROWS)]

    def add_cell_info(self, sent: int, ckey: int, cell_info: list) -> None:
        "Ajoute un rapport sur la case `ckey`, envoyé au tour `turn`."
        if cell_info == "blue" or cell_info == "red":
            value = CellInfo(sent, cell_info, [], [])
        else:
            units = sorted(cell_info, key=lambda x: x["strength"])
            blue = list(filter(lambda x: x["faction"] == "blue", units))
            red = list(filter(lambda x: x["faction"] == "red", units))
            value = CellInfo(sent, None, blue, red)
        self._data[ckey][sent] = value

    def get_cell_info(self, turn: int, ckey: int) -> CellInfo:
        "Renvoie les informations 'live' sur la case `ckey` à la date `turn`."
        reports = self._data[ckey]
        all_dates = [x for x in reports.keys() if x <= turn]
        if len(all_dates) > 0:
            date = max(all_dates)
            return reports[date]
        else:
            return None

def kind_as_fr(kind: str) -> str:
    "'Traduit' en français un type d'unité (une arme)."
    if kind == "infantry":
        return "Infanterie"
    elif kind == "cavalry":
        return "Cavalerie"
    elif kind == "artillery":
        return "Artillerie"
    else:
        raise ValueError("unknown kind of unit: '%s'" % kind)

def to_coords(ckey: str) -> (float, float):
    "Convertit un indice de case en une paire de coordonnées."
    _ckey = int(ckey)
    idx = _ckey % NCOLS
    jdx = _ckey // NCOLS
    if jdx % 2 == 0:
        x = (idx + 0.5) * CELL_WIDTH
    else:
        x = (idx + 1.0) * CELL_WIDTH
    y = HEIGHT - (jdx + 0.5) * CELL_HEIGHT
    return (x, y)

def b64_image(fname):
    """
    Charge une image de manière à l'afficher dans Dash.

    Voir https://dash.plotly.com/external-resources.
    """
    with open(fname, 'rb') as f:
        image = f.read()
    return 'data:image/png;base64,' + base64.b64encode(image).decode('utf-8')

def load_game_dir(dirname: str) -> dict:
    "Lit les données de la partie en cours."
    res = {}
    with open(osp.join(dirname, "atlas.json"), 'r') as fin:
        atlas = json.load(fin)
        if atlas["nrows"] != NROWS or atlas["ncols"] != NCOLS:
            msg = "the map should have exactly %d rows and %d columns, "\
                  "got %d rows x %d columns instead"\
                  % (NROWS, NCOLS, atlas["nrows"], atlas["ncols"])
            raise ValueError(msg)
    with open(osp.join(dirname, "config.json"), 'r') as fin:
        res["config"] = json.load(fin)
    with open(osp.join(dirname, "current-turn.json"), 'r') as fin:
        res["current turn"] = json.load(fin)
    with open(osp.join(dirname, "reports.json"), 'r') as fin:
        res["reports"] = json.load(fin)
    with open(osp.join(dirname, "orders.json"), 'r') as fin:
        res["orders"] = json.load(fin)
    return res

def mk_infos(player_hq: str, current_turn: int, reports: list) -> Infos:
    """
    Organise dans une instance de `Infos` les rapports `reports` reçus jusqu'au
    tour `current_turn` par le QG `player_hq`.
    """
    infos = Infos(current_turn)
    _ok = lambda x: (x["to"] == player_hq) and (x["received"] <= current_turn)
    for report in filter(_ok, reports):
        sent = report["sent"]
        for ckey, content in report["content"].items():
            infos.add_cell_info(sent, int(ckey), content)
    return infos

def select_latest_units_info(faction: str, player_hq: str, current_turn: int,
                             reports: list) -> dict:
    """
    Sélectionne, pour chaque unité de la faction `faction`, les dernières
    informations connues du QG `player_hq`.
    """
    res = {}
    hq_ok = lambda x: x["to"] == player_hq
    faction_ok = lambda x: x["faction"] == faction
    for report in filter(hq_ok, reports):
        for location, values in report["content"].items():
            if values != [] and values != "red" and values != "blue":
                for unit in filter(faction_ok, values):
                    key = unit["unit-key"]
                    if (report["received"] <= current_turn)\
                       and ((key in res
                            and res[key]["received"] < report["received"])
                            or key not in res):
                        tmp = {"location": location,
                               "received": report["received"],
                               **unit}
                        tmp["kind"] = kind_as_fr(tmp["kind"])
                        res[key] = tmp
    return res

def select_latest_orders(player_hq: str, orders: list) -> dict:
    """
    Sélectionne, pour chaque unité sous son contrôle, les derniers ordres
    envoyés par le QG `player_hq`.
    """
    res = {}
    hq_ok = lambda x: x["from"] == player_hq
    for order in filter(hq_ok, orders):
        key = order["to"]
        if ((key in res and res[key]["sent"] < order["sent"])
            or key not in res):
            res[key] = {"sent": order["sent"], "content": order["content"]}
    return res

def mk_units_table(faction: str, player_hq: str, current_turn: int,
                   reports: dict, orders: dict) -> html.Table:
    """
    Crée un tableau affichant les dernières informations connues sur les
    unités du joueur.
    """
    header = [html.Th("Unité"), html.Th("Arme"), html.Th("Effectif"),
              html.Th("Emplacement"), html.Th("Vue le"),
              html.Th("Destination"), html.Th("Envoyée le")]
    body = []
    info = select_latest_units_info(faction, player_hq, current_turn, reports)
    orders = select_latest_orders(player_hq, orders)
    for key in sorted(info.keys()):
        unit = info[key]
        if key in orders:
            last_order = orders[key]["content"]
            date_order = orders[key]["sent"]
        else:
            last_order = None
            date_order = None
        row = html.Tr([html.Td(key),
                       html.Td(unit["kind"]),
                       html.Td(unit["strength"]),
                       html.Td(unit["location"]),
                       html.Td(unit["received"]),
                       html.Td(last_order),
                       html.Td(date_order)])
        body.append(row)
    table = html.Table([html.Thead(html.Tr(header)),
                        html.Tbody(body)])
    return table

def display_base_map(img) -> go.Figure:
    "Crée une figure Plotly contenant le fond de carte `img`."
    fig = go.Figure()
    scale_factor = 1.0
    fig.update_xaxes(range=[0, WIDTH * scale_factor],
                     visible=False)
    fig.update_yaxes(range=[0, HEIGHT * scale_factor],
                     # Ensures that the aspect ratio stays constant:
                     scaleanchor="x",
                     visible=False)
    fig.add_layout_image(source=img,
                         x=0,
                         sizex=WIDTH * scale_factor,
                         y=HEIGHT * scale_factor,
                         sizey=HEIGHT * scale_factor,
                         xref="x",
                         yref="y",
                         opacity=1.0,
                         layer="below",
                         sizing="stretch")
    # Configure other layout
    fig.update_layout(width=WIDTH * scale_factor,
                      height=HEIGHT * scale_factor,
                      margin={"l": 0, "r": 0, "t": 0, "b": 0},
                      showlegend=False)
    return fig

def cell_info_status(current_turn: int, cell_info: CellInfo) -> str:
    "Indique la nature des informations sur une case."
    if cell_info is None:
        return "unknown"
    elif cell_info.marker is not None:
        base = "marker-" + cell_info.marker
    elif len(cell_info.blue) > 0 and len(cell_info.red) == 0:
        base = "blue"
    elif len(cell_info.blue) == 0 and len(cell_info.red) > 0:
        base = "red"
    elif len(cell_info.blue) > 0 and len(cell_info.red) > 0:
        base = "battle"
    else:
        base = "empty"
    if cell_info.sent == current_turn:
        return (base + "-now")
    else:
        return (base + "-past")

def cell_info_to_txt(ckey: int, cell_info: CellInfo) -> str:
    """
    Convertit les informations sur une case en une chaîne de caractères
    descriptive.
    """
    if cell_info is None:
        return f"Case n°{ckey}"
    txt = f"Case n°{ckey}<br>Date : {cell_info.sent}<br>"
    if cell_info.marker is not None:
        if cell_info.marker == "blue":
            color = "bleue"
        else:
            color = "rouge"
        txt += "Ligne de communication " + color
        return txt
    if len(cell_info.blue) > 0:
        tags = ["%s, %s: %d" % (x["unit-key"], kind_as_fr(x["kind"]),
                                x["strength"]) for x in cell_info.blue]
        txt += '<b>Bleu :</b><br>%s' % '<br>'.join(tags)
    if len(cell_info.red) > 0:
        tags = ["%s, %s: %d" % (x["unit-key"], kind_as_fr(x["kind"]),
                                x["strength"]) for x in cell_info.red]
        txt += '<b>Rouge :</b><br>%s' % '<br>'.join(tags)
    return txt

def display_infos(fig: go.Figure, infos: Infos, current_turn: int) -> None:
    "Affiche sur la carte les informations reçues par le QG du joueur."
    small = 3
    big = 10
    cfg = {"blue-now": {"size": big, "color": "blue", "opacity": 1.0},
           "red-now": {"size": big, "color": "red", "opacity": 1.0},
           "marker-blue-now": {"size": small, "color": "blue", "opacity": 1.0},
           "marker-red-now": {"size": small, "color": "red", "opacity": 1.0},
           "empty-now": {"size": small, "color": "LightGray", "opacity": 1.0},
           "battle-now": {"size": big, "color": "purple", "opacity": 1.0},
           "blue-past": {"size": big, "color": "blue", "opacity": 0.5},
           "red-past": {"size": big, "color": "red", "opacity": 0.5},
           "marker-blue-past": {"size": small, "color": "blue", "opacity": 0.5},
           "marker-red-past": {"size": small, "color": "red", "opacity": 0.5},
           "empty-past": {"size": small, "color": "LightGray", "opacity": 0.5},
           "battle-past": {"size": big, "color": "purple", "opacity": 0.5},
           "unknown": {"size": small, "color": "LightGray", "opacity": 0.5}}
    xs = {key: [] for key in cfg.keys()}
    ys = {key: [] for key in cfg.keys()}
    txts = {key: [] for key in cfg.keys()}
    for ckey in range(NROWS * NCOLS):
        cell_info = infos.get_cell_info(current_turn, ckey)
        status = cell_info_status(current_turn, cell_info)
        txt = cell_info_to_txt(ckey, cell_info)
        x, y = to_coords(ckey)
        txts[status].append(txt)
        xs[status].append(x)
        ys[status].append(y)
    for key in cfg.keys():
        fig.add_trace(go.Scatter(x=xs[key],
                                 y=ys[key],
                                 mode="markers",
                                 marker=cfg[key],
                                 text=txts[key],
                                 hovertemplate="%{text}"))

def build_app(dirname: str) -> dash.Dash:
    "Renvoie un objet `Dash` correspondant à l'interface de Picrochole."
    return app

if __name__ == "__main__":
    DESCR = "Launch a GUI for Picrochole, a game simulating pre-napoleonic "\
            "and napoleonic warfare"
    PARSER = argparse.ArgumentParser(description=DESCR)
    PARSER.add_argument('game_dir', help="Directory containing the current "\
                                         "game")
    ARGS = PARSER.parse_args()
    DATA = Data(ARGS.game_dir)
    units_table = mk_units_table(DATA.faction,
                                 DATA.player_hq,
                                 DATA.data["current turn"],
                                 DATA.data["reports"],
                                 DATA.data["orders"])
    APP = dash.Dash(title="Picrochole")
    APP.layout = html.Div([
        dcc.Markdown(children="**Partie :** %s" % ARGS.game_dir),
        dcc.Markdown(children="**Tour :** %d" % DATA.data["current turn"]),
        # Disable the autosize on double click because it adds unwanted margins
        # around the image (https://plotly.com/python/configuration-options/).
        dcc.Graph(id="map", config={"doubleClick": "reset",
                                    "displayModeBar": False}),
        dcc.Slider(min=0, max=DATA.data["current turn"], step=1,
                   value=DATA.data["current turn"], id="slider-turn",
                   tooltip={"placement": "bottom", "always_visible": True}),
        units_table
    ])
    APP.callback(Output("map", "figure"),
                 Input("slider-turn", "value"))(DATA.display_map)
    APP.run_server(debug=True)
