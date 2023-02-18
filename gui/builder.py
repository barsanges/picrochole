"Interface web pour Picrochole."

import os
import os.path as osp
import base64
import json
import plotly.graph_objects as go
import dash
import dash_core_components as dcc
import dash_html_components as html

CELL_HEIGHT = 8
NROWS = 60
HEIGHT = CELL_HEIGHT * NROWS

HALF_CELL_WIDTH = 4
CELL_WIDTH = 2 * HALF_CELL_WIDTH
NCOLS = 100
WIDTH = NCOLS * CELL_WIDTH + HALF_CELL_WIDTH

def kind_as_fr(kind: str) -> str:
    """
    'Traduit' en français un type d'unité (une arme).

    Paramètres
    ----------

    kind : str
        Chaîne de caractères décrivant un type d'unité.
    """
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
    """
    Lit les données de la partie en cours.

    Paramètres
    ----------

    dirname : str
        Chemin vers le dossier contenant la partie en cours.
    """
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

def organize_reports(player_hq: str, current_turn: int, reports: list) -> dict:
    """
    Organise par date et par case de la carte le contenu des rapports reçus par
    le QG `player_hq`.

    Paramètres
    ----------

    player_hq: str
        Identifiant du QG du joueur.

    current_turn: int
        Tour en cours.

    reports : dict
        Rapports envoyés dans la partie en cours.
    """
    res = {}
    _ok = lambda x: (x["to"] == player_hq) and (x["sent"] <= current_turn)
    for report in filter(_ok, reports):
        date = report["sent"]
        if date not in res:
            res[date] = {}
        for ckey, content in report["content"].items():
            res[date][ckey] = content
    return res

def select_latest_units_info(faction: str, player_hq: str, current_turn: int,
                             reports: list) -> dict:
    """
    Sélectionne, pour chaque unité de la faction `faction`, les dernières
    informations connues du QG `player_hq`.

    Paramètres
    ----------

    faction: str
        Identifiant de la faction du joueur.

    player_hq: str
        Identifiant du QG du joueur.

    current_turn: int
        Tour en cours.

    reports : dict
        Rapports envoyés dans la partie en cours.
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

    Paramètres
    ----------

    player_hq: str
        Identifiant du QG du joueur.

    orders : dict
        Ordres envoyés dans la partie en cours.
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

    Paramètres
    ----------

    faction: str
        Identifiant de la faction du joueur.

    player_hq: str
        Identifiant du QG du joueur.

    current_turn: int
        Tour en cours.

    reports : dict
        Rapports envoyés dans la partie en cours.

    orders : dict
        Ordres envoyés dans la partie en cours.
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
    """
    Crée une figure Plotly contenant le fond de carte.

    Paramètre
    ---------

    img:
        Image à utiliser comme fond de carte.
    """
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

def report_to_txt(content: list) -> (str, str):
    "Convertit un rapport sur une case en une chaîne de caractères descriptive."
    blue = []
    red = []
    _content = sorted(content, key=lambda x: x["strength"])
    for x in _content:
        tag = "%s, %s: %d" % (x["unit-key"], kind_as_fr(x["kind"]),
                              x["strength"])
        if x["faction"] == "blue":
            blue.append(tag)
        else:
            red.append(tag)
    blue_txt = '<b>Bleu :</b><br>%s' % '<br>'.join(blue)
    red_txt = '<b>Rouge :</b><br>%s' % '<br>'.join(red)
    if blue != [] and red == []:
        status = "blue"
        txt = blue_txt
    elif blue == [] and red != []:
        status = "red"
        txt = red_txt
    elif blue != [] and red != []:
        status = "battle"
        txt = blue_txt + "\n" + red_txt
    else:
        status = "empty"
        txt = ""
    return (status, txt)

def display_infos(fig: go.Figure, infos: dict, current_turn: int) -> None:
    """
    Affiche sur la carte les informations reçues par le QG du joueur.

    Paramètres
    ----------

    fig: go.Figure
        Représentation de la carte sur laquelle afficher les informations.

    infos: dict
        Rapports reçus par le joueur, organisés par date et par case de la
        carte.

    current_turn: int
        Tour en cours.
    """
    if current_turn not in infos:
        return None
    data = infos[current_turn]
    xs = {"blue": [], "red": [], "marker-blue": [], "marker-red": [],
          "battle": []}
    ys = {"blue": [], "red": [], "marker-blue": [], "marker-red": [],
          "battle": []}
    txts = {"blue": [], "red": [], "marker-blue": [], "marker-red": [],
            "battle": []}
    cfg = {"blue": {"size": 20, "color": "blue"},
           "red": {"size": 20, "color": "red"},
           "marker-blue": {"size": 10, "color": "blue"},
           "marker-red": {"size": 10, "color": "red"},
           "battle": {"size": 20, "color": "purple"}}
    for ckey, content in data.items():
        x, y = to_coords(ckey)
        if content == "blue":
            xs["marker-blue"].append(x)
            ys["marker-blue"].append(y)
            txts["marker-blue"].append("Ligne de communication bleue")
        elif content == "red":
            xs["marker-red"].append(x)
            ys["marker-red"].append(y)
            txts["marker-red"].append("Ligne de communication rouge")
        elif content != []:
            status, txt = report_to_txt(content)
            xs[status].append(x)
            ys[status].append(y)
            txts[status].append(txt)
    for key in xs.keys():
        fig.add_trace(go.Scatter(x=xs[key],
                                 y=ys[key],
                                 mode="markers",
                                 marker=cfg[key],
                                 text=txts[key],
                                 hovertemplate="%{text}"))

def mk_map_graph(fname: str, infos: dict, current_turn: int) -> dcc.Graph:
    """
    Crée un graphique contenant la carte.

    Paramètres
    ----------

    fname: str
        Chemin vers l'image à utiliser comme fond de carte.

    infos: dict
        Rapports reçus par le joueur, organisés par date et par case de la
        carte.

    current_turn: int
        Tour en cours.
    """
    img = b64_image(fname)
    fig = display_base_map(img)
    display_infos(fig, infos, current_turn)
    # Disable the autosize on double click because it adds unwanted margins
    # around the image (https://plotly.com/python/configuration-options/).
    graph = dcc.Graph(figure=fig, config={"doubleClick": "reset",
                                          "displayModeBar": False})
    return graph

def build_app(dirname: str) -> dash.Dash:
    """
    Renvoie un objet `Dash` correspondant à l'interface de Picrochole.

    Paramètres
    ----------

    dirname : str
        Chemin vers le dossier contenant la partie en cours.
    """
    app = dash.Dash(title="Picrochole")
    data = load_game_dir(dirname)
    if data["config"]["ia-faction"] == "red":
        faction = "blue"
        player_hq = data["config"]["hq-blue"]
    else:
        faction = "red"
        player_hq = data["config"]["hq-red"]
    units_table = mk_units_table(faction, player_hq, data["current turn"],
                                 data["reports"], data["orders"])
    infos = organize_reports(player_hq, data["current turn"], data["reports"])
    graph = mk_map_graph(osp.join(dirname, "map.png"), infos,
                         data["current turn"])
    app.layout = html.Div([
        dcc.Markdown(children="**Partie :** %s" % dirname),
        dcc.Markdown(children="**Tour :** %d" % data["current turn"]),
        graph,
        # TODO : slider sous la carte
        units_table
    ])
    # TODO : à ce stade, il n'y a a priori pas de callback hormis le slider
    return app
