"Interface web pour Picrochole."

import os
import os.path as osp
import base64
import json
import plotly.graph_objects as go
import dash
import dash_core_components as dcc
import dash_html_components as html

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
        if atlas["nrows"] != 60 or atlas["ncols"] != 100:
            msg = "the map should have exactly 60 rows and 100 columns, "\
                  "got %d rows x %d columns instead"\
                  % (atlas["nrows"], atlas["ncols"])
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

def select_latest_info(faction: str, player_hq: str, current_turn: int,
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
    info = select_latest_info(faction, player_hq, current_turn, reports)
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

def mk_map_graph(fname: str, img_width: int, img_height: int) -> go.Figure:
    """
    Crée un graphique contenant la carte.

    Paramètres
    ----------

    fname: str
        Chemin vers l'image à utiliser comme fond de carte.

    img_width: int
        Largeur de l'image, en pixels.

    img_height: int
        Hauteur de l'image, en pixels.
    """
    fig = go.Figure()
    scale_factor = 1.0
    fig.update_xaxes(range=[0, img_width * scale_factor],
                     visible=False)
    fig.update_yaxes(range=[0, img_height * scale_factor],
                     # Ensures that the aspect ratio stays constant:
                     scaleanchor="x",
                     visible=False)
    img = b64_image(fname)
    fig.add_layout_image(source=img,
                         x=0,
                         sizex=img_width * scale_factor,
                         y=img_height * scale_factor,
                         sizey=img_height * scale_factor,
                         xref="x",
                         yref="y",
                         opacity=1.0,
                         layer="below",
                         sizing="stretch")
    # Configure other layout
    fig.update_layout(width=img_width * scale_factor,
                      height=img_height * scale_factor,
                      margin={"l": 0, "r": 0, "t": 0, "b": 0},
                      showlegend=False)
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
    img_width = 800
    img_height = 455
    graph = mk_map_graph(osp.join(dirname, "map.png"), img_width, img_height)
    app.layout = html.Div([
        dcc.Markdown(children="**Partie :** %s" % dirname),
        dcc.Markdown(children="**Tour :** %d" % data["current turn"]),
        graph,
        # TODO : slider sous la carte
        units_table
    ])
    # TODO : à ce stade, il n'y a a priori pas de callback hormis le slider
    return app
