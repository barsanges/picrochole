# Picrochole : sérialisation des données

Une partie de *Picrochole* correspond à un dossier contenant plusieurs
fichiers JSON qui décrivent l'état du jeu. Les différentes briques de
*Picrochole* interviennent sur ces fichiers, soit pour les lire soit
pour les altérer.

Ce document décrit la structure de chacun de ces fichiers. Il
constitue la référence à laquelle le code doit se conformer. En cas
d'ambigüité, le [code Haskell](../engine/src/Picrochole/JSON) fait
référence (dans la mesure où il est compatible avec cette
spécification).

Le dossier de jeu doit contenir les fichiers suivants :
* `atlas.json`
* `config.json`
* `current-turn.json`
* `current-units.json`
* `ia-plan.json`
* `initiative.json`
* `orders.json`
* `reports.json`

Après le début de la partie, il contient par ailleurs un dossier
`past`, où sont archivés des fichiers `units-i.json` (où `i` est un
numéro de tour) qui correspondent aux versions antérieures du fichier
`current-units.json` : le fichier `units-i.json` est le fichier
`current-units.json` tel qu'il était au début du tour `i`.

## Configuration

Le fichier de configuration décrit certains paramètres généraux de la
partie. Il s'agit d'un dictionnaire qui doit contenir les éléments
suivants :
* `ia-faction` : l'identifiant de la faction jouée par l'IA (soit
  `red` soit `blue`) ;
* `hq-blue` : l'identifiant de l'état-major de la faction bleue ;
* `hq-red` : l'identifiant de l'état-major de la faction rouge.

## Tour en cours

Le fichier du tour indique quel est le tour en cours. Il contient un
entier naturel.

## Atlas

Le fichier d'atlas décrit la grille hexagonale sur laquelle se déroule
le jeu. Il s'agit d'un dictionnaire qui doit contenir trois éléments :
* `ncols` : le nombre de colonnes de la grille ;
* `nrows` : le nombre de lignes de la grille ;
* `content` : le contenu des cases de la grille.

Plus précisèment, le vecteur `content` doit contenir `ncols * nrows`
éléments. Les éléments sont rangés par ligne : les `ncols` premiers
éléments du vecteur correspondent à la première ligne de la grille,
les `ncols` suivants à la deuxième ligne, et ainsi de suite. Chaque
élément du vecteur est constitué ainsi :
* il doit contenir un champ `topography`, qui vaut soit `road`, soit
  `land`, soit `water` ;
* si le champ `topography` vaut `road` ou `land`, l'élément doit aussi
  contenir un champ `capacity` contenant un réel positif.

## Unités

Le fichier des unités décrit la position et l'état courant de toutes
les unités actuellement en jeu. Il s'agit d'un dictionnaire dont les
clefs sont des indices de cases de la grille hexagonale, et dont les
valeurs sont soit :
* une chaîne de caractères valant `blue` ou `red`, qui indique la
  présence d'un marqueur de faction sur cette case ;
* un vecteur d'unités (l'ordre des éléments de ce vecteur n'a pas
  d'importance).

Une unité est représentée par un dictionnaire qui doit contenir :
* un champ textuel `unit-key` ;
* un champ textuel `faction`, valant soit `blue` soit `red` ;
* un champ textuel `kind`, valant soit `infantry`, soit `cavalry`,
  soit `artillery` ;
* un champ numérique `strength`, dont la valeur doit être strictement
  positive ;
* un champ `progress`, qui contient soit un réel compris entre 0 et 1,
  soit `null`.

## Initiative

Le fichier d'initiative contient un vecteur de chaînes de caractères
uniques, correspondant chacune à l'identifiant d'une unité du jeu.

## Ordres

Le fichier d'ordres contient un vecteur d'ordres.

Un ordre est représenté par un dictionnaire qui doit contenir :
* un champ textuel `from`, correspondant à l'identifiant d'une unité
  du jeu ;
* un champ textuel `to`, correspondant à l'identifiant d'une unité du
  jeu ;
* un champ `sent`, qui contient un entier correspondant au tour lors
  duquel l'ordre est envoyé ;
* un champ `received`, qui contient soit un entier (le tour lors
  duquel l'ordre est reçu) soit `null` (si l'ordre ne sera jamais
  reçu) ;
* un champ `content`, qui contient un entier positif correspondant à
  l'indice d'une case du plateau.

## Rapports

Le fichier de rapports contient un vecteur de rapports.

Un rapport est représenté par un dictionnaire qui doit contenir :
* un champ textuel `from`, correspondant à l'identifiant d'une unité
  du jeu ;
* un champ textuel `to`, correspondant à l'identifiant d'une unité du
  jeu ;
* un champ `sent`, qui contient un entier correspondant au tour lors
  duquel le rapport est envoyé ;
* un champ `received`, qui contient soit un entier (le tour lors
  duquel le rapport est reçu) soit `null` (si le rapport ne sera
  jamais reçu) ;
* un dictionnaire `content`, dont les clefs sont des indices de cases
  de la grille hexagonale, et dont les valeurs sont soit :
  * une chaîne de caractères valant `blue` ou `red`, qui indique la
    présence d'un marqueur de faction sur cette case ;
  * un vecteur d'unités tel que décrit dans le paragraphe
    [Unités](#unités).

## Plan de l'IA

Le plan de l'IA est un dictionnaire qui doit contenir :
* un champ `limit`, qui est la durée maximale de validité (en nombre
  de tours) des messages pris en compte par l'IA.
* un champ `concentration`, dont la valeur est l'indice (entier) d'une
  case du plateau ;
* un vecteur `reserve`, dont les éléments sont des identifiants
  d'unités gérées par l'IA ; l'ordre des éléments de ce vecteur n'a
  pas d'importance ;
* un vecteur `objectives` d'objectifs. L'ordre des éléments de ce
  vecteur a une importance et correspond à l'ordre de priorité des
  objectifs (le premier élément est prioritaire).

Un objectif est représenté par un dictionnaire qui doit contenir :
* un champ `target`, dont la valeur est l'indice (entier) d'une case
  du plateau ;
* un vecteur `assigned`, dont les éléments sont des identifiants
  d'unités gérées par l'IA ; l'ordre des éléments de ce vecteur n'a
  pas d'importance ;
* un vecteur `reinforcements`, dont les éléments sont des identifiants
  d'unités gérées par l'IA ; l'ordre des éléments de ce vecteur n'a
  pas d'importance.
