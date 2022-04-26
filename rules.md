# Picrochole

*Picrochole* est un jeu qui vise à simuler de manière très simplifiée
des conflits pré-napoléoniens ou napoléoniens.

## Matériel

Le jeu est constitué :

* d'une carte hexagonale, sur laquelle est dessinée un réseau routier ;

* de marqueurs bleus (resp. rouges) ;

* d'une série d'unités bleues (resp. rouges). Une unité est définie
  par son type (infanterie, cavalerie, artillerie) et par son effectif
  courant (de 1 à 4).

## Déroulement du jeu

La partie se décompose en tours. Un tour est constitué de quatre
phases : mouvement, encerclement, bombardement et combat. Toutes les
unités sont activées à chaque tour.

La partie se termine quand l'un des deux camps capitule ou est
éliminé.

### Phase 1 : mouvement

À chaque tour, chaque unité a la possibilité d'effectuer un
mouvement. Les unités bougent dans un ordre prédéterminé et immuable,
qui peut mélanger les unités bleues et les unités rouges.

La vitesse maximale des unités dépend de leur type et de la nature du
terrain :

* les unités de cavalerie bougent de deux cases par tour sur route, et
  d'une case sur les autres terrains ;

* les unités d'infanterie bougent d'une case par tour sur route,
  et d'une demie case sur les autres terrains ;

* les unités d'artillerie bougent d'une case par tour sur route,
  et d'un quart de case sur les autres terrains.

À un instant donné, une case du plateau ne peut accueillir qu'un
effectif limité pour chaque camp : cet effectif est généralement
limité à 12 points par camp.

Lorsqu'une unité quitte une case qui n'est pas occupée par une unité
adverse, elle y laisse un marqueur de son camp. Lorsqu'elle entre sur
une case sur laquelle se trouve un marqueur de l'autre camp, celui-ci
est immédiatement défaussé.

Une unité qui quitte une case sur laquelle se trouve une unité adverse
(i.e. : l'unité se désengage d'un combat) ne peut se déplacer que sur
une case sur laquelle se trouve soit un marqueur soit une unité de son
camp.

La phase de mouvement est intégralement résolue pour toutes les unités
du plateau, bleues et rouges, avant de passer à la phase
d'encerclement.

### Phase 2 : encerclement

Des unités d'un même camp qui occupent un ensemble de cases connexes
dont la bordure est constituée exclusivement de cases infranchissables
(e.g. rivière) ou occupées par des unités adverses sont dites
encerclées.

À l'issue de la phase d'action toutes les unités encerclées sont
détruites. En cas d'encerclement multiple (i.e. : les unités
encerclantes sont elles-mêmes encerclées), toutes les unités
encerclées sont détruites.

La phase d'encerclement est intégralement résolue pour toutes les
unités du plateau, bleues et rouges, avant de passer à la phase de
combat.

### Phase 3 : bombardement

À chaque tour, chaque unité d'artillerie a la possibilité d'effectuer
un tir. Les unités tirent dans le même ordre que celui utilisé pour la
phase de mouvement.

Chaque unité d'artillerie peut tirer sur sa propre case ou sur une
case adjacente. Elle vise automatiquement la case qui contient le plus
grand nombre de troupes ennemies, et réduit l'effectif de la plus
forte des unités adverses de *x / 15* dégâts, où *x* est son propre
effectif à l'instant du tir. Une unité dont l'effectif devient négatif
ou nul est retirée du jeu.

### Phase 4 : combat

La phase de combat se déroule simultanément pour toutes les unités du
plateau engagées dans un combat (i.e. : les unités présentes sur la
même case qu'au moins une unité adverse).

Sur une case donnée, les unités d'infanterie et de cavalerie d'un camp
infligent *x / 30* dégâts à l'unité la plus forte du camp adverse, où
*x* est l'effectif total des unités alliées engagées dans le combat,
hors artillerie. Une unité dont l'effectif devient négatif ou nul est
retirée du jeu.

Une fois que la phase de combat est résolue pour toutes les unités du
plateau, bleues et rouges, on passe au tour suivant.

## Champ de vision

Le champ de vision des unités est limité :

* la cavalerie a un champ de vision de 4 cases de rayon ;

* l'infanterie et l'artillerie ont un champ de vision de 2 cases de
  rayon ;

* une unité engagée en combat a un champ de vision d'une case de
  rayon, quel que soit son type.
