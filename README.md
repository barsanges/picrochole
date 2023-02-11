# Picrochole

*Picrochole* est une simulation simple à l'échelon opérationnel de
conflits pré-napoléoniens et napoléoniens. Deux documents décrivent la
simulation :

* Une [note d'intention](docs/intent.md) décrit le projet
  général. Elle peut ne pas correspondre à l'état actuel de
  l'implémentation.

* Les [règles](docs/rules.md) spécifient l'algorithme implémenté par
  le moteur de jeu. Elles constituent la référence à laquelle le code
  doit se conformer.

**Licence :** ce code est distribué sous la licence GNU GPL (cf. le
fichier [LICENSE](LICENSE)).

**Besoins d'évolution :**
- [ ] Ajouter aux cartes des éléments de toponymie
- [ ] Améliorer l'IA
- [ ] Ajouter un échelon de commandement (e.g. : corps d'armée)
- [ ] Ajouter une condition de victoire (dans le code et dans les
      règles) et clarifier le document de règles en conséquence

**Pistes d'évolution :**
- [ ] Ajouter une notion de ravitaillement et la possibilité de vivre
      sur le pays
- [ ] Ajouter la possibilité d'altérer l'environnement (tranchées,
      ponts, etc)
- [ ] Ajouter une notion de moral
- [ ] Utiliser Elm ou Haskell à la place de Python pour l'interface
- [ ] Ajouter un mode multijoueur
- [ ] Ajouter des logs
- [ ] Étoffer le README
- [ ] Créer des scénarios
- [ ] Ajouter une commande pour créer une nouvelle partie
- [ ] Automatiser la création des rapports initiaux lors de la
      création d'une nouvelle partie
- [ ] Présenter les tours sous forme de date
- [ ] Réfléchir à l'intérêt d'introduire une alternance jour / nuit
- [ ] Autoriser des unités de factions différentes à avoir le même nom
- [ ] Réduire la portée de l'artillerie
- [ ] Faire dépendre la capacité de la cellule du type de terrain
- [ ] Ajouter d'autres types de terrain
- [ ] Introduire une notion de défenseur et / ou de couvert pour
      certains terrains
- [ ] Intégrer aux rapports des unités ce qu'elles ont vu entre deux
      rapports (et pas uniquement ce qu'elles voient au moment du
      rapport)
