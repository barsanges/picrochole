# Picrochole

*Picrochole* est un jeu qui vise à simuler de manière très simplifiée
des conflits pré-napoléoniens ou napoléoniens. Le joueur endosse le
rôle d'un général en chef et doit, pour le temps d'une campagne, mener
des opérations militaires de grande ampleur sur une carte en deux
dimensions, en tenant compte du terrain, des difficultés de
communication, du ravitaillement et du moral des troupes.

La modélisation utilisée est notamment caractérisée par les points
suivants :

* La carte représente un théâtre d'opérations à part entière, pour des
  campagnes qui s'étalent sur plusieurs semaines.

* Les mouvements se font à l'échelon opérationnel. La plus petite
  unité représentée est la division.

* Les unités ont une extension géographique.

* Chaque unité ou groupe d'unités a une connaissance distincte du
  monde. Son champ de vision est limité et lui est propre. Il
  correspond à l'espace environnant normalement couvert par ses
  patrouilles. Il peut être affecté par la topographie.

* Les unités coordonnent leurs actions en s'échangeant des messages
  par estafette.

  * Les estafettes ne sont pas représentées en tant que telles. Le
    temps que met un message pour parvenir de l'expéditeur au
    destinataire se fonde simplement sur la distance à vol d'oiseau
    entre les deux unités au moment de l'envoi du message. Cette
    distance peut être inconnue de l'expéditeur.

  * La circulation de l'information au sein d'une unité est en
    revanche immédiate.

* Les batailles surviennent lorsque des unités adverses se
  rencontrent. Les unités qui atteignent une unité adverse déjà
  engagée rejoignent les affrontements et étendent la zone de combat.

  * La résolution des batailles, très schématique, s'appuie sur la
    complémentarité éventuelle des différentes unités.

* Le ravitaillement en nourriture et en matériel des unités est
  effectué de manière automatique et régulière, à partir d'un centre
  de ravitaillement allié suffisamment proche.

  * Les armées étendent progressivement leur ligne d'approvisionnement
    en installant ou en réquisitionnant des centres le long de leur
    ligne d'opération.

  * Il peut exister des centres neutres. Ils correspondent aux
    populations civiles auxquelles des contributions peuvent être
    demandées.

* Les opérations militaires affectent le moral des unités
  combattantes.

  * Être vainqueur lors d'une bataille, gagner du terrain sur
    l'adversaire ou capturer des centres de ravitaillement adverses
    améliore le moral.

  * Être vaincu lors d'une bataille ou perdre du terrain face à
    l'adversaire détériore le moral.

* Les manœuvres sont motivées par les spécificités du terrain, les
  chaînes logistiques des belligérants, et d'éventuels objectifs qui
  affectent le moral.
