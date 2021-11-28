# PRR | Labo 2 - Guide pour provoquer un accès concurrent (version Lamport)

> Auteurs : Marco Maziero, Gaétan Zwick
> 
> **/!\ Une version avec screenshots est disponible dans le README.md pricipal du repo**

### Description

Ce guide détaille les étapes pour provoquer un accès concurrent lors d'un envoi de la même commande depuis plusieurs clients vers plusieurs serveurs différents du pool. Cela permet de vérifier le bon fonctionnement de la gestion de la concurrence.

Lorsqu'il est en mode `debug`, le serveur démarre et accepte les nouvelles connexions client. Le contexte s'occupant du traitement des requêtes se met en pause pendant X secondes afin de laisser le temps à la création d'un accès concurrent.

Pour ce test, le pool contient **trois serveurs (0, 1 et 2)** interconnectés et **deux clients** effectuant la même requete `BOOK 1 2 3` en même temps vers les serveurs 1 et 2.

### Etapes

**1 - Activer le mode `debug`**

-> Ouvrir le fichier `config.go`se trouvant dans le dossier `hôtel/config`

-> Modifier le champ `DEBUG` en lui affectant la valeur 1 :

```go
DEBUG = 1
```

-> Modifier si nécessaire le temps de "sommeil" du serveur au démarrage. Ce temps de sommeil sera le temps pour lancer les deux clients et écrire les deux requêtes. (temps conseillé 45s-60s)

```go
DEBUG_SLEEP = X // Remplacer X par le nombre de secondes à attendre
```

**2 - Préparer plusieurs terminaux de commande**

-> Ouvrir 5 terminaux de commande et les placer côte à côte

-> Démarrer deux instances de clients se connectant aux serveurs 1 et 2

```bash
client 1
client 2
```

-> Démarrer trois instances de serveurs

> ATTENTION /!\ Au démarrage du dernier serveur, le pool va se connecter et vous aurez X secondes pour provoquer l'accès concurrent !

```bash
server 0
server 1
server 2
```

**3 - Provoquer l'accès concurrent**

-> Dès que le serveur est lancé, entrer deux noms de différents sur chaque client

-> Envoyer la requête suivante depuis les deux clients

```sh
BOOK 1 2 3
```

> Cette commande demande la réservation de la chambre 1 depuis le jour 2 pendant 3 nuits

**4 - Résultat de la manipulation**

-> A ce moment, les deux clients vont se bloquer jusqu'à ce que les serveur terminent ensemble les X secondes d'attente. Ils vont ensuite traîter les deux commandes et envoyer les retours aux clients.

-> L'un des clients aura donc obtenu la réservation et l'autre reçevra un message de refus, l'accès concurent aura été provoqué et correctement géré par l'algorithme de Lamport afin que l'état de l'hotel soit le même sur tous les serveurs.

Le log des serveurs permet de suivre l'exécution de l'algorithme de Lamport. Les accès à la section critique et les synchronisation de commandes sont mises en évidence.
