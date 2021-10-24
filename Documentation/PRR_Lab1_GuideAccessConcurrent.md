# PRR | Labo 1 - Guide pour provoquer un accès concurrent

> Auteurs : Marco Maziero, Gaétan Zwick

## Description

Ce guide détaille les étapes pour provoquer un accès concurrent lors d'un envoi de la même commande depuis plusieurs clients vers le serveur. Cela permet de vérifier le bon fonctionnement de la gestion de la concurrence.

Lorsqu'il est en mode `debug`, le serveur démarre et accepte les nouvelles connexions client. Le contexte s'occupant du traitement des requêtes se met en pause pendant X secondes afin de laisser le temps à la création d'un accès concurrent.

## Etapes

**1 - Activer le mode `debug`**

-> Ouvrir le fichier `config.go`se trouvant dans le dossier `hôtel/config`

-> Modifier le champ `DEBUG` en lui affectant la valeur 1 :

```go
DEBUG = 1
```

-> Modifier si nécessaire le temps de "sommeil" du serveur au démarrage

```go
DEBUG_SLEEP = X // Remplacer X par le nombre de secondes à attendre
```

**2 - Préparer plusieurs terminaux de commande**

-> Ouvrir 3 terminaux de commande et les placer côte à côte

-> Démarrer deux instances de clients

-> Démarrer une instance de serveur

> ATTENTION /!\ Au démarrage du serveur, vous aurez X secondes pour provoquer l'accès concurrent !

**3 - Provoquer l'accès concurrent**

-> Dès que le serveur est lancé, entrer deux noms de différents sur chaque client

-> Envoyer la requête suivante depuis les deux clients

```sh
BOOK 1 2 3
```

> Cette commande demande la réservation de la chambre 1 depuis le jour 2 pendant 3 nuits

**4 - Résultat de la manipulation**

-> A ce moment, les deux clients vont se bloquer jusqu'à ce que le serveur termine les X secondes d'attente. Il va ensuite traîter les deux commandes et envoyer les retours aux clients

-> L'un des clients aura donc obtenu la réservation et l'autre reçevra un message de refus, l'accès concurent aura été provoqué et  correctement géré 

