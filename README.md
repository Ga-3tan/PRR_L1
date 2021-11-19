

# PRR | Laboratoire 1 - Réservation de chambres d'hôtel

## Installation

### 0 - Structure du projet

Le projet est consitué de deux principaux dossiers : `client` et `server`

Chacun de ces dossiers contient un ficheir `main.go` qui représente le point d'entrée de chacun des deux programmes. Dans `client` il y a aussi un fichier `main_test.go` qui contient tous les tests permettant de vérifier que le service de réservation fonctionne correctement.

Le code est ensuite divisé dans divers packets qui séparent les différents aspects des programmes.

- Les packets `clientTcp` et `serverTcp` contiennent tout ce qui touche aux connexions tcp des programmes
- Les packets `utils` et `config` contiennent du code utilisé par les deux programmes 
- Le packet `cmd` contient tout ce qui touche aux commandes utilisateur
- Le packet `logic` regroupe tout ce qui touche à la logique de réservation de chambres de l'hôtel

Plus d'information est disponible en générant une documentation grâce à l'utilitaire `godoc`

### 1 - Cloner le repository

Pour commencer, ouvrir un terminal et exécuter la commande suivante :

```bash
git clone git@github.com:Ga-3tan/PRR_L1_Reservation_Hotel.git
```

ou

```bash
git clone https://github.com/Ga-3tan/PRR_L1_Reservation_Hotel.git
```

Un dossier va se créer avec tout le contenu du laboratoire. Il faut ensuite build le client et le serveur pour pouvoir les utiliser.

### 2 - Build le projet

**Build le serveur**

Pour build le serveur, il faut se rendre dans le dossier `PRR_L1_Reservation_Hotel/server` et ouvrir un terminal de commande dans ce dossier.

Utiliser ensuite la commande suivante pour créer un fichier exécutable du programme (golang doit être installé sur la machine au préalable)

```bash
go build
```

Un fichier `server` sera crée et pourra être lancé dans un terminal.

**Build le client**

Pour build le serveur, il faut se rendre dans le dossier `PRR_L1_Reservation_Hotel/client` et ouvrir un terminal de commande dans ce dossier.

Utiliser ensuite la commande suivante pour créer un fichier exécutable du programme (golang doit être installé sur la machine au préalable)

```bash
go build
```

Un fichier `client` sera crée et pourra être lancé dans un terminal.

### 3 - Utiliser le client et le serveur

Maintenant que les deux programes ont été build, il est possible de les lancer dans plusieurs terminaux de commande. Il est possible de lancer plusieures instances de clients mais uniquement un seul serveur.

### 4 - Lancer les tests

Pour lancer les tests il suffit d'ouvrir un terminal dans le dossier `PRR_L1_Reservation_Hotel/client` et lancer la commande suivante.

```bash
go test
```

/!\ Pour que les tests fonctionnent il faut que :

- Il y ait au minimum 10 chambres
- Il y ait au minimum 20 jours
- L'instace du serveur lancée en arrière plan soit vierge (pas de chambres déjà réservées au lancement des tests)

Ces valeurs sont modifiables dans le fichier `config.go` qui se trouve dans le dossier `PRR_L1_Reservation_Hotel/config`

## Fonctionnalités

**Fonctionne**

- Réserver une chambre libre pour un jour donné et un nombre de nuits
- Lister le status (Libre, occupé, réservé) des chambres selon un jour donné
- Trouver une chambre libre selon un jour et un nombre de nuits donnés
- Lancer le serveur en mode debug (attente de X secondes avant la prise de requêtes client)
- Terminer la connexion au serveur avec la commande `STOP`
- Le fichier `config.go` dans le dossier `PRR_L1_Reservation_Hotel/config` contient les paramètres modifiables du programme
- Une documentation peut être générée grâce à l'utilitaire `godoc`
- L'utilitaire `go race` ne relève aucun problème d'accès concurrent pendant l'utilisation du serveur et de plusieurs clients

**Ne fonctionne pas**

- Le test pour vérifier la commande `ROOMS` ne fonctionne pas. Le test est commenté dans le fichier `main_test`dans le dossier `PRR_L1_Reservation_Hotel/client`. (La commande fonctionne sans problèmes et retourne des résultats corrects)

## Protocole

### Spécificités

- Une chambre réservée le jour 1 pour 5 nuits sera libre le jour 6
- Les numéros de chambre et de jours commencent à 1

### Commandes

#### Connexion 
##### Premier message lors d'une connexion :
##### d'un serveur

```css
SRV
```

##### d'un client

```css
CLI
```

#### Message venant d'un serveur
##### Lamport

```css
LPRT [COMMAND]
```

##### Message de synchronisation

```css
SYNC [USERNAME]|[COMMAND]
```

##### Réserver une chambre

```css
BOOK [n° de chambre] [jour] [nb de nuits] 
```

##### Lister les disponibilités des chambres

```css
ROOMS [jour]
```

##### Une chambre disponible pour un séjour précisé

```css
FREE [jour] [nb de nuits]
```

##### Quitter le service

```css
STOP
```

### Types de réponses

##### Réponse à une réservation

```css
BOOK 5 2 3
OK votre chambre a été réservée
```

```css
BOOK 1 2 3
ERR votre chambre est déjà réservée
```

##### Liste des disponibilités des chambres

```css
ROOMS 7
| Chambre: 1, Status: OCCUPE
| Chambre: 2, Status: LIBRE
| Chambre: 3, Status: LIBRE
| Chambre: 4, Status: LIBRE
| Chambre: 5, Status: LIBRE
| Chambre: 6, Status: LIBRE
| Chambre: 7, Status: LIBRE
| Chambre: 8, Status: LIBRE
| Chambre: 9, Status: LIBRE
| Chambre: 10, Status: LIBRE
END
```

##### Réponse à une demande de chambre pour un séjour

```css
FREE 5 2
OK chambre 1 disponible
```

## Guide pour provoquer un accès concurrent

### Description

Ce guide détaille les étapes pour provoquer un accès concurrent lors d'un envoi de la même commande depuis plusieurs clients vers le serveur. Cela permet de vérifier le bon fonctionnement de la gestion de la concurrence.

Lorsqu'il est en mode `debug`, le serveur démarre et accepte les nouvelles connexions client. Le contexte s'occupant du traitement des requêtes se met en pause pendant X secondes afin de laisser le temps à la création d'un accès concurrent.

### Etapes

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

## License

[MIT](https://choosealicense.com/licenses/mit/)
