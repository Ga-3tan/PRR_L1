# PRR - Labo 1

## Installation

## Usage

## Rapport
### Protocole

#### client :

##### - réserver une chambre :

```http
BOOK [n° de chambre] [jour] [nb de nuit] 
```

##### - lister les disponibilités des chambres :

```http
ROOMS
```

##### - une chambre disponible pour un séjour précisé :

```http
FREE [jour] [nb de nuit]
```

##### - quitter le service :

```http
STOP
```

#### serveur :

##### réponse à une réservation :

```http
OK votre chambre a été réservée
```

```http
ERR votre chambre est déjà réservée
```

##### liste des disponibilités des chambres :

```http
OK
|1 : OCCUPIED	|
|2 : FREE		|			
|3 : YOUR ROOM	|
...
[n° de chambre : disponibilité]
END
```

##### réponse à une demande de chambre pour un séjour :

```http
OK chambre [n° chambre] disponible
```

```http
ERR aucunes chambres disponibles
```

## License
[MIT](https://choosealicense.com/licenses/mit/)
