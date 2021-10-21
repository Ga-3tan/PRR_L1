# PRR - Labo 1

## Installation

## Usage

## Rapport
### Protocole

#### Client :

##### - Réserver une chambre :

```http
BOOK [n° de chambre] [jour] [nb de nuit] 
```

##### - Lister les disponibilités des chambres :

```http
ROOMS [jour]
```

##### - Une chambre disponible pour un séjour précisé :

```http
FREE [jour] [nb de nuit]
```

##### - Quitter le service :

```http
STOP
```

#### Serveur :

##### Réponse à une réservation :

```http
>> BOOK 5 2 3
OK votre chambre a été réservée
```

```http
>> BOOK 1 2 3
ERR votre chambre est déjà réservée
```

##### Liste des disponibilités des chambres :

```http
>> ROOMS 7
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

##### Réponse à une demande de chambre pour un séjour :

```http
>> FREE 5 2
OK chambre 1 disponible
```

## License
[MIT](https://choosealicense.com/licenses/mit/)
