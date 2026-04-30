![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-1E90FF?style=for-the-badge)

# Accessibilité des Hébergements en Île-de-France — Paris je t\'aime

Application Shiny interactive présentant l\'accessibilité des hébergements touristiques
de la Métropole du Grand Paris ayant participé au programme d\'accompagnement dédié de
**Paris je t\'aime – Office de tourisme**.

---

## Aperçu

L\'application propose trois onglets :

- **Accueil** : présentation du projet, chiffres clés (répartition par année de certification,
  équipements par type de chambre)
- **Carte** : carte interactive des hébergements avec filtres par localisation et détail
  complet de chaque établissement au clic
- **Tableau** : tableau paginé et filtrable par localisation et type de chambre adaptée

---

## Source des données

Les données proviennent de l\'open data de la Ville de Paris :

**Accessibilité des Hébergements en Île-de-France — Paris je t\'aime**  
[https://opendata.paris.fr/explore/dataset/accessibilite-des-hebergements-en-ile-de-france-paris-je-t-aime/information/](https://opendata.paris.fr/explore/dataset/accessibilite-des-hebergements-en-ile-de-france-paris-je-t-aime/information/)

Les données sont récupérées dynamiquement via l\'API OpenData Paris au lancement de
l\'application (pagination automatique, ~536 établissements).

---

## Structure du projet

```text
projet_rshiny_m2/
├── app.R          # Chargement des librairies, appel API, nettoyage des données
├── server.R       # Logique serveur (graphiques, carte, tableau, filtres)
├── ui.R           # Interface utilisateur (mise en page, onglets, widgets)
├── www/
│   ├── styles.css             # Feuille de style personnalisée
│   ├── nantes_universite.png  # Logo Nantes Université
│   └── ville_de_paris.jpg     # Logo Ville de Paris
└── README.md
```

---

## Prérequis

### Version R

R >= 4.1.0 recommandé.

### Packages R requis

Installez les packages nécessaires avec :

```r
install.packages(c(
  "shiny",
  "dplyr",
  "tidyr",
  "leaflet",
  "rsconnect",
  "data.table",
  "shinyjs",
  "httr",
  "ggplot2",
  "rAmCharts",
  "jsonlite"
))
```

---

## Installation et lancement

### 1. Cloner le dépôt

```bash
git clone https://github.com/juliette-grison/projet-rshiny-m2.git
cd projet-rshiny-m2
```

### 2. Ouvrir le projet dans RStudio

Double-cliquez sur le fichier `.Rproj` ou ouvrez RStudio et définissez le répertoire
de travail :

```r
setwd("chemin/vers/projet-rshiny-m2")
```

### 3. Installer les dépendances

```r
source("app.R")  # vérifie les packages au premier lancement
```

Ou installez manuellement avec le bloc ci-dessus.

### 4. Lancer l\'application

```r
shiny::runApp()
```

---

## Appel API

L\'application interroge l\'API OpenData Paris au démarrage pour récupérer les données
à jour. La pagination est gérée automatiquement (100 enregistrements par requête).

```r
connectApiUrl <- "https://opendata.paris.fr/api/explore/v2.1/catalog/datasets/accessibilite-des-hebergements-en-ile-de-france-paris-je-t-aime/records"
```

Si vous souhaitez travailler hors-ligne, les données peuvent être sauvegardées
localement après le premier lancement :

```r
save(data_hebergements, file = "data_hebergements.RData")
# Puis rechargées avec :
load("data_hebergements.RData")
```

---

## Déploiement sur shinyapps.io

### 1. Configurer rsconnect

```r
library(rsconnect)
rsconnect::setAccountInfo(
  name   = "<votre-compte>",
  token  = "<votre-token>",
  secret = "<votre-secret>"
)
```

### 2. Déployer

```r
rsconnect::deployApp(appDir = ".")
```

---

## Charte graphique

L\'application suit la charte graphique de la Ville de Paris.  
[Consulter la charte](https://chartes-graphiques.com/chartes/charte-ville-de-paris.pdf)

| Couleur        | Hex       | Usage                        |
|----------------|-----------|------------------------------|
| Bleu nuit      | `#071F32` | Fond header, sidebar, titres |
| Turquoise      | `#25DCCC` | Accents, liens, bordures     |
| Jaune          | `#FFCD00` | Graphiques                   |
| Rouge          | `#FB394A` | Graphiques                   |
| Gris clair     | `#F0F0F0` | Fonds secondaires            |

Police : **Montserrat** (Google Fonts)

---

## Fonctionnalités détaillées

### Carte
- Fond de carte : CartoDB Positron
- Pins SVG personnalisés aux couleurs de la charte
- Filtre par ville / code postal avec recherche textuelle (tolère les espaces,
  insensible à la casse)
- Popup au clic : nom, adresse, guide d\'accessibilité (lien cliquable), parking,
  extérieurs, intérieurs, services, personnel, chambres adaptées (liste hiérarchique)
- Commentaire réactif indiquant le nombre d\'établissements visibles dans la zone

### Tableau
- 10 entrées par page avec navigation
- Filtre localisation identique à la carte
- Filtres Oui/Non par type de chambre adaptée (cumulables)
- Réinitialisation en un clic

### Graphiques (onglet Accueil)
- **Graphique 1** : Pie chart de la répartition des certifications par année
  (2022, 2023, 2024) avec popup mensuel au clic
- **Graphique 2** : Bar chart empilé du % d\'hébergements par type de chambre adaptée,
  avec vue Paris / Hors Paris et commentaire réactif au clic

---

## Données — colonnes principales

| Colonne                    | Description                              |
|----------------------------|------------------------------------------|
| `etablissement`            | Nom de l\'établissement                   |
| `adresse`                  | Adresse complète                         |
| `ville`                    | Ville                                    |
| `code_postal`              | Code postal                              |
| `latitude` / `longitude`   | Coordonnées géographiques                |
| `date`                     | Date de certification                    |
| `lien_guide`               | URL du guide d\'accessibilité             |
| `parking`                  | Informations parking                     |
| `exterieurs`               | Accessibilité extérieure                 |
| `interieurs`               | Accessibilité intérieure                 |
| `services`                 | Services adaptés                         |
| `personnel`                | Formation du personnel                   |
| `chambres_adapees`         | Détail des chambres adaptées             |
| `nb_chambre_sourd`         | Nb chambres sourds/malentendants         |
| `nb_chambre_aveugle`       | Nb chambres aveugles/malvoyants          |
| `nb_chambres_communicantes`| Nb chambres communicantes                |
| `nb_chambres_famille`      | Nb chambres familiales                   |
| `nb_chambre_senior`        | Nb chambres seniors                      |
| `nb_chambres_pmr`          | Nb chambres PMR                          |
| `parking_nombre_de_places` | Nombre de places de parking              |

---

## Auteurs

Projet réalisé par Juliette GRISON dans le cadre d'un **Master 2 en Économétrie Appliquée**  
Données fournies par **Paris je t\'aime – Office de tourisme** et la **Ville de Paris**

---

## Licence

Les données sont disponibles sous licence ouverte (Open Data).  
[Licence Ouverte / Open Licence](https://www.etalab.gouv.fr/licence-ouverte-open-licence)
