# =========================================================
# PROJET SHINY - ACCESSIBILITÉ DES HÉBERGEMENTS
# =========================================================
#
# SOURCE DES DONNÉES
# API Open Data Paris :
# https://opendata.paris.fr/explore/dataset/accessibilite-des-hebergements-en-ile-de-france-paris-je-t-aime/api/
#
# ---------------------------------------------------------
# MÉTHODE D'APPEL API
# ---------------------------------------------------------
#
# Type de requête : GET
#
# Paramètres utilisés :
# - limit  : nombre de lignes retournées par requête (ici 100)
# - offset : permet la pagination (déplacement dans les données)
#
# ---------------------------------------------------------
# STRATÉGIE DE RÉCUPÉRATION DES DONNÉES
# ---------------------------------------------------------
#
# Les données étant paginées, une boucle est utilisée pour
# récupérer l'ensemble du dataset :
#
# 1. Initialisation :
#    - offset = 0
#    - limit = 100
#
# 2. Boucle :
#    - Envoi d'une requête GET avec offset
#    - Récupération des données JSON
#    - Extraction de la variable "results"
#    - Stockage temporaire
#
# 3. Incrémentation :
#    - offset = offset + limit
#
# 4. Condition d'arrêt :
#    - La boucle s'arrête lorsque l'API ne retourne plus de données
#
# ---------------------------------------------------------
# FORMAT DES DONNÉES
# ---------------------------------------------------------
#
# - Format initial : JSON
# - Conversion : JSON → data.table
#
# ---------------------------------------------------------
# TRAITEMENT DES DONNÉES
# ---------------------------------------------------------
#
# - Extraction de la clé "results"
# - Fusion des différentes pages avec rbindlist()
# - Nettoyage des valeurs manquantes
#
# ---------------------------------------------------------
# STOCKAGE
# ---------------------------------------------------------
#
# Les données sont sauvegardées localement sous forme d’un fichier :
# → data_hebergements.RData
#
# Ce fichier est ensuite chargé dans l’application Shiny pour éviter
# de refaire l’appel API à chaque exécution.
#
# Commandes utilisées :
# save(data_hebergements, file = "data_hebergements.RData")
# load("data_hebergements.RData")
#
# =========================================================

# -------------------------------------------------------
# Librairies
# -------------------------------------------------------

library(httr)
library(data.table)
library(jsonlite)
library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rAmCharts)

# -------------------------------------------------------
# Appel API
# -------------------------------------------------------

connectApiUrl <- "https://opendata.paris.fr/api/explore/v2.1/catalog/datasets/accessibilite-des-hebergements-en-ile-de-france-paris-je-t-aime/records"

limit  <- 100  # Nombre maximum d'enregistrements par requête (plafond API)
offset <- 0    # Point de départ de la première requête
all_data <- list()
i <- 1

# --- Boucle de pagination ---
# Chaque itération récupère une page de 100 enregistrements.
# La boucle s'arrête automatiquement quand une page vide est reçue.
repeat {
  
  cat("Requête :", i, " | offset =", offset, "\n")
  
  req <- GET(
    url   = connectApiUrl,
    query = list(
      limit  = limit,
      offset = offset
    )
  )
  
  # Décodage de la réponse JSON en UTF-8
  resp      <- content(req, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(resp)
  
  # Conversion en data.table
  temp <- as.data.table(json_data$results)
  
  # Condition d'arrêt : page vide = toutes les données ont été récupérées
  if (nrow(temp) == 0) break
  
  all_data[[i]] <- temp
  
  offset <- offset + limit
  i      <- i + 1
}

# --- Fusion de toutes les pages ---
# fill = TRUE gère les colonnes potentiellement absentes sur certaines pages
data_hebergements <- rbindlist(all_data, fill = TRUE)

# --- Sauvegarde locale ---
# Permet de recharger les données sans rappeler l'API à chaque session.
getwd()
save(data_hebergements, file = "data_hebergements.RData")

# Chargement des données
load("data_hebergements.RData")

# -------------------------------------------------------
# Nettoyage des données
# -------------------------------------------------------

# Supprimer les espaces dans la colonne des codes postaux
data_hebergements <- data_hebergements %>%
  mutate(code_postal = gsub("[^0-9]", "", code_postal))

# Parse les colonnes type c("item1", "item2", "")
parse_colonne <- function(x) {
  # Vérification scalaire : prend uniquement le premier élément
  if (length(x) == 0) return(character(0))
  x <- as.character(x[[1]])
  if (is.na(x) || trimws(x) == "") return(character(0))
  
  if (grepl("^c\\(", trimws(x))) {
    matches <- regmatches(x, gregexpr('"([^"]*)"', x))[[1]]
    valeurs <- gsub('^"|"$', '', matches)
  } else {
    valeurs <- x
  }
  
  valeurs <- trimws(valeurs)
  valeurs <- valeurs[valeurs != ""]
  return(valeurs)
}

# Fonction qui génère le HTML d'une liste à tirets pour le popup
html_liste <- function(x) {
  x <- as.character(x[[1]])
  items <- parse_colonne(x)
  
  if (length(items) == 0) {
    return("<span style='color:#aaa; font-style:italic;'>Information non disponible</span>")
  }
  
  # Chaque item est de la forme "Niveau1 / Niveau2 / Niveau3"
  # On affiche le dernier segment en gras, les précédents en chemin grisé
  items_html <- sapply(items, function(item) {
    parts <- trimws(strsplit(item, "/")[[1]])
    parts <- parts[parts != ""]
    
    if (length(parts) == 0) return("")
    
    if (length(parts) == 1) {
      # Pas de hiérarchie
      paste0("<li style='margin-bottom:4px; color:#333;'>", parts[1], "</li>")
    } else {
      # Chemin grisé + dernier élément en couleur normale
      chemin <- paste(parts[-length(parts)], collapse = " › ")
      dernier <- parts[length(parts)]
      paste0(
        "<li style='margin-bottom:4px;'>",
        "<span style='color:#999; font-size:11px;'>", chemin, " › </span>",
        "<span style='color:#333;'>", dernier, "</span>",
        "</li>"
      )
    }
  })
  
  items_html <- items_html[items_html != ""]
  
  paste0(
    "<ul style='margin:4px 0 6px 0; padding-left:14px; list-style:disc;'>",
    paste(items_html, collapse = ""),
    "</ul>"
  )
}

html_lien <- function(x) {
  x <- as.character(x[[1]])   # force scalaire
  if (is.na(x) || trimws(x) == "") {
    return("<span style='color:#aaa; font-style:italic;'>Non disponible</span>")
  }
  paste0("<a href='", x, "' target='_blank' style='color:#25DCCC; text-decoration:none; font-weight:bold;'>Consulter le guide d'accessibilite</a>")
}

# Coordonnées manquantes
data_hebergements <- data_hebergements %>%
  mutate(
    latitude  = ifelse(adresse == "295 Avenue Daumesnil" & ville == "Paris",
                       48.8397, latitude),
    longitude = ifelse(adresse == "295 Avenue Daumesnil" & ville == "Paris",
                       2.3941,  longitude)
  )

# NA -> 0 sur les colonnes numériques
data_hebergements <- data_hebergements %>%
  mutate(
    parking_nombre_de_places    = replace_na(parking_nombre_de_places,    0),
    nb_chambre_sourd            = replace_na(nb_chambre_sourd,            0),
    nb_chambre_aveugle          = replace_na(nb_chambre_aveugle,          0),
    nb_chambres_communicantes   = replace_na(nb_chambres_communicantes,   0),
    nb_chambres_famille         = replace_na(nb_chambres_famille,         0),
    nb_chambre_senior           = replace_na(nb_chambre_senior,           0),
    nb_chambres_pmr             = replace_na(nb_chambres_pmr,             0)
  )