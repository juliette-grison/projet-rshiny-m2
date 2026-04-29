library(httr)
library(data.table)
library(jsonlite)
library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rAmCharts)
library(shiny)
library(shinyjs)

ui <- fluidPage(
  
  useShinyjs(),
  
  # Lien CSS
  tags$head(
    includeCSS("www/styles.css")
  ),
  
  # HEADER
  div(class = "header",
      h1("Accessibilité des Hébergements en Ile de France"),
      h2("Paris je t'aime")
  ),
  
  fluidRow(
    
    # MENU LATERAL
    column(2,
           div(class = "sidebar",
               
               # Logo / titre app
               div(class = "sidebar-brand",
                   div(class = "sidebar-brand-text", "Accessibilité IDF")
               ),
               
               hr(class = "sidebar-divider"),
               
               # Navigation
               div(class = "sidebar-nav",
                   div(class = "sidebar-section", "Navigation"),
                   
                   a(class = "sidebar-link",
                     id    = "nav-accueil",
                     onclick = "Shiny.setInputValue('tab', 'Accueil', {priority: 'event'}); setSidebarActive(this);",
                     div(class = "sidebar-link-icon", "⌂"),
                     span("Accueil")
                   ),
                   
                   a(class = "sidebar-link",
                     id    = "nav-carte",
                     onclick = "Shiny.setInputValue('tab', 'Carte', {priority: 'event'}); setSidebarActive(this);",
                     div(class = "sidebar-link-icon", "◎"),
                     span("Carte")
                   ),
                   
                   a(class = "sidebar-link",
                     id    = "nav-tableau",
                     onclick = "Shiny.setInputValue('tab', 'Tableau', {priority: 'event'}); setSidebarActive(this);",
                     div(class = "sidebar-link-icon", "☰"),
                     span("Tableau")
                   )
               ),
               
               # JS pour gérer l'état actif
               tags$script(HTML("
      function setSidebarActive(el) {
        document.querySelectorAll('.sidebar-link')
          .forEach(function(l) { l.classList.remove('sidebar-link-active'); });
        el.classList.add('sidebar-link-active');
      }
      // Actif par défaut sur Accueil
      document.addEventListener('DOMContentLoaded', function() {
        var el = document.getElementById('nav-accueil');
        if (el) el.classList.add('sidebar-link-active');
      });
    "))
           )
    ),
    
    # CONTENU
    column(10,
           
           tabsetPanel(
             id = "tabs",
             type = "hidden",
             
             tabPanel("Accueil",
                      div(class = "accueil-wrapper",
                          
                          # BLOC PRESENTATION
                          div(class = "accueil-section",
                              div(class = "accueil-section-titre", "Présentation du site"),
                              div(class = "accueil-texte",
                                  tags$p(
                                    "Ce site recense l'ensemble des hébergements touristiques accessibles 
          de la Métropole du Grand Paris ayant participé au programme d'audit 
          d'accessibilité coordonné par ", tags$b("Paris je t'aime – Office de tourisme"), 
                                    ", en partenariat avec la Ville de Paris et ses ministères associés."
                                  ),
                                  tags$p(
                                    "Dans le cadre des Jeux Olympiques et Paralympiques 2024, une vaste campagne 
          de collecte d'informations a été menée auprès des hébergements volontaires 
          (hôtels, appart-hôtels, , B&B, auberges de jeunesse, chambres d'hôtes et campings), 
          afin de fournir aux personnes en situation de handicap une information 
          précise et fiable sur les conditions d'accueil."
                                  ),
                                  tags$p("Ce site met à votre disposition deux outils complémentaires :"),
                                  tags$ul(
                                    tags$li(
                                      tags$a(
                                        tags$b("La carte interactive"),
                                        onclick = "Shiny.setInputValue('tab', 'Carte')",
                                        href    = "#",
                                        style   = "color:#071F32; text-decoration:underline; cursor:pointer;"
                                      ), 
                                      " : visualisez géographiquement l'ensemble des hébergements accessibles 
            sur le territoire. Cliquez sur un établissement pour consulter le détail 
            de ses équipements : parking, accès extérieurs et intérieurs, services, 
            personnel formé et chambres adaptées, ainsi que le lien vers son guide 
            d'accessibilité complet."
                                    ),
                                    tags$li(
                                      tags$a(
                                        tags$b("Le tableau"),
                                        onclick = "Shiny.setInputValue('tab', 'Tableau')",
                                        href    = "#",
                                        style   = "color:#071F32; text-decoration:underline; cursor:pointer;"
                                      ),
                                      " : consultez et filtrez les hébergements selon leur localisation et le 
            type de chambres adaptées disponibles (chambres PMR, sourds/malentendants, 
            aveugles/malvoyants, familiales, seniors ou communicantes). 
            Idéal pour comparer les établissements selon vos besoins spécifiques."
                                    )
                                  ),
                                  tags$p(
                                    "Les audits ont été réalisés par trois cabinets spécialisés : ",
                                    tags$a("Accèsmetrie",
                                           href   = "https://www.accesmetrie.com",
                                           target = "_blank",
                                           style  = "color:#25DCCC; font-weight:bold;"),
                                    ", ",
                                    tags$a("Action Handicap France",
                                           href   = "https://action-handicap.org",
                                           target = "_blank",
                                           style  = "color:#25DCCC; font-weight:bold;"),
                                    " et ",
                                    tags$a("LiessAccess",
                                           href   = "https://liessaccess.fr",
                                           target = "_blank",
                                           style  = "color:#25DCCC; font-weight:bold;"),
                                    ". Leur visite, gratuite pour les établissements, garantit la fiabilité 
                                    des informations présentées."
                                  )
                              )
                          ),
                          
                          # TITRE CHIFFRES CLES
                          div(class = "accueil-section",
                              div(class = "accueil-section-titre", "Quelques chiffres clés")
                          ),
                          
                          # GRAPHIQUES
                          div(class = "accueil-row",
                              
                              # GRAPHIQUE 1
                              div(class = "chart-box",
                                  div(class = "chart-header",
                                      div(class = "chart-title", "Hébergements certifiés par année"),
                                      div(class = "chart-legend",
                                          div(class = "legend-text",
                                              "Cliquez sur une part pour voir le détail mensuel")
                                      )
                                  ),
                                  
                                  div(class = "chart-area",
                                      rAmCharts::amChartsOutput("chart_annees", height = "420px")
                                  ),
                                  
                                  # Légende fixe
                                  div(class = "chart1-legende",
                                      tags$p(
                                        "Ce graphique présente la répartition des ", tags$b(nrow(data_hebergements)),
                                        " hébergements audités selon leur année de certification.",
                                        " Chaque part représente le pourcentage d'établissements certifiés cette année-là",
                                        " par rapport à l'ensemble du programme.",
                                        " Cliquez sur une part pour consulter le détail mois par mois."
                                      )
                                  )
                              ),
                              
                              # GRAPHIQUE 2
                              div(class = "chart-box",
                                  div(class = "chart-header",
                                      div(class = "chart-title", "Équipements par type de chambre"),
                                      div(class = "chart-legend",
                                          div(class = "legend-text",
                                              "Cliquez sur une barre pour voir le commentaire détaillé")
                                      )
                                  ),
                                  
                                  # Légende custom
                                  div(class = "legende-custom",
                                      actionButton("leg_total",  "Total",      class = "leg-btn leg-btn-active"),
                                      actionButton("leg_paris",  "Paris",      class = "leg-btn"),
                                      actionButton("leg_hors",   "Hors Paris", class = "leg-btn")
                                  ),
                                  
                                  div(class = "chart-area",
                                      rAmCharts::amChartsOutput("chart_2", height = "420px")
                                  ),
                                  uiOutput("chart2_commentaire")
                              )
                          )
                      )
             ),
             
             tabPanel("Carte",
                      
                      div(class = "onglet-header",
                          div(class = "accueil-section-titre", "Carte interactive"),
                          div(class = "onglet-description",
                              "Explorez la localisation des hébergements accessibles en Île-de-France. ",
                              "Utilisez le filtre pour cibler une commune ou un arrondissement, ",
                              "et cliquez sur un point pour consulter le détail complet de l'établissement."
                          )
                      ),
                      
                      
                      div(class = "filter-panel",
                          div(class = "filter-header",
                              span("Filtrer par localisation"),
                              actionLink("reset_filter", "Tout réinitialiser", class = "reset-link")
                          ),
                          div(class = "filter-search",
                              textInput("search_ville", label = NULL,
                                        placeholder = "Rechercher une ville ou un code postal...")
                          ),
                          div(class = "filter-body",
                              uiOutput("checkbox_villes")
                          )
                      ),
                      
                      div(class = "carte-info-wrapper",
                          uiOutput("carte_commentaire")
                      ),
                      
                      leafletOutput("map", height = "600px")
             ),
             
             tabPanel("Tableau",
                      
                      div(class = "onglet-header",
                          div(class = "accueil-section-titre", "Tableau des hébergements"),
                          div(class = "onglet-description",
                              "Consultez l'ensemble des hébergements audités sous forme de tableau. ",
                              "Filtrez par localisation et par type de chambre adaptée pour identifier ",
                              "rapidement les établissements correspondant à vos besoins."
                          )
                      ),
                      
                      
                      div(class = "filter-panel",
                          div(class = "filter-header",
                              span("Filtrer par localisation"),
                              actionLink("reset_filter_tableau", "Tout réinitialiser", class = "reset-link")
                          ),
                          div(class = "filter-search",
                              textInput("search_ville_tableau", label = NULL,
                                        placeholder = "Rechercher une ville ou un code postal...")
                          ),
                          div(class = "filter-body",
                              uiOutput("checkbox_villes_tableau")
                          )
                      ),
                      
                      div(class = "filter-panel", style = "margin-top: 10px;",
                          div(class = "filter-header",
                              span("Filtrer par équipements")
                          ),
                          div(class = "filter-checkboxes-grid",
                              div(class = "filter-col",
                                  checkboxGroupInput("filter_sourd", "Chambres sourds/malentendants",
                                                     choices = c("Oui" = "oui", "Non" = "non"))
                              ),
                              div(class = "filter-col",
                                  checkboxGroupInput("filter_aveugle", "Chambres aveugles/malvoyants",
                                                     choices = c("Oui" = "oui", "Non" = "non"))
                              ),
                              div(class = "filter-col",
                                  checkboxGroupInput("filter_communicantes", "Chambres communicantes",
                                                     choices = c("Oui" = "oui", "Non" = "non"))
                              ),
                              div(class = "filter-col",
                                  checkboxGroupInput("filter_famille", "Chambres famille",
                                                     choices = c("Oui" = "oui", "Non" = "non"))
                              ),
                              div(class = "filter-col",
                                  checkboxGroupInput("filter_senior", "Chambres senior",
                                                     choices = c("Oui" = "oui", "Non" = "non"))
                              ),
                              div(class = "filter-col",
                                  checkboxGroupInput("filter_pmr", "Chambres PMR",
                                                     choices = c("Oui" = "oui", "Non" = "non"))
                              )
                          )
                      ),
                      
                      div(class = "tableau-wrapper",
                          div(class = "tableau-nav",
                              div(class = "tableau-info",
                                  textOutput("tableau_info", inline = TRUE)
                              ),
                              div(class = "tableau-buttons",
                                  actionButton("prev_page", label = NULL,
                                               icon = icon("chevron-left"), class = "nav-btn"),
                                  actionButton("next_page", label = NULL,
                                               icon = icon("chevron-right"), class = "nav-btn")
                              )
                          ),
                          div(class = "tableau-scroll",
                              tableOutput("tableau")
                          )
                      ) 
             )
           )
    )
    
    
  ),
  
  
  # FOOTER
  div(class = "footer",
      
      div(class = "footer-logos",
          img(src = "nantes_universite.png", class = "footer-logo"),
          div(class = "footer-separator"),
          img(src = "ville_de_paris.jpg",    class = "footer-logo")
      ),
      
      div(class = "footer-links",
          div(class = "footer-links-title", "Ressources"),
          tags$a(
            "Source des données",
            href   = "https://opendata.paris.fr/explore/dataset/accessibilite-des-hebergements-en-ile-de-france-paris-je-t-aime/information/",
            target = "_blank",
            class  = "footer-link"
          ),
          tags$a(
            "Charte graphique Ville de Paris",
            href   = "https://chartes-graphiques.com/chartes/charte-ville-de-paris.pdf",
            target = "_blank",
            class  = "footer-link"
          )
      ),
      
      div(class = "footer-credits",
          tags$p("© 2026 — Projet de Master 2 Économétrie Appliquée"),
          tags$p("Données : Paris je t'aime – Office de tourisme")
      )
  )
)
