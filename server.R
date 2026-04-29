server <- function(input, output, session) {
  
  observeEvent(input$tab, {
    updateTabsetPanel(session, "tabs", selected = input$tab)
  })
  
  
  # -------------------------------------------------------
  # Préparation données graphique 1
  # -------------------------------------------------------
  
  data_chart <- reactive({
    data_hebergements %>%
      filter(!is.na(date)) %>%
      mutate(
        date_parsed = as.Date(date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d")),
        annee       = as.integer(format(date_parsed, "%Y")),
        mois        = as.integer(format(date_parsed, "%m")),
        mois_label  = format(date_parsed, "%m")
      )
  })
  
  # -------------------------------------------------------
  # Graphique rAmCharts
  # -------------------------------------------------------
  
  observeEvent(input$chart_annees_clickItem, {
    click     <- input$chart_annees_clickItem
    label     <- gsub("[^0-9]", "", as.character(click$label))
    annee_val <- suppressWarnings(as.integer(label))
    
    if (is.na(annee_val)) return()
    
    mois_labels <- c("01"="Janvier","02"="Février","03"="Mars","04"="Avril",
                     "05"="Mai","06"="Juin","07"="Juillet","08"="Août",
                     "09"="Septembre","10"="Octobre","11"="Novembre","12"="Décembre")
    
    df_mois <- data_hebergements %>%
      filter(!is.na(date)) %>%
      mutate(
        annee      = as.integer(format(as.Date(date), "%Y")),
        mois_label = format(as.Date(date), "%m")
      ) %>%
      filter(annee == annee_val) %>%
      group_by(mois_label) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(mois_nom = mois_labels[mois_label]) %>%
      filter(!is.na(mois_nom)) %>%
      arrange(mois_label)
    
    lignes <- apply(df_mois, 1, function(row) {
      tags$div(class = "popup-ligne",
               tags$span(class = "popup-mois", row["mois_nom"]),
               tags$span(class = "popup-n",    paste0(row["n"], " hébergements"))
      )
    })
    
    showModal(modalDialog(
      title    = div(class = "popup-titre", paste0("Détail mensuel — ", annee_val)),
      div(class = "popup-body", lignes),
      footer   = tagList(
        modalButton("Fermer"),
        # Script JS exécuté à la fermeture pour reset toutes les parts
        tags$script(HTML("
        $(document).on('hide.bs.modal', function() {
          setTimeout(function() {
            Shiny.setInputValue('reset_pie_slice', Math.random(), {priority: 'event'});
          }, 50);
        });
      "))
      ),
      easyClose = TRUE,
      size      = "s"
    ))
  })
  
  # Reset immédiat 
  observeEvent(input$reset_pie_slice, {
    shinyjs::runjs("
    AmCharts.charts.forEach(function(chart) {
      if (chart.type === 'pie') {
        chart.dataProvider.forEach(function(dp) {
          dp.pulled = false;
        });
        chart.validateData();
      }
    });
  ")
  })
  
  output$chart_annees <- rAmCharts::renderAmCharts({
    
    total <- nrow(data_hebergements)
    
    df_annee <- data_hebergements %>%
      filter(!is.na(date)) %>%
      mutate(annee = as.integer(format(as.Date(date), "%Y"))) %>%
      filter(annee %in% c(2022, 2023, 2024)) %>%
      group_by(annee) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(
        label   = as.character(annee),
        pct     = round(n / total * 100, 2),
        desc    = paste0(pct, "%")
      ) %>%
      arrange(annee)
    
    amPieChart(
      dataProvider     = df_annee,
      titleField       = "label",
      valueField       = "n",
      descriptionField = "desc"
    ) %>%
      setProperties(
        colors             = list("#FB394A", "#25DCCC", "#FFCD00"),
        labelText          = "[[label]]\n[[desc]]",
        balloonText        = "<b>[[label]]</b><br>[[n]] hébergements ([[desc]])",
        outlineColor       = "white",
        outlineAlpha       = 1,
        outlineThickness   = 2,
        labelsEnabled      = TRUE,
        labelRadius        = -40,
        fontSize           = 13,
        fontFamily         = "Montserrat",
        boldLabels         = TRUE,
        startDuration      = 0,
        radius             = "42%",
        pullOutRadius      = "8%",      
        pullOutOnlyOne     = TRUE       
      ) %>%
      addListener(
        name = "clickSlice",
        expression = "function(e) {
        var ctx = e.dataItem.dataContext;
        var lbl = ctx !== undefined && ctx.label !== undefined ? String(ctx.label) : '';
        if (lbl !== '') {
          Shiny.setInputValue(
            'chart_annees_clickItem',
            { label: lbl },
            { priority: 'event' }
          );
        }
      }"
      )
  })
  
  
  # -------------------------------------------------------
  # Données graphique 2
  # -------------------------------------------------------
  
  data_chart2 <- reactive({
    
    total <- nrow(data_hebergements)
    
    cols <- c(
      "Ch. malentendants"  = "nb_chambre_sourd",
      "Ch. malvoyants"     = "nb_chambre_aveugle",
      "Ch. communicantes"  = "nb_chambres_communicantes",
      "Ch. familiales"     = "nb_chambres_famille",
      "Ch. seniors"        = "nb_chambre_senior",
      "Ch. PMR"            = "nb_chambres_pmr"
    )
    
    commentaires <- c(
      "Ch. malentendants"  = "accessibles aux personnes sourdes ou malentendantes",
      "Ch. malvoyants"     = "accessibles aux personnes aveugles ou malvoyantes",
      "Ch. communicantes"  = "communicantes (reliées à une chambre adjacente)",
      "Ch. familiales"     = "adaptées aux familles avec enfants",
      "Ch. seniors"        = "adaptées aux personnes âgées",
      "Ch. PMR"            = "adaptées aux personnes à mobilité réduite (PMR)"
    )
    
    total_paris <- sum(data_hebergements$ville == "Paris", na.rm = TRUE)
    total_hors  <- total - total_paris
    
    do.call(rbind, lapply(names(cols), function(nom) {
      col <- cols[nom]
      
      n_paris <- sum(
        data_hebergements$ville == "Paris" &
          data_hebergements[[col]] > 0, na.rm = TRUE)
      n_hors <- sum(
        data_hebergements$ville != "Paris" &
          data_hebergements[[col]] > 0, na.rm = TRUE)
      n_total <- n_paris + n_hors
      
      pct_total        <- round(n_total  / total       * 100, 1)
      pct_paris_abs    <- round(n_paris  / total_paris * 100, 1)  
      pct_hors_abs     <- round(n_hors   / total_hors  * 100, 1)  
      
      pct_paris_within <- if (n_total > 0) round(n_paris / n_total * 100, 1) else 0
      pct_hors_within  <- if (n_total > 0) round(n_hors  / n_total * 100, 1) else 0
      
      # Barres empilées — fraction de pct_total
      pct_paris_bar <- if (n_total > 0) round(n_paris / n_total * pct_total, 1) else 0
      pct_hors_bar  <- if (n_total > 0) round(n_hors  / n_total * pct_total, 1) else 0
      
      data.frame(
        label            = nom,
        commentaire      = commentaires[nom],
        pct_total        = pct_total,
        pct_paris_abs    = pct_paris_abs,
        pct_hors_abs     = pct_hors_abs,
        pct_paris_bar    = pct_paris_bar,
        pct_hors_bar     = pct_hors_bar,
        pct_paris_within = pct_paris_within,
        pct_hors_within  = pct_hors_within,
        n_total          = n_total,
        label_top        = 0,
        stringsAsFactors = FALSE
      )
    }))
  })
  
  # -------------------------------------------------------
  # Légende custom — boutons
  # -------------------------------------------------------
  
  legende_selectionnee <- reactiveVal("total")
  
  observeEvent(input$leg_total, {
    legende_selectionnee("total")
    shinyjs::runjs("
    $('.leg-btn').removeClass('leg-btn-active');
    $('#leg_total').addClass('leg-btn-active');
  ")
  })
  
  observeEvent(input$leg_paris, {
    legende_selectionnee("paris")
    shinyjs::runjs("
    $('.leg-btn').removeClass('leg-btn-active');
    $('#leg_paris').addClass('leg-btn-active');
  ")
  })
  
  observeEvent(input$leg_hors, {
    legende_selectionnee("hors")
    shinyjs::runjs("
    $('.leg-btn').removeClass('leg-btn-active');
    $('#leg_hors').addClass('leg-btn-active');
  ")
  })
  
  # -------------------------------------------------------
  # Commentaire réactif
  # -------------------------------------------------------
  
  chambre_selectionnee <- reactiveVal("Ch. malentendants")
  
  observeEvent(input$chart2_clickItem, {
    click <- input$chart2_clickItem
    if (!is.null(click$label) && nchar(trimws(click$label)) > 0) {
      chambre_selectionnee(click$label)
    }
  })
  
  output$chart2_commentaire <- renderUI({
    df  <- data_chart2()
    sel <- chambre_selectionnee()
    row <- df[df$label == sel, ]
    if (nrow(row) == 0) return(NULL)
    
    texte <- paste0(
      row$pct_total, "% des hébergements proposent des chambres ",
      row$commentaire, ". ",
      "Parmi ceux-ci, ", row$pct_paris_within, "% sont situés à Paris",
      " et ", row$pct_hors_within, "% sont situés hors Paris."
    )
    
    div(class = "chart2-commentaire",
        tags$b(sel),
        tags$p(texte)
    )
  })
  
  # -------------------------------------------------------
  # Graphique 2
  # -------------------------------------------------------
  
  output$chart_2 <- rAmCharts::renderAmCharts({
    
    df  <- data_chart2()
    sel <- legende_selectionnee()
    
    # Selon la sélection : quelle barre et quel label afficher
    if (sel == "total") {
      vf1       <- "pct_paris_bar"
      vf2       <- "pct_hors_bar"
      fa1       <- 0.9
      fa2       <- 0.9
      label_txt <- "[[pct_total]]%"
      label_col <- "#071F32"
      show2     <- TRUE
    } else if (sel == "paris") {
      vf1       <- "pct_paris_abs"
      vf2       <- "label_top"      
      fa1       <- 0.9
      fa2       <- 0
      label_txt <- "[[pct_paris_abs]]%"
      label_col <- "#071F32"
      show2     <- FALSE
    } else {
      vf1       <- "label_top"      
      vf2       <- "pct_hors_abs"
      fa1       <- 0
      fa2       <- 0.9
      label_txt <- "[[pct_hors_abs]]%"
      label_col <- "#1aaa99"
      show2     <- TRUE
    }
    
    amSerialChart(
      categoryField = "label",
      dataProvider  = df
    ) %>%
      addGraph(
        type        = "column",
        valueField  = vf1,
        fillColors  = "#071F32",
        lineColor   = "#071F32",
        fillAlphas  = fa1,
        lineAlphas  = 0,
        title       = "Paris",
        balloonText = "<b>[[category]]</b><br>Total : [[pct_total]]%<br>dont Paris : [[pct_paris_within]]%  |  Hors Paris : [[pct_hors_within]]%",
      ) %>%
      addGraph(
        type        = "column",
        valueField  = vf2,
        fillColors  = "#25DCCC",
        lineColor   = "#25DCCC",
        fillAlphas  = fa2,
        lineAlphas  = 0,
        title       = "Hors Paris",
        balloonText = "<b>[[category]]</b><br>Total : [[pct_total]]%<br>dont Paris : [[pct_paris_within]]%  |  Hors Paris : [[pct_hors_within]]%"
      ) %>%
      addGraph(
        type            = "column",
        valueField      = "label_top",
        fillAlphas      = 0,
        lineAlphas      = 0,
        lineThickness   = 0,
        showBalloon     = TRUE,    
        balloonText     = "<b>[[category]]</b><br>Total : [[pct_total]]%<br>dont Paris : [[pct_paris_within]]%  |  Hors Paris : [[pct_hors_within]]%",
        labelText       = label_txt,
        labelPosition   = "top",
        fontSize        = 12,
        color           = label_col,
        boldLabels      = TRUE,
        visibleInLegend = FALSE
      ) %>%
      setCategoryAxis(
        gridPosition  = "start",
        axisAlpha     = 0,
        gridAlpha     = 0,
        labelRotation = 40
      ) %>%
      addValueAxis(
        gridColor  = "#eeeeee",
        gridAlpha  = 1,
        axisAlpha  = 0,
        unit       = "%",
        maximum    = 100,
        stackType  = "regular"
      ) %>%
      setChartCursor(
        cursorAlpha  = 0,
        oneBalloonOnly = TRUE    
      ) %>%
      setProperties(
        fontFamily = "Montserrat",
        fontSize   = 12
      ) %>%
      amOptions(legend = FALSE) %>%
      addListener(
        name = "clickGraphItem",
        expression = "function(e) {
        var ctx = e.item.dataContext;
        var lbl = ctx !== undefined && ctx.label !== undefined ? String(ctx.label) : '';
        if (lbl !== '') {
          Shiny.setInputValue(
            'chart2_clickItem',
            { label: lbl },
            { priority: 'event' }
          );
        }
      }"
      )
  })
  
  
  # -------------------------------------------------------
  # Combos ville / code_postal
  # -------------------------------------------------------
  combos <- data_hebergements %>%
    filter(!is.na(ville), !is.na(code_postal)) %>%
    mutate(
      combo_key = paste0(ville, " (", code_postal, ")"),
      code_norm = gsub(" ", "", code_postal)
    ) %>%
    group_by(combo_key, ville, code_postal, code_norm) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(ville)
  
  # -------------------------------------------------------
  # Filtre textuel
  # -------------------------------------------------------
  combos_filtrees <- reactive({
    query <- input$search_ville
    if (is.null(query) || trimws(query) == "") return(combos)
    
    q_norm <- tolower(gsub(" ", "", trimws(query)))
    
    combos %>%
      filter(
        grepl(q_norm, tolower(gsub(" ", "", ville)),       fixed = TRUE) |
          grepl(q_norm, tolower(gsub(" ", "", code_postal)), fixed = TRUE) |
          grepl(q_norm, tolower(gsub(" ", "", combo_key)),   fixed = TRUE)
      )
  })
  
  # -------------------------------------------------------
  # Rendu checkboxes
  # -------------------------------------------------------
  output$checkbox_villes <- renderUI({
    df <- combos_filtrees()
    
    if (nrow(df) == 0) {
      return(div(class = "no-result", "Aucun résultat"))
    }
    
    checkboxGroupInput(
      inputId      = "selected_villes",
      label        = NULL,
      choiceNames  = lapply(seq_len(nrow(df)), function(i) {
        tags$span(
          df$combo_key[i],
          tags$span(
            paste0("(", df$n[i], ")"),
            style = "font-size:11px; color:#888; margin-left:5px;"
          )
        )
      }),
      choiceValues = df$combo_key,
      selected     = input$selected_villes
    )
  })
  
  observeEvent(input$reset_filter, {
    updateCheckboxGroupInput(session, "selected_villes", selected = character(0))
    updateTextInput(session, "search_ville", value = "")
  })
  
  # -------------------------------------------------------
  # pin_icon défini UNE SEULE FOIS, hors de observe/reactive
  # -------------------------------------------------------
  pin_icon <- makeIcon(
    iconUrl     = "data:image/svg+xml;charset=utf-8,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 36'%3E%3Cpath d='M12 0C5.373 0 0 5.373 0 12c0 9 12 24 12 24S24 21 24 12C24 5.373 18.627 0 12 0z' fill='%23071F32' stroke='%2325DCCC' stroke-width='1.5'/%3E%3Ccircle cx='12' cy='12' r='5' fill='%2325DCCC'/%3E%3C/svg%3E",
    iconWidth   = 20, iconHeight   = 30,
    iconAnchorX = 10, iconAnchorY  = 30,
    popupAnchorX = 0, popupAnchorY = -30
  )
  
  # -------------------------------------------------------
  # Données filtrées
  # -------------------------------------------------------
  data_carte <- reactive({
    selected <- input$selected_villes
    if (is.null(selected) || length(selected) == 0) return(data_hebergements)
    
    data_hebergements %>%
      mutate(combo_key = paste0(ville, " (", code_postal, ")")) %>%
      filter(combo_key %in% selected)
  })
  
  # -------------------------------------------------------
  # Carte initiale
  # -------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(
        lng1 = min(data_hebergements$longitude, na.rm = TRUE),
        lat1 = min(data_hebergements$latitude,  na.rm = TRUE),
        lng2 = max(data_hebergements$longitude, na.rm = TRUE),
        lat2 = max(data_hebergements$latitude,  na.rm = TRUE)
      )
  })
  
  # -------------------------------------------------------
  # Mise à jour des marqueurs
  # -------------------------------------------------------
  observe({
    df <- data_carte()
    
    popups <- mapply(function(
    etablissement, adresse, code_postal, ville,
    lien_guide, parking, exterieurs, interieurs,
    services, personnel, chambres_adapees
    ) {
      paste0(
        "<div style='font-family:Montserrat,sans-serif; min-width:260px; max-width:320px; max-height:380px; overflow-y:auto; font-size:13px; line-height:1.6;'>",
        
        "<div style='background:#071F32; color:white; padding:10px 14px; border-radius:6px 6px 0 0; margin:-10px -10px 12px -10px; font-size:14px; font-weight:bold; word-wrap:break-word;'>",
        etablissement, "</div>",
        
        "<div style='margin-bottom:10px; color:#444;'>",
        adresse, "<br>",
        "<span style='color:#071F32; font-weight:bold;'>", code_postal, " ", ville, "</span>",
        "</div>",
        
        "<hr style='border:none; border-top:1px solid #F0F0F0; margin:8px 0;'>",
        
        "<div style='margin-bottom:8px;'>",
        "<span style='color:#071F32; font-weight:bold; text-transform:uppercase; font-size:11px; letter-spacing:0.5px;'>Guide</span><br>",
        html_lien(lien_guide),
        "</div>",
        
        "<hr style='border:none; border-top:1px solid #F0F0F0; margin:8px 0;'>",
        
        "<div style='margin-bottom:8px;'>",
        "<span style='color:#071F32; font-weight:bold; text-transform:uppercase; font-size:11px; letter-spacing:0.5px;'>Parking</span>",
        html_liste(parking), "</div>",
        
        "<div style='margin-bottom:8px;'>",
        "<span style='color:#071F32; font-weight:bold; text-transform:uppercase; font-size:11px; letter-spacing:0.5px;'>Exterieurs</span>",
        html_liste(exterieurs), "</div>",
        
        "<div style='margin-bottom:8px;'>",
        "<span style='color:#071F32; font-weight:bold; text-transform:uppercase; font-size:11px; letter-spacing:0.5px;'>Interieurs</span>",
        html_liste(interieurs), "</div>",
        
        "<div style='margin-bottom:8px;'>",
        "<span style='color:#071F32; font-weight:bold; text-transform:uppercase; font-size:11px; letter-spacing:0.5px;'>Services</span>",
        html_liste(services), "</div>",
        
        "<div style='margin-bottom:8px;'>",
        "<span style='color:#071F32; font-weight:bold; text-transform:uppercase; font-size:11px; letter-spacing:0.5px;'>Personnel</span>",
        html_liste(personnel), "</div>",
        
        "<div style='margin-bottom:4px;'>",
        "<span style='color:#071F32; font-weight:bold; text-transform:uppercase; font-size:11px; letter-spacing:0.5px;'>Chambres adaptees</span>",
        html_liste(chambres_adapees), "</div>",
        
        "</div>"
      )
    },
    df$etablissement, df$adresse, df$code_postal, df$ville,
    df$lien_guide, df$parking, df$exterieurs, df$interieurs,
    df$services, df$personnel, df$chambres_adapees,
    SIMPLIFY = FALSE   
    )
    
    popups <- unname(unlist(popups))   
    
    leafletProxy("map", data = df) %>%
      clearMarkers() %>%
      addMarkers(
        lng  = ~longitude,
        lat  = ~latitude,
        icon = pin_icon,
        
        label = ~lapply(paste0(
          "<div style='font-family:Montserrat,sans-serif; font-size:13px;'>",
          "<b>", etablissement, "</b><br>",
          "<span style='color:#ccc'>", adresse, "</span><br>",
          "<span style='color:#25DCCC'>", code_postal, " ", ville, "</span>",
          "</div>"
        ), htmltools::HTML),
        
        labelOptions = labelOptions(
          style = list(
            "background-color" = "#071F32",
            "color"            = "white",
            "border-radius"    = "8px",
            "padding"          = "8px 12px",
            "border"           = "1px solid #25DCCC",
            "font-family"      = "Montserrat, sans-serif",
            "font-size"        = "13px",
            "box-shadow"       = "2px 2px 6px rgba(0,0,0,0.4)"
          ),
          noHide    = FALSE,
          direction = "top"
        ),
        
        popup = popups
      )
  })
  
  # -------------------------------------------------------
  # Commentaire réactif zoom carte
  # -------------------------------------------------------
  
  observeEvent(input$map_bounds, {
    bounds <- input$map_bounds
    if (is.null(bounds)) return()
    
    df <- data_carte()
    
    n_visible <- df %>%
      filter(
        !is.na(latitude), !is.na(longitude),
        latitude  >= bounds$south & latitude  <= bounds$north,
        longitude >= bounds$west  & longitude <= bounds$east
      ) %>%
      nrow()
    
    output$carte_commentaire <- renderUI({
      div(class = "carte-commentaire",
          if (n_visible == 0) {
            "Aucun hébergement accessible dans la zone actuellement affichée."
          } else if (n_visible == 1) {
            "1 hébergement accessible visible dans la zone actuellement affichée."
          } else {
            paste0(n_visible,
                   " hébergements accessibles visibles dans la zone actuellement affichée.")
          }
      )
    })
  })
  
  # -------------------------------------------------------
  # Filtre localisation tableau
  # -------------------------------------------------------
  
  combos_filtrees_tableau <- reactive({
    query <- input$search_ville_tableau
    if (is.null(query) || trimws(query) == "") return(combos)
    q_norm <- tolower(gsub(" ", "", trimws(query)))
    combos %>%
      filter(
        grepl(q_norm, tolower(gsub(" ", "", ville)),       fixed = TRUE) |
          grepl(q_norm, tolower(gsub(" ", "", code_postal)), fixed = TRUE) |
          grepl(q_norm, tolower(gsub(" ", "", combo_key)),   fixed = TRUE)
      )
  })
  
  output$checkbox_villes_tableau <- renderUI({
    df <- combos_filtrees_tableau()
    if (nrow(df) == 0) return(div(class = "no-result", "Aucun résultat"))
    checkboxGroupInput(
      inputId      = "selected_villes_tableau",
      label        = NULL,
      choiceNames  = lapply(seq_len(nrow(df)), function(i) {
        tags$span(
          df$combo_key[i],
          tags$span(paste0("(", df$n[i], ")"),
                    style = "font-size:11px; color:#888; margin-left:5px;")
        )
      }),
      choiceValues = df$combo_key,
      selected     = input$selected_villes_tableau
    )
  })
  
  observeEvent(input$reset_filter_tableau, {
    updateCheckboxGroupInput(session, "selected_villes_tableau", selected = character(0))
    updateTextInput(session, "search_ville_tableau", value = "")
    updateCheckboxGroupInput(session, "filter_sourd",         selected = character(0))
    updateCheckboxGroupInput(session, "filter_aveugle",       selected = character(0))
    updateCheckboxGroupInput(session, "filter_communicantes", selected = character(0))
    updateCheckboxGroupInput(session, "filter_famille",       selected = character(0))
    updateCheckboxGroupInput(session, "filter_senior",        selected = character(0))
    updateCheckboxGroupInput(session, "filter_pmr",           selected = character(0))
  })
  
  # -------------------------------------------------------
  # Données filtrées tableau
  # -------------------------------------------------------
  
  filtre_oui_non <- function(df, col, filtre) {
    if (is.null(filtre) || length(filtre) == 0) return(df)
    keep_oui <- "oui" %in% filtre
    keep_non <- "non" %in% filtre
    if (keep_oui && keep_non) return(df)
    if (keep_oui) return(df %>% filter(.data[[col]] > 0))
    if (keep_non) return(df %>% filter(.data[[col]] == 0))
    return(df)
  }
  
  data_tableau_filtree <- reactive({
    df <- data_hebergements %>%
      mutate(combo_key = paste0(ville, " (", code_postal, ")"))
    
    # Filtre localisation
    selected <- input$selected_villes_tableau
    if (!is.null(selected) && length(selected) > 0) {
      df <- df %>% filter(combo_key %in% selected)
    }
    
    # Filtres oui/non
    df <- filtre_oui_non(df, "nb_chambre_sourd",         input$filter_sourd)
    df <- filtre_oui_non(df, "nb_chambre_aveugle",       input$filter_aveugle)
    df <- filtre_oui_non(df, "nb_chambres_communicantes",input$filter_communicantes)
    df <- filtre_oui_non(df, "nb_chambres_famille",      input$filter_famille)
    df <- filtre_oui_non(df, "nb_chambre_senior",        input$filter_senior)
    df <- filtre_oui_non(df, "nb_chambres_pmr",          input$filter_pmr)
    
    df
  })
  
  # -------------------------------------------------------
  # Pagination
  # -------------------------------------------------------
  
  page_courante <- reactiveVal(1)
  nb_par_page   <- 10
  
  # Reset page si filtre change
  observeEvent(data_tableau_filtree(), {
    page_courante(1)
  })
  
  observeEvent(input$prev_page, {
    if (page_courante() > 1) page_courante(page_courante() - 1)
  })
  
  observeEvent(input$next_page, {
    df    <- data_tableau_filtree()
    total <- ceiling(nrow(df) / nb_par_page)
    if (page_courante() < total) page_courante(page_courante() + 1)
  })
  
  # -------------------------------------------------------
  # Rendu tableau
  # -------------------------------------------------------
  
  output$tableau_info <- renderText({
    df    <- data_tableau_filtree()
    n     <- nrow(df)
    page  <- page_courante()
    debut <- (page - 1) * nb_par_page + 1
    fin   <- min(page * nb_par_page, n)
    if (n == 0) return("Aucun résultat")
    paste0(debut, " – ", fin, " sur ", n, " établissements")
  })
  
  output$tableau <- renderTable({
    df   <- data_tableau_filtree()
    page <- page_courante()
    debut <- (page - 1) * nb_par_page + 1
    fin   <- min(page * nb_par_page, nrow(df))
    
    if (nrow(df) == 0) return(data.frame(Message = "Aucun résultat"))
    
    df %>%
      slice(debut:fin) %>%
      mutate(ville_cp = paste0(ville, "\n", code_postal)) %>%
      select(
        "Etablissement"          = etablissement,
        "Ville"                  = ville_cp,
        "Adresse"                = adresse,
        "Parking"                = parking_nombre_de_places,
        "Ch. sourds"             = nb_chambre_sourd,
        "Ch. aveugles"           = nb_chambre_aveugle,
        "Ch. communicantes"      = nb_chambres_communicantes,
        "Ch. famille"            = nb_chambres_famille,
        "Ch. senior"             = nb_chambre_senior,
        "Ch. PMR"                = nb_chambres_pmr
      )
  }, striped = FALSE, hover = TRUE, bordered = FALSE, spacing = "m",
  width = "100%", align = "l")
  
}