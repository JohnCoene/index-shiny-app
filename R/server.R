#' Application Server
#' 
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' 
#' @importFrom grDevices col2rgb
#' @importFrom stats na.omit quantile
#' 
#' @keywords internal
server <- function(input, output, session) {

  # Create labels, in case user has filtered LAs
  labels <-
    paste0(
      sprintf("<strong>%s</strong><br/>", .globals$ri_shp$lad19nm),
      "Vulnerability quintile (5 = highest vulnerability): ",
      .globals$ri_shp$`Vulnerability quintile`,
      "<br/>",
      "Capacity quintile (5 = lowest capacity): ",
      .globals$ri_shp$`Capacity quintile`
    ) %>%
    lapply(htmltools::HTML)

  # start guide
  guide()

  # ---- Functions and variables for the server ----
  vi_pal <- colorFactor("viridis", c(1:10), reverse = TRUE)

  selected_polygon <- reactiveVal() # track which LA the user clicked on
  selected_msoa <- reactiveVal() # track if user clicked an MSOA

  cols_to_format <- reactiveVal() # list of data table columns to format in `renderDT()`

  # Set up a waiter for the map
  map_waiter <- Waiter$new(
    id = "waiter-content",
    color = "white"
  )

  # ---- Draw basemap ----
  # set up the static parts of the map (that don't change as user selects different options)
  output$map <- renderLeaflet({
    leaflet(
      .globals$ri_shp,
      options = leafletOptions(
        minZoom = 5, 
        maxZoom = 15, 
        attributionControl = FALSE
      )
    ) %>%
      setView(
        lat = 54.00366, 
        lng = -2.547855, 
        zoom = 7
      ) %>% # centre map on Whitendale Hanging Stones, the centre of GB: https://en.wikipedia.org/wiki/Centre_points_of_the_United_Kingdom
      addProviderTiles(providers$CartoDB.Positron) %>%

      # Show filtered Local Authorities
      addPolygons(
        # Use the layerID to observe click-events and update plots
        layerId = ~lad19cd,
        group = "Vulnerability vs. Resilience",
        fillColor = ~fill,
        weight = 0.7,
        opacity = 0.8,
        color = "black",
        dashArray = "0.1",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addEasyButton(easyButton(
        icon = "fa-globe",
        title = "Reset zoom level",
        onClick = JS("function(btn, map){ map.setZoom(6); }")
      ))
  })

  # ---- Map filters ----
  filteredLAs <- reactive({
    output_shp <- .globals$ri_shp # Set up object to return

    if (is.null(input$sidebarItemExpanded)) {
      return(output_shp)
    }

    # - Filter based on type of resilience selected -
    if (input$sidebarItemExpanded == "DisastersandEmergencies") {
      if (input$shocks == "None") {
        output_shp <- .globals$ri_shp
      } else if (input$shocks == "Floods") {
        if (input$highest_flood_risks & !input$flood_incidents) {
          # Show areas with highest flood risks but not historical incidents
          output_shp <- .globals$ri_shp %>% filter(`Flood incidents quintile` == 5)
        } else if (!input$highest_flood_risks & !input$flood_incidents) {
          # Show areas with any flood risk but not historical incidents
          output_shp <- .globals$ri_shp %>% filter(!is.na(`Flood incidents quintile`))
        } else if (input$highest_flood_risks & input$flood_incidents) {
          # Show areas with highest floods risk and/or historical incidents
          output_shp <- .globals$ri_shp %>% filter(`Flood incidents quintile` == 5 | !is.na(`Total historical flooding incidents`))
        } else if (!input$highest_flood_risks & input$flood_incidents) {
          # Show areas with any floods ris and/or historical incidents
          output_shp <- .globals$ri_shp %>% filter(!is.na(`Flood incidents quintile`) | !is.na(`Total historical flooding incidents`))
        }
      } else if (input$shocks == "Dwelling fires") {
        output_shp <- .globals$ri_shp %>% filter(`Fire incidents quintile` == 5)
      }
    } else if (input$sidebarItemExpanded == "HealthInequalities") {
      output_shp <- .globals$ri_shp
    } else if (input$sidebarItemExpanded == "MigrationandDisplacement") {
      output_shp <- .globals$ri_shp
    }

    # - Filter only least resilient areas? -
    if (input$filter_least_resilient) {
      output_shp <- output_shp %>%
        filter(group == "3 - 3")
    }

    output_shp
  })

  # Track which polygon the user clicked on
  observeEvent(input$map_shape_click, {
    if (is.null(input$map_shape_click$id)) {
      selected_polygon(NULL)

      # Hide the right-hand sidebar with VI indicators (if it was open)
      shinyjs::removeClass(selector = "body", class = "control-sidebar-open")
    } else if (str_detect(input$map_shape_click$id, "^E02")) {
      # User selected an MSOA - show VI
      selected_msoa(input$map_shape_click$id)

      # Show the right-hand sidebar with VI indicators
      shinyjs::addClass(selector = "body", class = "control-sidebar-open")
    } else {
      selected_polygon(input$map_shape_click$id)

      # Get name from selected LA code
      curr_lad <- .globals$hp$lad19nm[.globals$lad_shp$lad19cd == input$map_shape_click$id]

      # Show clicked LA name in the select box
      updateSelectInput(session, "lad", selected = curr_lad)

      # Hide the right-hand sidebar with VI indicators (if it was open)
      shinyjs::removeClass(selector = "body", class = "control-sidebar-open")
    }
  })

  # ---- Reset view if user changes disasters/shocks ----
  observeEvent(input$shocks, {
    updateSelectInput(session, "lad", selected = "- Show all Local Authorities -")
  })

  observeEvent(input$highest_flood_risks, {
    updateSelectInput(session, "lad", selected = "- Show all Local Authorities -")
  })

  observeEvent(input$flood_incidents, {
    updateSelectInput(session, "lad", selected = "- Show all Local Authorities -")
  })

  observeEvent(input$lad, {
    # Hide the right-hand sidebar with VI indicators (if it was open)
    shinyjs::removeClass(selector = "body", class = "control-sidebar-open")

    # - Filter based on user-selected LAs from list -
    if (input$lad == "- Show all Local Authorities -") {
      # Deselect LAs
      selected_polygon(NULL)
    } else {
      # update map zoom
      selected_polygon(.globals$lad_shp$lad19cd[.globals$lad_shp$lad19nm == input$lad])
    }
  })

  # ---- Observer for updating map ----
  observe({
    # Debug
    # print(input$sidebarItemExpanded)
    # print(input$shocks)
    # print(nrow(filteredLAs()))
    # print(selected_polygon())
    # print(input$map_shape_click$id)

    map_waiter$show()
    on.exit({
      map_waiter$hide()
    })

    # Get selected set of LAs
    curr_LAs <- filteredLAs()

    # Re-create labels, in case user has filtered LAs
    labels <-
      paste0(
        sprintf("<strong>%s</strong><br/>", curr_LAs$lad19nm),
        "Vulnerability quintile (5 = highest vulnerability): ",
        curr_LAs$`Vulnerability quintile`,
        "<br/>",
        "Capacity quintile (5 = lowest capacity): ",
        curr_LAs$`Capacity quintile`
      ) %>%
      lapply(htmltools::HTML)

    map <- leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%

      # Show filtered Local Authorities
      addPolygons(
        data = curr_LAs,
        # Use the layerID to observe click-events and update plots
        layerId = ~lad19cd,
        group = "Vulnerability vs. Resilience",
        fillColor = ~fill,
        weight = 0.7,
        opacity = 0.8,
        color = "black",
        dashArray = "0.1",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )

    # If user clicks a Local Authority, zoom to it and show vulnerable MSOAs
    if (!is.null(selected_polygon())) {
      # Get vulnerable MSOAs for current LA
      vi_curr <- .globals$vi_shp %>%
        filter(LAD19CD == selected_polygon())

      # Get bounding box of current LA for zooming
      curr_bbox <- st_bbox(vi_curr)

      map <- map %>%
        addPolygons(
          data = vi_curr,
          group = "VI",
          layerId = ~MSOA11CD,
          fillColor = ~ vi_pal(`Vulnerability decile`),
          fillOpacity = 0.8,
          color = "white",
          weight = 0.7,
          popup = ~ paste(
            "<b>",
            Name_clean,
            "</b><br/><br/>",
            "Overall vulnerability (10 = worst): ",
            `Vulnerability decile`,
            "<br/>",
            "Clinical vulnerability: ",
            `Clinical Vulnerability decile`,
            "<br/>",
            "Health/wellbeing vulnerability: ",
            `Health/Wellbeing Vulnerability decile`,
            "<br/>",
            "Socioeconomic vulnerability: ",
            `Socioeconomic Vulnerability decile`,
            "<br/>"
          )
        ) %>%
        addLegend_decreasing(
          data = .globals$vi_shp,
          position = "bottomright",
          pal = vi_pal,
          values = ~`Vulnerability decile`,
          title = paste0("Vulnerability", tags$br(), " (10 = most vulnerable)"),
          opacity = 0.8,
          decreasing = TRUE
        ) %>%
        flyToBounds(
          lng1 = as.numeric(curr_bbox["xmin"]),
          lat1 = as.numeric(curr_bbox["ymin"]),
          lng2 = as.numeric(curr_bbox["xmax"]),
          lat2 = as.numeric(curr_bbox["ymax"])
        )
    } else {
      # Get bounding box of current LAs and zoom to it
      curr_bbox <- st_bbox(curr_LAs)

      map <- map %>%
        flyToBounds(
          lng1 = as.numeric(curr_bbox["xmin"]),
          lat1 = as.numeric(curr_bbox["ymin"]),
          lng2 = as.numeric(curr_bbox["xmax"]),
          lat2 = as.numeric(curr_bbox["ymax"])
        )
    }

    map
  })

  # ---- Data ----
  filteredData <- reactive({
    if (is.null(input$sidebarItemExpanded)) {
      cols_to_format(c("Extent of population living in highly vulnerable areas"))

      return(
        .globals$ri %>%
          select(
            `LA code` = LAD19CD,
            `LA name` = LAD19NM,
            `Capacity rank`,
            `Capacity quintile`,
            `Vulnerability rank`,
            `Vulnerability quintile`,
            `Extent of population living in highly vulnerable areas`
          )
      )
    }

    # - Filter based on type of resilience selected -
    if (input$sidebarItemExpanded == "DisastersandEmergencies") {
      if (input$shocks == "None") {
        cols_to_format(c("Extent of population living in highly vulnerable areas"))

        .globals$ri %>%
          select(
            `LA code` = LAD19CD,
            `LA name` = LAD19NM,
            `Capacity rank`,
            `Capacity quintile`,
            `Vulnerability rank`,
            `Vulnerability quintile`,
            `Extent of population living in highly vulnerable areas`
          )
      } else if (input$shocks == "Floods") {
        if (input$highest_flood_risks & !input$flood_incidents) {
          cols_to_format(c("Extent of population living in highly vulnerable areas", "% people in flood risk areas"))

          # Show areas with highest flood risks but not historical incidents
          .globals$ri %>%
            filter(`Flood incidents quintile` == 5) %>%
            select(
              `LA code` = LAD19CD,
              `LA name` = LAD19NM,
              `Capacity rank`,
              `Capacity quintile`,
              `Vulnerability rank`,
              `Vulnerability quintile`,
              `Extent of population living in highly vulnerable areas`,
              `Flood risk quintile`,
              `% people in flood risk areas`
            )
        } else if (!input$highest_flood_risks & !input$flood_incidents) {
          cols_to_format(c("Extent of population living in highly vulnerable areas", "% people in flood risk areas"))

          # Show areas with any flood risk but not historical incidents
          .globals$ri %>%
            filter(!is.na(`Flood incidents quintile`)) %>%
            select(
              `LA code` = LAD19CD,
              `LA name` = LAD19NM,
              `Capacity rank`,
              `Capacity quintile`,
              `Vulnerability rank`,
              `Vulnerability quintile`,
              `Extent of population living in highly vulnerable areas`,
              `Flood risk quintile`,
              `% people in flood risk areas`
            )
        } else if (input$highest_flood_risks & input$flood_incidents) {
          cols_to_format(c("Extent of population living in highly vulnerable areas", "% people in flood risk areas"))

          # Show areas with highest floods risk and/or historical incidents
          .globals$ri %>%
            filter(`Flood incidents quintile` == 5 | !is.na(`Total historical flooding incidents`)) %>%
            mutate(
              `Flooding incidents per 10,000 people` = round(`Flooding incidents per 10,000 people`, 1),
              `Total historical flooding incidents` = round(`Total historical flooding incidents`, 1)
            ) %>%
            select(
              `LA code` = LAD19CD,
              `LA name` = LAD19NM,
              `Capacity rank`,
              `Capacity quintile`,
              `Vulnerability rank`,
              `Vulnerability quintile`,
              `Extent of population living in highly vulnerable areas`,
              `Flood risk quintile`,
              `% people in flood risk areas`,
              `Flooding incidents per 10,000 people`,
              `Total historical flooding incidents`
            )
        } else if (!input$highest_flood_risks & input$flood_incidents) {
          cols_to_format(c("Extent of population living in highly vulnerable areas", "% people in flood risk areas"))

          # Show areas with any floods ris and/or historical incidents
          .globals$ri %>%
            filter(!is.na(`Flood incidents quintile`) | !is.na(`Total historical flooding incidents`)) %>%
            mutate(
              `Flooding incidents per 10,000 people` = round(`Flooding incidents per 10,000 people`, 1),
              `Total historical flooding incidents` = round(`Total historical flooding incidents`, 1)
            ) %>%
            select(
              `LA code` = LAD19CD,
              `LA name` = LAD19NM,
              `Capacity rank`,
              `Capacity quintile`,
              `Vulnerability rank`,
              `Vulnerability quintile`,
              `Extent of population living in highly vulnerable areas`,
              `Flood risk quintile`,
              `% people in flood risk areas`,
              `Flooding incidents per 10,000 people`,
              `Total historical flooding incidents`
            )
        } # end if for flooding
      } else if (input$shocks == "Dwelling fires") {
        cols_to_format(c("Extent of population living in highly vulnerable areas"))

        .globals$ri %>%
          filter(`Fire incidents quintile` == 5) %>%
          mutate(
            `Dwelling fire incidents per 10,000 people` = round(`Dwelling fire incidents per 10,000 people`, 1),
            `Total dwelling fires (three-year average)` = round(`Total dwelling fires (three-year average)`, 1)
          ) %>%
          select(
            `LA code` = LAD19CD,
            `LA name` = LAD19NM,
            `Capacity rank`,
            `Capacity quintile`,
            `Vulnerability rank`,
            `Vulnerability quintile`,
            `Extent of population living in highly vulnerable areas`,
            `Fire incidents quintile`,
            `Dwelling fire incidents per 10,000 people`,
            `Total dwelling fires (three-year average)`
          )
      } # end if for shocks
    } else if (input$sidebarItemExpanded == "HealthInequalities") {
      cols_to_format(c("Extent of population living in highly vulnerable areas"))

      .globals$ri %>%
        select(
          `LA code` = LAD19CD,
          `LA name` = LAD19NM,
          `Capacity rank`,
          `Capacity quintile`,
          `Vulnerability rank`,
          `Vulnerability quintile`,
          `Extent of population living in highly vulnerable areas`
        )
    } else if (input$sidebarItemExpanded == "MigrationandDisplacement") {
      cols_to_format(c("Extent of population living in highly vulnerable areas"))

      .globals$ri %>%
        select(
          `LA code` = LAD19CD,
          `LA name` = LAD19NM,
          `Capacity rank`,
          `Capacity quintile`,
          `Vulnerability rank`,
          `Vulnerability quintile`,
          `Extent of population living in highly vulnerable areas`
        )
    }
  })

  output$data <- renderDT(
    datatable(
      filteredData(),
      rownames = FALSE,
      escape = FALSE,
      extensions = c("Buttons", "ColReorder"),
      options = list(
        dom = "Bfrtip",
        buttons = c("csv", "excel", "colvis"),
        colReorder = TRUE,
        scrollX = TRUE
      )
    ) %>%
      formatPercentage(columns = cols_to_format(), digits = 1)
  )

  # ---- Vulnerability Index underlying indicators ----
  # Clinical vulnerability
  output$vi_clinical <- renderUI({
    if (is.null(selected_msoa())) {
      return("Select a Local Authority then click a neighbourhood to see the underlying indicators.")
    }

    # Get vulnerable MSOAs for current LA
    vi_curr <- vi %>%
      filter(Code == selected_msoa())

    str_stats <- paste0("<strong>", vi_curr$Name_clean, "</strong>")

    if (!is.na(vi_curr$`Modelled prevalence of people aged 15 who are regular smokers Rate`)) {
      str_stats <- c(str_stats, paste0("Smoking prevalence: ", round(vi_curr$`Modelled prevalence of people aged 15 who are regular smokers Rate`, 2), "<br/>(England average: ", round(mean(vi$`Modelled prevalence of people aged 15 who are regular smokers Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Obesity prevalence Rate`)) {
      str_stats <- c(str_stats, paste0("Obesity prevalence: ", round(vi_curr$`Obesity prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Obesity prevalence Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Cancer prevalence Rate`)) {
      str_stats <- c(str_stats, paste0("Cancer prevalence: ", round(vi_curr$`Cancer prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Cancer prevalence Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Asthma prevalence Rate`)) {
      str_stats <- c(str_stats, paste0("Asthma prevalence: ", round(vi_curr$`Asthma prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Asthma prevalence Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Atrial Fibrillation prevalence Rate`)) {
      str_stats <- c(str_stats, paste0("Atrial Fibrillation prevalence: ", round(vi_curr$`Atrial Fibrillation prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Atrial Fibrillation prevalence Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Cardiovascular Disease prevalence Rate`)) {
      str_stats <- c(str_stats, paste0("Cardiovascular Disease prevalence: ", round(vi_curr$`Cardiovascular Disease prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Cardiovascular Disease prevalence Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`COPD prevalence Rate`)) {
      str_stats <- c(str_stats, paste0("COPD prevalence: ", round(vi_curr$`COPD prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`COPD prevalence Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Diabetes prevalence Rate`)) {
      str_stats <- c(str_stats, paste0("Diabetes prevalence: ", round(vi_curr$`Diabetes prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Diabetes prevalence Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Coronary Heart Disease prevalence Rate`)) {
      str_stats <- c(str_stats, paste0("Coronary Heart Disease prevalence: ", round(vi_curr$`Coronary Heart Disease prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Coronary Heart Disease prevalence Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Heart Failure prevalence Rate`)) {
      str_stats <- c(str_stats, paste0("Heart Failure prevalence: ", round(vi_curr$`Heart Failure prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Heart Failure prevalence Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`High Blood Pressure prevalence Rate`)) {
      str_stats <- c(str_stats, paste0("High Blood Pressure prevalence: ", round(vi_curr$`High Blood Pressure prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`High Blood Pressure prevalence Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Chronic Kidney Disease prevalence Rate`)) {
      str_stats <- c(str_stats, paste0("Chronic Kidney Disease prevalence: ", round(vi_curr$`Chronic Kidney Disease prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Chronic Kidney Disease prevalence Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Peripheral Arterial Disease prevalence Rate`)) {
      str_stats <- c(str_stats, paste0("Peripheral Arterial Disease prevalence: ", round(vi_curr$`Peripheral Arterial Disease prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Peripheral Arterial Disease prevalence Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Proportion people over 70`)) {
      str_stats <- c(str_stats, paste0("Proportion of people over 70: ", round(vi_curr$`Proportion people over 70`, 2), "<br/>(England average: ", round(mean(vi$`Proportion people over 70`, na.rm = TRUE), 2), ")"))
    }

    HTML(paste(str_stats, collapse = "<br/><br/>"))
  })

  # Health/wellbeing vulnerability
  output$vi_wellbeing <- renderUI({
    if (is.null(selected_msoa())) {
      return("Select a Local Authority then click a neighbourhood to see the underlying indicators.")
    }

    # Get vulnerable MSOAs for current LA
    vi_curr <- vi %>%
      filter(Code == selected_msoa())

    str_stats <- paste0("<strong>", vi_curr$Name_clean, "</strong>")

    if (!is.na(vi_curr$`Dementia prevalence Rate`)) {
      str_stats <- c(str_stats, paste0("Dementia prevalence: ", round(vi_curr$`Dementia prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Dementia prevalence Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Learning Disabilities prevalence Rate`)) {
      str_stats <- c(str_stats, paste0("Learning Disabilities prevalence: ", round(vi_curr$`Learning Disabilities prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Learning Disabilities prevalence Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Serious Mental Illness prevalence Rate`)) {
      str_stats <- c(str_stats, paste0("Serious Mental Illness prevalence: ", round(vi_curr$`Serious Mental Illness prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Serious Mental Illness prevalence Rate`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$Loneliness)) {
      str_stats <- c(str_stats, paste0("Loneliness score: ", round(vi_curr$Loneliness, 2), "<br/>(England average: ", round(mean(vi$Loneliness, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Frailty rank`)) {
      str_stats <- c(str_stats, paste0("Frailty (rank out of ", max(vi$`Frailty rank`), "): ", round(vi_curr$`Frailty rank`, 2)))
    }

    if (!is.na(vi_curr$`Percentage of adresses with private outdoor space (reverse ranked)`)) {
      str_stats <- c(str_stats, paste0("Percentage of adresses with private outdoor space (rank out of ", max(vi$`Percentage of adresses with private outdoor space (reverse ranked)`), "): ", round(vi_curr$`Percentage of adresses with private outdoor space (reverse ranked)`, 2)))
    }

    if (!is.na(vi_curr$`Average distance to nearest Park, Public Garden, or Playing Field (m)`)) {
      str_stats <- c(str_stats, paste0("Average distance to green spaces: ", round(vi_curr$`Average distance to nearest Park, Public Garden, or Playing Field (m)`, 2), "m<br/>(England average: ", round(mean(vi$`Average distance to nearest Park, Public Garden, or Playing Field (m)`, na.rm = TRUE), 2), "m)"))
    }

    HTML(paste(str_stats, collapse = "<br/><br/>"))
  })

  # Economic vulnerability
  output$vi_economic <- renderUI({
    if (is.null(selected_msoa())) {
      return("Select a Local Authority then click a neighbourhood to see the underlying indicators.")
    }

    # Get vulnerable MSOAs for current LA
    vi_curr <- vi %>%
      filter(Code == selected_msoa())

    str_stats <- paste0("<strong>", vi_curr$Name_clean, "</strong>")

    if (!is.na(vi_curr$`Older people social care benefit (Attendance Allowance) Rate`)) {
      str_stats <- c(str_stats, paste0("Older people social care benefit: ", round(vi_curr$`Older people social care benefit (Attendance Allowance) Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Older people social care benefit (Attendance Allowance) Rate`, na.rm = TRUE), 2), "%)"))
    }

    if (!is.na(vi_curr$`Employment and Support Allowance claimants, disease code nervous system Rate`)) {
      str_stats <- c(str_stats, paste0("ESA claimants - disease code nervous system: ", round(vi_curr$`Employment and Support Allowance claimants, disease code nervous system Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Employment and Support Allowance claimants, disease code nervous system Rate`, na.rm = TRUE), 2), "%)"))
    }

    if (!is.na(vi_curr$`Employment and Support Allowance claimants, disease code respiratory or circulatory Rate`)) {
      str_stats <- c(str_stats, paste0("ESA claimants - disease code respiratory or circulatory: ", round(vi_curr$`Employment and Support Allowance claimants, disease code respiratory or circulatory Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Employment and Support Allowance claimants, disease code respiratory or circulatory Rate`, na.rm = TRUE), 2), "%)"))
    }

    if (!is.na(vi_curr$`People receiving Disability Benefits Rate`)) {
      str_stats <- c(str_stats, paste0("People receiving Disability Benefits: ", round(vi_curr$`People receiving Disability Benefits Rate`, 2), "%<br/>(England average: ", round(mean(vi$`People receiving Disability Benefits Rate`, na.rm = TRUE), 2), "%)"))
    }

    if (!is.na(vi_curr$`Personal Independence Payment (PIP), respiratory disease claimants Rate`)) {
      str_stats <- c(str_stats, paste0("PIP claimants: ", round(vi_curr$`Personal Independence Payment (PIP), respiratory disease claimants Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Personal Independence Payment (PIP), respiratory disease claimants Rate`, na.rm = TRUE), 2), "%)"))
    }

    if (!is.na(vi_curr$`Households on Universal Credit - Limited Capability for Work Entitlement Rate`)) {
      str_stats <- c(str_stats, paste0("Households on Universal Credit - Limited Capability for Work Entitlement: ", round(vi_curr$`Households on Universal Credit - Limited Capability for Work Entitlement Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Households on Universal Credit - Limited Capability for Work Entitlement Rate`, na.rm = TRUE), 2), "%)"))
    }

    if (!is.na(vi_curr$`Universal Credit claimants - Conditionality Regime: No work requirements Rate`)) {
      str_stats <- c(str_stats, paste0("Universal Credit claimants - No work requirements: ", round(vi_curr$`Universal Credit claimants - Conditionality Regime: No work requirements Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Universal Credit claimants - Conditionality Regime: No work requirements Rate`, na.rm = TRUE), 2), "%)"))
    }

    if (!is.na(vi_curr$`Jobs in accommodation and food services (hospitality) Rate`)) {
      str_stats <- c(str_stats, paste0("Jobs in accommodation and hospitality: ", round(vi_curr$`Jobs in accommodation and food services (hospitality) Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Jobs in accommodation and food services (hospitality) Rate`, na.rm = TRUE), 2), "%)"))
    }

    if (!is.na(vi_curr$`Jobs in arts, entertainment, recreation and other services Rate`)) {
      str_stats <- c(str_stats, paste0("Jobs in arts, entertainment, recreation: ", round(vi_curr$`Jobs in arts, entertainment, recreation and other services Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Jobs in arts, entertainment, recreation and other services Rate`, na.rm = TRUE), 2), "%)"))
    }

    if (!is.na(vi_curr$`Jobs in retail Rate`)) {
      str_stats <- c(str_stats, paste0("Jobs in retail: ", round(vi_curr$`Jobs in retail Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Jobs in retail Rate`, na.rm = TRUE), 2), "%)"))
    }

    if (!is.na(vi_curr$`Jobs in transport and storage (inc postal) Rate`)) {
      str_stats <- c(str_stats, paste0("Jobs in transport and storage: ", round(vi_curr$`Jobs in transport and storage (inc postal) Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Jobs in transport and storage (inc postal) Rate`, na.rm = TRUE), 2), "%)"))
    }

    if (!is.na(vi_curr$`Financial Vulnerability score`)) {
      str_stats <- c(str_stats, paste0("Financial Vulnerability score: ", round(vi_curr$`Financial Vulnerability score`, 2), "<br/>(England average: ", round(mean(vi$`Financial Vulnerability score`, na.rm = TRUE), 2), ")"))
    }

    HTML(paste(str_stats, collapse = "<br/><br/>"))
  })

  # Social vulnerability
  output$vi_social <- renderUI({
    if (is.null(selected_msoa())) {
      return("Select a Local Authority then click a neighbourhood to see the underlying indicators.")
    }

    # Get vulnerable MSOAs for current LA
    vi_curr <- vi %>%
      filter(Code == selected_msoa())

    str_stats <- paste0("<strong>", vi_curr$Name_clean, "</strong>")

    if (!is.na(vi_curr$`Longest distance to supermarket (km)`)) {
      str_stats <- c(str_stats, paste0("Distance to supermarket: ", round(vi_curr$`Longest distance to supermarket (km)`, 2), "km<br/>(England average: ", round(mean(vi$`Longest distance to supermarket (km)`, na.rm = TRUE), 2), "km)"))
    }

    if (!is.na(vi_curr$`Longest distance to GP surgery (km)`)) {
      str_stats <- c(str_stats, paste0("Distance to GP surgery: ", round(vi_curr$`Longest distance to GP surgery (km)`, 2), "km<br/>(England average: ", round(mean(vi$`Longest distance to GP surgery (km)`, na.rm = TRUE), 2), "km)"))
    }

    if (!is.na(vi_curr$`Longest distance to Post Office (km)`)) {
      str_stats <- c(str_stats, paste0("Distance to Post Office: ", round(vi_curr$`Longest distance to Post Office (km)`, 2), "km<br/>(England average: ", round(mean(vi$`Longest distance to Post Office (km)`, na.rm = TRUE), 2), "km)"))
    }

    if (!is.na(vi_curr$`Longest distance to hospital (km)`)) {
      str_stats <- c(str_stats, paste0("Distance to hospital: ", round(vi_curr$`Longest distance to hospital (km)`, 2), "km<br/>(England average: ", round(mean(vi$`Longest distance to hospital (km)`, na.rm = TRUE), 2), "km)"))
    }

    if (!is.na(vi_curr$`Longest distance to food bank (km)`)) {
      str_stats <- c(str_stats, paste0("Distance to food bank: ", round(vi_curr$`Longest distance to food bank (km)`, 2), "km<br/>(England average: ", round(mean(vi$`Longest distance to food bank (km)`, na.rm = TRUE), 2), "km)"))
    }

    if (!is.na(vi_curr$`Household overcrowding`)) {
      str_stats <- c(str_stats, paste0("Household overcrowding: ", round(vi_curr$`Household overcrowding`, 2), "<br/>(England average: ", round(mean(vi$`Household overcrowding`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$Homelessness)) {
      str_stats <- c(str_stats, paste0("Homelessness rate: ", round(vi_curr$Homelessness, 2), "<br/>(England average: ", round(mean(vi$Homelessness, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Housing in poor condition`)) {
      str_stats <- c(str_stats, paste0("Housing in poor condition: ", round(vi_curr$`Housing in poor condition`, 2), "<br/>(England average: ", round(mean(vi$`Housing in poor condition`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Houses without central heating`)) {
      str_stats <- c(str_stats, paste0("Houses without central heating: ", round(vi_curr$`Houses without central heating`, 2), "<br/>(England average: ", round(mean(vi$`Houses without central heating`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Air quality`)) {
      str_stats <- c(str_stats, paste0("Air quality: ", round(vi_curr$`Air quality`, 2), "<br/>(England average: ", round(mean(vi$`Air quality`, na.rm = TRUE), 2), ")"))
    }

    if (!is.na(vi_curr$`Digital Vulnerability rank`)) {
      str_stats <- c(str_stats, paste0("Digital Vulnerability (rank out of ", max(vi$`Digital Vulnerability rank`), "): ", round(vi_curr$`Digital Vulnerability rank`, 2)))
    }

    HTML(paste(str_stats, collapse = "<br/><br/>"))
  })

  # ---- Download buttons ----
  # VI download button on help tab
  output$downloadVI1 <- downloadHandler(
    filename = "vulnerability-index.csv",
    content = function(con) {
      write_csv(.globals$vi, con)
    }
  )

  # VI download button on data tab
  output$downloadVI2 <- downloadHandler(
    filename = "vulnerability-index.csv",
    content = function(con) {
      write_csv(.globals$vi, con)
    }
  )

  # RI download button on help tab
  output$downloadRI1 <- downloadHandler(
    filename = "resilience-index.csv",
    content = function(con) {
      write_csv(.globals$ri, con)
    }
  )

  # RI download button on data tab
  output$downloadRI2 <- downloadHandler(
    filename = "resilience-index.csv",
    content = function(con) {
      write_csv(.globals$ri, con)
    }
  )

  # ---- Button to (re-)show interactive guide ----
  observeEvent(input$guide, {
    guide$start()
  })

  # - Error messages -
  sever(
    html = disconnected,
    bg_color = "white",
    color = "black" 
  )
}
