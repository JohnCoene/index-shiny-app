#' @importFrom cicerone use_cicerone
ui <- function(req){

  body_colwise <- dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "assets/styles.css"
      ),
      tags$script(
        src = "assets/section.js"
      ),
      tags$script(
        src = "assets/sidebar.js"
      )
    ),
    # - Error and waiting functions to improve UX -
    use_sever(),
    use_waiter(),
    use_cicerone(),
    useShinyjs(),
    tags$style(
      type = "text/css", 
      "#map {height: calc(100vh - 200px) !important;}"
    ),
    # import custom JavaScript for Waiter
    # Load custom theme
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    fluidRow(
      tabBox(
        width = NULL,
        side = "left",
        title = NULL,
        tabPanel(
          "Map",
          id = "map-tab",
          icon = icon(name = "globe"),
          leafletOutput("map", height = "100%"),
          absolutePanel(
            id = "legend",
            class = "panel panel-default",
            top = "auto",
            bottom = 50,
            right = "auto",
            width = 225,
            fixed = FALSE,
            draggable = FALSE,
            height = "auto",
            img(
              src = "assets/bivar-legend.png", 
              width = 300
            )
          )
        ),
        tabPanel(
          "Data",
          icon = icon(name = "table"),
          div(
            style = "overflow-y:scroll; height: calc(100vh - 200px) !important;",
            fluidRow(
              id = "data-info",
              p(
                "Explore English local authority vulnerability and capacity scores in the table below.
                        Scores are given as ranks, quintiles and deciles - in all cases, higher numbers mean",
                tags$b("higher"),
                " vulnerability and a ",
                tags$b("lower"),
                " capacity. To learn more about the British Red Cross Vulnerability Index and how these scores are calculated,",
                tags$a(
                  href = "https://github.com/britishredcrosssociety/covid-19-vulnerability/", 
                  target = "_blank", 
                  "click here."
                )
              ),
              downloadButton(
                "downloadVI2", 
                "Download Vulnerability Index data"
              ),
              downloadButton(
                "downloadRI2", 
                "Download Resilience Index data"
              ),
              hr()
            ),
            fluidRow(
              id = "data-infographic-row",
              img(
                src = "data-info-download.png",
                height = 90,
                style = "margin-left:30px;margin-bottom:-8px;"
              ),
              img(
                src = "data-info-hide-cols.png",
                height = 90,
                style = "margin-left:20px;margin-bottom:-15px;"
              ),
              # img(
              #   src = "data-info-find.png",
              #   height = 90,
              #   style = "position:absolute;top:180px;right:0;margin-right:50px;"
              # )
            ),
            fluidRow(
              id = "data-table-row",
              DTOutput("data")
            )
          )
        ),
        tabPanel(
          "Help",
          icon = icon(name = "question-circle"),
          id = "help-info",
          div(
            style = "overflow-y:scroll; height: calc(100vh - 200px) !important;",
            h3("About the Resilience Index"),
            p(
              "The Resilience Index is formed of three parts:",
              tags$strong("vulnerability"),
              ",",
              tags$strong("capacity to cope"),
              ", and",
              tags$strong("risk/history of shocks"),
              "."
            ),
            p(
              "Currently the Resilience Index only includes England. We will add devolved nations in a future release."
            ),
            actionButton("guide", "View interactive guide"),
            downloadButton("downloadVI1", "Download Vulnerability Index data"),
            downloadButton("downloadRI1", "Download Resilience Index data"),
            br(),
            br(),
            h4("Vulnerability"),
            p(
              "The Vulnerability Index models vulnerability in small neighbourhoods of ~7,000 - 10,000 people in four domains: ",
              tags$ul(
                tags$li("Clinical vulnerability"),
                tags$li("Health and wellbeing vulnerability"),
                tags$li("Economic vulnerability"),
                tags$li("Social vulnerability")
              )
            ),
            p(
              "For more information, see",
              tags$a(href = "https://docs.google.com/document/d/1aWpzgvLKGEF5Ay_xVps17nnbT1zIEki7RGIIJXL5APo/edit#", target = "_blank", "this document"),
              "."
            ),
            br(),
            h4("Capacity to cope"),
            p(
              "Capacity to cope to shocks such as floods and fires is calculated for Local Authorities, based on the following indicators:",
              tags$ul(
                tags$li("Local Authority spending power (£m per person) in the current year"),
                tags$li("Charities per 1,000 people"),
                tags$li("Volunteer capacity"),
                tags$li("Community engagement"),
                tags$li("Fire and Rescue Service response times")
              )
            ),
            p(
              "Community engagement measures the levels of third sector civic and community activity and barriers to participation and engagement. 
                      It shows whether charities are active in the area and whether people appear to be engaged in the broader civic life of their community. 
                      For more information, see",
              tags$a(href = "https://localtrust.org.uk/insights/research/left-behind-understanding-communities-on-the-edge", "this report")
            ),
            p("Volunteer capacity measures the ability of the NHS Volunteer Responders and the British Red Cross volunteers to meet demand."),
            br(),
            h4("Shocks"),
            p("Shocks are events such as disasters that impact a person or community, and may have large adverse effects on people/communities who are especially vulnerable. 
                      The Resilience Index includes two kinds of shock: flooding and dwelling fires."),
            p("Flooding is measured by the proportion of people in a Local Authority who live in high-risk flood zones. 
                      We have also included data on historical flooding incidents attended by Fire & Rescue Services as a measure of requiring additional support. 
                      Dwelling fires are measured as the three-year average number of fires per 10,000 people in a Local Authority."),
            br(),
            h3("About this app"),
            p("The Vulnerability Index, the Resilience Index, and this app were designed and developed by Matt Thomas, Elle Gordon, Mike Page and Freya Neason 
                      from the Insight & Improvement team at the British Red Cross. If you have any questions, comments or issues, please contact ", tags$a(href = "mailto:MattThomas@redcross.org.uk", "Matt")),
            p("All our code and as much of our data as possible are all available for free. They can be viewed, alongside our licenses, at:"),
            tags$ul(
              tags$li(tags$a(href = "https://github.com/britishredcrosssociety/covid-19-vulnerability", target = "_blank", "Vulnerability Index code and data")),
              tags$li(tags$a(href = "https://github.com/britishredcrosssociety/resilience-index", target = "_blank", "Resilience Index code and data")),
              tags$li(tags$a(href = "https://github.com/britishredcrosssociety/index-shiny-app", target = "_blank", "Code for this dashboard"))
            )
          )
        ) # tabPanel
      ) # tabBox
    ) # fluidRow
  ) # dashboardBody

  dashboardPagePlus(
    title = "British Red Cross Vulnerability Index and Resilience Index",
    header = dashboardHeaderPlus(
      title = "Resilience Index",
      titleWidth = "300px",
      # to add in bookmark button
      tags$li(class = "dropdown", bookmarkButton(), style = "padding-top: 8px; padding-bottom: 8px; padding-right: 15px"),
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "chart-pie"
    ),
    # - Main, left-hand sidebar -
    sidebar = dashboardSidebar(
      width = "300px",
      br(),
      h4("1. Select theme", id = "h_resilience", style = "padding-left:10px; padding-right:10px;"),
      sidebarMenu(
        id = "sidebar",
        menuItem(
          "Disasters and Emergencies",
          tabName = "shocks_tab",
          icon = icon("fire-extinguisher"),
          startExpanded = TRUE,
          selectInput(
            "shocks",
            label = "Filter areas by disaster",
            choices = c("None", "Floods", "Dwelling fires"),
            selected = "None",
            multiple = FALSE
          ),
          conditionalPanel(
            condition = "input.shocks == 'Floods'",
            checkboxInput(
              "highest_flood_risks",
              label = "Show only most-affected areas?",
              value = TRUE
            ),
            checkboxInput(
              "flood_incidents",
              label = "Include historical flooding?",
              value = FALSE
            )
          ),
          hr()
        ),
        menuItem(
          "Health Inequalities",
          icon = icon("stethoscope"),
          tabName = "health_tab",
          br(),
          tags$small(
            class = "badge center-block bg-green",
            tags$style(type = "text/css", ".badge{min-width: 200px;}"),
            "Coming soon"
          ),
          br(),
          hr()
        ),
        menuItem(
          "Migration and Displacement",
          icon = icon("user"),
          tabName = "migration_tab",
          br(),
          tags$small(
            class = "badge center-block bg-green",
            tags$style(type = "text/css", ".badge{min-width: 200px;}"),
            "Coming soon"
          ),
          br(),
          hr()
        )
      ),
      h4("2. Select area", style = "padding-left:10px; padding-right:10px;"),
      div(
        class = "lad-select",
        # use this <div> for help guide
        selectInput(
          "lad",
          label = "Choose a Local Authority",
          choices = c("- Show all Local Authorities -", sort(.globals$lad_shp$lad19nm)),
          selected = "- Show all Local Authorities -"
        ),
        checkboxInput(
          "filter_least_resilient",
          label = "Show only areas with highest vulnerability and lowest capacity?",
          value = FALSE
        )
      ),
      br(),
      div(
        a(href = "https://redcross.org.uk", target = "_blank", img(src = "assets/brc-logo.jpg", width = 225)),
        p(
          style = "font-size:7px; color:black;",
          a(href = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target = "_blank", "Contains public sector information licensed under the Open Government Licence v3.0.")
        ),
        style = "position:fixed; bottom:0; padding:10px; text-align: center;"
      )
    ),
    # - Right-hand sidebar showing VI underlying indicators -
    rightsidebar = rightSidebar(
      background = "light",
      rightSidebarTabContent(
        id = 1,
        title = "Clinical Vulnerability",
        icon = "stethoscope",
        active = TRUE,
        div(
          style = "overflow-y:scroll; height: calc(100vh - 200px) !important;",
          uiOutput("vi_clinical")
        )
      ),
      rightSidebarTabContent(
        id = 2,
        title = "Health/Wellbeing Vulnerability",
        icon = "heartbeat",
        div(
          style = "overflow-y:scroll; height: calc(100vh - 200px) !important;",
          uiOutput("vi_wellbeing")
        )
      ),
      rightSidebarTabContent(
        id = 3,
        title = "Economic Vulnerability",
        icon = "pound-sign",
        div(
          style = "overflow-y:scroll; height: calc(100vh - 200px) !important;",
          uiOutput("vi_economic")
        )
      ),
      rightSidebarTabContent(
        id = 4,
        title = "Social Vulnerability",
        icon = "users",
        div(
          style = "overflow-y:scroll; height: calc(100vh - 200px) !important;",
          uiOutput("vi_social")
        )
      )
    ),
    body_colwise
  )
}