ui <- dashboardPage(
  ##
  ## dashboardHeader    --------------------------------------------------------
  ##
  
  dashboardHeader(
    title = "Data Analysis"
  ),
  
  ##
  ## dashboardSidebar    -------------------------------------------------------
  ##
  
  dashboardSidebar(
    tags$style(type="text/css", "ul.sidebar-menu {height: 800px;}"), # prevent select_lead_UI crop when focus is released
    sidebarMenu(
      id = "tabs",
      width = 350,
      menuItem("Data", icon = icon("home"), tabName = "data"),
      menuItem("Data Exploration", icon = icon("tasks"), tabName = "data_exploration"),
      menuItem("Data Analysis", icon = icon("tasks"), tabName = "data_analysis"),
      shiny::HTML("</br>")
    )
  ),
  
  
  ##
  ## dashboardBody    ----------------------------------------------------------
  ##
  
  dashboardBody(
    ### . shiny-notification   -------------------------------------------------
    tags$head(tags$style(
      HTML(
        ".shiny-notification {
             position:fixed;
             top: calc(90%);
             left: calc(0.5%);
             }
             "
      )
    )),
    
    fluidPage(tabItems(
      
      ### . Tab / Data   -------------------------------------------------------
      tabItem(
        tabName = "data",
        fluidRow(
          column(
            width = 8,
            h3("Variables"),
            verbatimTextOutput("structure",placeholder = FALSE),
            tags$head(tags$style("#structure{height:400px; overflow-y:scroll;}"))
          ),
          column(
            width = 12,
            h3("Summary"),
            verbatimTextOutput("summary",placeholder = FALSE),
            tags$head(tags$style("#summary{height:400px; overflow-y:scroll;}"))
          )
        ),
        fluidRow(
          column(
            h3("View Data"),
            width = 12,
            DTOutput('contents')
          )
        )
      ),
      
      ### . Tab / Data Exploration   ----------------------------------------
      tabItem(
        tabName = "data_exploration",
        fluidRow(
          column(
            width = 12,
            style = 'padding-bottom: 10px;'
          )
        ),
        box(
          dataTableOutput("data_structure"),
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          title = "Introduce Data",
          collapsible = TRUE
        ),
        fluidRow(
          column(
            style = 'padding-bottom: 10px;',
            width = 6,
            column(width = 12, 
                   uiOutput("bi_variate_selection"))
          )),
        box(
          uiOutput("page_selection"),
          plotOutput("data_distribution_2"),
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          title = "Distribution of discrete features",
          collapsible = TRUE
        )
        
      ), ### End Tab / Data Exploration
      
      ### . Tab / Data Analysis   ----------------------------------------
      tabItem(
        tabName = "data_analysis",
        fluidRow(
          column(
            width = 12,
            style = 'padding-bottom: 10px;'
          )
        ),
        fluidRow(
          column(width = 2),
          column(
            width = 10,
            h2("Occurances of Reptiles in ACT region")
          )
        ),
        box(
          uiOutput("map_col_selection"),
          plotlyOutput("map_plot"),
          br(),
          br(),
          h4("All the records are indeed within the ACT region and most 
             are located in Non Forest areas and majority of them belongs to 'Squamta' order"),
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          title = "Map plot",
          collapsible = TRUE
        ),
        box(
          uiOutput("dist_col_selection"),
          plotOutput("dist_plot"),
          br(),
          br(),
          h4("Non forest, Eucalypt Medium Open, Eucalypt Medium Woodland, Other Native Forest are 
             the four categories of forests in which about 95% of the records were found."),
          h4("The nature of record for most of the occurances in 'Unknown'."),
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          title = "Distribution plot",
          collapsible = TRUE
        ),
        box(
          plotOutput("facet_plot"),
          br(),
          br(),
          h4("Most of the records are from 'Squamata' order among which a vast majority 
             belongs to 'Scincidae' family. There are only two families viz., 'Chelidae' and 'cheloniidae' in 
             'Testudines' order among which majority of them belongs to 'Chelidae' family." ),
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          title = "Family Taxon plot",
          collapsible = TRUE
        ),
        box(
          plotOutput("heat_plot1"),
          br(),
          br(),
          h4("As seen above, the largest number of records in the two order taxa 
             are found in non-forest areas"),
          br(),
          plotOutput("heat_plot2"),
          br(),
          br(),
          h4("All the occurances recorded from 'ACT wildlife Atlas' are of 'unknown' nature" ),
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          title = "Heat map",
          collapsible = TRUE
        )
        
      ) ### End Tab / Data Exploration
      
    ))
  )
)