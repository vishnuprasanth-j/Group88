# -----------------------------------------------------------------------------
# Case Study 4
# AKSTA Statistical Computing
# Authored by members of Group 88
# 2026-06-10
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(jsonlite)
library(DT)
library(countrycode)


# -----------------------------------------------------------------------------
# More preparation and global variables for later usage
# -----------------------------------------------------------------------------

# Importing CIA data
data_cia <- jsonlite::fromJSON("data_cia2.json")

# Mapping of raw (non user-friendly) column names and labels we want to show
var_choices <- c(
  "Expenditure on education (% of GDP)" = "expenditure",
  "Youth unemployment rate (%)"         = "youth_unempl_rate",
  "Net migration rate (per 1,000)"      = "net_migr_rate",
  "Population growth rate (%)"          = "pop_growth_rate",
  "Electricity from fossil fuels (%)"   = "electricity_fossil_fuel",
  "Life expectancy at birth (years)"    = "life_expectancy"
)

# Variables that can be used to scale the size of the points in the scatterplot
size_choices <- c(
  "Population" = "population",
  "Area (km2)" = "area"
)

# Helper: turn a raw column name back into its user-friendly label.
label_for <- function(var) names(var_choices)[match(var, var_choices)]

# Prepare world map (done only once)
world_map <- map_data("world")
world_map$ISO3 <- countrycode::countrycode(
  sourcevar   = world_map$region,
  origin      = "country.name",
  destination = "iso3c",
  nomatch     = NA,
  warn        = FALSE
)

# Color palette for the five continents.
continent_palette <- c(
  "Africa"   = "#E69F00",
  "Americas" = "#56B4E9",
  "Asia"     = "#009E73",
  "Europe"   = "#0072B2",
  "Oceania"  = "#CC79A7"
)


# -----------------------------------------------------------------------------
# User interface
# -----------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("CIA World Factbook 2020"),
  p(em(paste(
    "Welcome to my shiny app, which allows you to visualize variables from the",
    "CIA 2020 factbook on the world map, generate descriptive statistics and",
    "statistical graphics."
  ))),

  tabsetPanel(

    # Tab 1: Univariate analysis
    tabPanel(
      "Univariate analysis",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          selectInput(
            "uni_var",
            "Select a variable:",
            choices  = var_choices,
            selected = "expenditure"
          ),
          actionButton("show_raw", "View raw data"),
          br(), br(),
          # The raw data table only shows up after the button is pressed.
          DT::DTOutput("raw_table")
        ),
        mainPanel(
          width = 8,
          tabsetPanel(
            tabPanel(
              "Map",
              br(),
              p(em(paste(
                "The map contains values of the selected variable. The countries",
                "with gray areas have a missing value for the visualized variable."
              ))),
              plotlyOutput("map_plot", height = "500px")
            ),
            tabPanel(
              "Global analysis",
              br(),
              p("Distribution of the selected variable over all countries."),
              fluidRow(
                column(6, plotlyOutput("global_hist", height = "400px")),
                column(6, plotlyOutput("global_box",  height = "400px"))
              )
            ),
            tabPanel(
              "Analysis per continent",
              br(),
              p("Distribution of the selected variable grouped by continent."),
              fluidRow(
                column(6, plotlyOutput("cont_density", height = "400px")),
                column(6, plotlyOutput("cont_box",     height = "400px"))
              )
            )
          )
        )
      )
    ),

    # Tab 2: Multivariate analysis
    tabPanel(
      "Multivariate analysis",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          selectInput(
            "multi_var1",
            "Select variable 1:",
            choices  = var_choices,
            selected = "expenditure"
          ),
          selectInput(
            "multi_var2",
            "Select variable 2:",
            choices  = var_choices,
            selected = "youth_unempl_rate"
          ),
          selectInput(
            "size_var",
            "Scale points by:",
            choices  = size_choices,
            selected = "population"
          )
        ),
        mainPanel(
          width = 8,
          h4("Scatterplot"),
          plotlyOutput("scatter_plot", height = "550px")
        )
      )
    )
  )
)



# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  # Univariate analysis -------------------------------------------------

  # Reactive selected variable (raw column name) for the univariate tab.
  uni_var <- reactive(input$uni_var)

  # Raw-data table
  raw_data <- eventReactive(input$show_raw, {
    data_cia %>%
      select(country, continent, value = all_of(uni_var())) %>%
      rename(
        "Country"            = country,
        "Continent"          = continent
      ) %>%
      rename_with(~ label_for(uni_var()), .cols = "value")
  })

  output$raw_table <- DT::renderDT({
    DT::datatable(
      raw_data(),
      rownames = FALSE,
      options  = list(pageLength = 15, lengthChange = FALSE)
    )
  })

  # Map: join the world polygons with the selected CIA variable and colour by it.
  output$map_plot <- renderPlotly({
    var <- uni_var()

    cia_by_iso <- data_cia %>%
      select(ISO3, country, value = all_of(var)) %>%
      distinct(ISO3, .keep_all = TRUE)

    map_df <- world_map %>%
      left_join(cia_by_iso, by = "ISO3")

    p <- ggplot(map_df, aes(
      x = long, y = lat, group = group,
      # Custom tooltip text: country name + value of the selected variable.
      text = paste0(country, "\n", label_for(var), ": ", value)
    )) +
      geom_polygon(aes(fill = value), colour = "white", linewidth = 0.1) +
      scale_fill_viridis_c(name = label_for(var), na.value = "grey80") +
      labs(x = "long", y = "lat") +
      theme_minimal()

    ggplotly(p, tooltip = "text")
  })

  # Global histogram with density overlay.
  output$global_hist <- renderPlotly({
    var <- uni_var()
    df  <- data_cia %>% filter(!is.na(.data[[var]]))

    p <- ggplot(df, aes(x = .data[[var]])) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 30, fill = "grey70", colour = "white") +
      geom_density(fill = "#0072B2", alpha = 0.3, colour = "#0072B2") +
      labs(x = label_for(var), y = "density") +
      theme_minimal()

    ggplotly(p)
  })

  # Global boxplot over whole data set.
  output$global_box <- renderPlotly({
    var <- uni_var()
    df  <- data_cia %>% filter(!is.na(.data[[var]]))

    p <- ggplot(df, aes(y = .data[[var]])) +
      geom_boxplot(fill = "#0072B2", alpha = 0.4) +
      labs(x = NULL, y = label_for(var)) +
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

    ggplotly(p)
  })

  # Density plot grouped by continent.
  output$cont_density <- renderPlotly({
    var <- uni_var()
    df  <- data_cia %>% filter(!is.na(.data[[var]]))

    p <- ggplot(df, aes(x = .data[[var]], fill = continent, colour = continent)) +
      geom_density(alpha = 0.4) +
      scale_fill_manual(values = continent_palette, name = "continent") +
      scale_colour_manual(values = continent_palette, name = "continent") +
      labs(x = label_for(var), y = "density") +
      theme_minimal()

    ggplotly(p)
  })

  # Grouped boxplot by continent.
  output$cont_box <- renderPlotly({
    var <- uni_var()
    df  <- data_cia %>% filter(!is.na(.data[[var]]))

    p <- ggplot(df, aes(x = continent, y = .data[[var]], fill = continent)) +
      geom_boxplot(alpha = 0.6) +
      scale_fill_manual(values = continent_palette, guide = "none") +
      labs(x = "Continent", y = label_for(var)) +
      theme_minimal()

    ggplotly(p)
  })

  
  # Multivariate analysis ----------------------------------------------

  # Reactive selections for the scatterplot.
  multi_var1 <- reactive(input$multi_var1)
  multi_var2 <- reactive(input$multi_var2)
  size_var   <- reactive(input$size_var)

  output$scatter_plot <- renderPlotly({
    v1 <- multi_var1()
    v2 <- multi_var2()
    sz <- size_var()

    df <- data_cia %>%
      filter(!is.na(.data[[v1]]), !is.na(.data[[v2]]), !is.na(.data[[sz]]))

    p <- ggplot(df, aes(x = .data[[v1]], y = .data[[v2]], colour = continent)) +
      # Size only the points, not the smooth lines.
      geom_point(aes(size = .data[[sz]],
                     text = paste0(
                       country, "\n",
                       label_for(v1), ": ", .data[[v1]], "\n",
                       label_for(v2), ": ", .data[[v2]], "\n",
                       label_for(sz), ": ", .data[[sz]]
                     )),
                 alpha = 0.6) +
      geom_smooth(aes(group = continent), method = "loess",
                  se = FALSE, linewidth = 0.7) +
      scale_colour_manual(values = continent_palette, name = "continent") +
      scale_size_continuous(name = label_for(sz)) +
      labs(x = label_for(v1), y = label_for(v2)) +
      theme_minimal()

    ggplotly(p, tooltip = "text")
  })
}



# -----------------------------------------------------------------------------
# Startup Server
# -----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
