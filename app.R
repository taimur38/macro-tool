library(shiny)
library(tidyverse)
library(arrow)
library(ggthemes)
library(plotly)

options(warn = -1)

db <- read_parquet("data/DEV309_Database.parquet")
labels <- read_parquet("data/labels_macro.parquet") %>% pull(variable, label)
reverse_labels <- setNames(names(labels), labels)

countries <- db %>% 
    select(weo_countryname, weo_countrycodeiso) %>% 
    unique() %>% 
    pull(weo_countrycodeiso, weo_countryname)

theme_set(theme_few())

ui <- fixedPage(
                navbarPage(
                           "Macro Dashboard",
                           collapsible = TRUE,
                           fluidRow(
                                    column(6,
                                           align="center",
                                           selectInput("country", "Country", choices = countries, selected = "PAK")
                                    ), 
                                    column(6,
                                           selectInput("comparators", 
                                                       "Comparators", 
                                                       choices = countries, 
                                                       multiple = TRUE, 
                                                       selected = c("IND", "BGD", "EGY", "VNM", "IDN", "LKA", "MAR")
                                           )
                                    )
                           ),
                           tabPanel("Explore", explore_ui("explore", db)),
                           tabPanel("BB-NN", bbnn_ui("bbnn", db))
                )

)

server <- function(input, output, session) {

    observeEvent(input$plot_type, {
        updateTabsetPanel(session, "explore_switcher", selected=input$plot_type)
    })

    explore_server("explore", 
                   reactive(input$country), 
                   reactive(input$comparators), 
                   db, labels, reverse_labels
    )

    bbnn_server("bbnn", 
                reactive(input$country), 
                reactive(input$comparators), 
                db, labels, reverse_labels
    )

    # this is a hack to get automatic reloading to work nicely 
    # in development mode.
    # will take this out

    source("R/explore.R", local=TRUE)
    source("R/bbnn_module.R", local=TRUE)

}

shinyApp(ui, server)

