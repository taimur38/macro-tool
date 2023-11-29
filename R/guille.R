guille_ui <- function(id, db) {

  tagList(
    tags$h2("Guille"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(NS(id, "internal_balance"),
                       "Internal Balance",
                       choices=NULL
        ),
        selectizeInput(NS(id, "external_balance"),
                       "External Balance",
                       choices=NULL
        ),
        selectInput(NS(id, "year"), "Year",
                    choices=unique(db$year),
                    selected=2020
        ),
        sliderInput(NS(id, "year_range"), "Year Range",
                    min=min(db$year),
                    max=max(db$year),
                    sep="",
                    step = 1,
                    value=c(1990, 2021)
        ),
        selectInput(NS(id, "test"), "Test Input", choices=c("A", "B", "C"), selected="A")
      ),
      mainPanel(
        plotlyOutput(NS(id, "bbnn_plot")),
        tags$h2(""),
        tags$h3("Internal Balance"),
        plotOutput(NS(id, "internal_balance_plot")),
        tags$h3("External Balance"),
        plotOutput(NS(id, "external_balance_plot"))
      )
    )

  )
}

guille_server <- function(id, country, comparators, db, labels, reverse_labels) {

  stopifnot(is.reactive(country))
  stopifnot(is.reactive(comparators))

  moduleServer(id, function(input, output, session) {

    updateSelectizeInput(session, 'internal_balance', choices=labels, server=TRUE, selected="wdi_fp_cpi_totl_zg")
    updateSelectizeInput(session, 'external_balance', choices=labels, server=TRUE, selected="wdi_bn_cab_xoka_gd_zs")

    output$bbnn_plot <- renderPlotly({
      # show the bbnn plot based on indicators selected
      # for the year chosen
      bbnn_plot(input$internal_balance, input$external_balance, ccode=country(), df=db, fyear=input$year, reverse_labels=reverse_labels) %>%
        ggplotly()
    })

    output$internal_balance_plot <- renderPlot({
      db %>%
        filter(weo_countrycodeiso == country()) %>%
        ggplot(aes_string(x="year", y=input$internal_balance)) +
        geom_line() +
        geom_point()
    })

    output$external_balance_plot <- renderPlot({

    })

  })

}
