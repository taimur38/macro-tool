

debt_ui <- function(id, db) {

  tagList(
    tags$h2("Debt"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(NS(id, "internal_balance"),
                       "Internal Balance",
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
        )
      ),
      mainPanel(
        plotlyOutput(NS(id, "bbnn_plot")),
        tags$h3("Internal Balance"),
        plotOutput(NS(id, "plot1"))
      )
    )

  )
}

debt_server <- function(id, country, comparators, db, labels, reverse_labels) {

  stopifnot(is.reactive(country))
  stopifnot(is.reactive(comparators))

  moduleServer(id, function(input, output, session) {

    updateSelectizeInput(session, 'internal_balance', choices=labels, server=TRUE, selected="wdi_fp_cpi_totl_zg")

    output$bbnn_plot <- renderPlotly({
      # show the bbnn plot based on indicators selected
      # for the year chosen
      bbnn_plot(input$internal_balance, input$external_balance, ccode=country(), df=db, fyear=input$year, reverse_labels=reverse_labels) %>%
        ggplotly()
    })

    output$plot1 <- renderPlot({
      db %>%
        filter(weo_countrycodeiso == country()) %>%
        ggplot(aes_string(x="year", y=input$internal_balance)) +
        geom_line() +
        geom_point()

    })

  })

}
