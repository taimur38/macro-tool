guille_ui <- function(id, db) {

  tagList(
    tags$h2("Guille"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(NS(id, "internal_balance"),
                       "Internal Balance",
                       choices = NULL
        ),
        selectizeInput(NS(id, "external_balance"),
                       "External Balance",
                       choices = NULL
        ),
        selectInput(NS(id, "year"), "Year",
                    choices = unique(db$year),
                    selected = 2020
        ),
        sliderInput(NS(id, "year_range"), "Year Range",
                    min = min(db$year),
                    max = max(db$year),
                    sep = "",
                    step = 1,
                    value = c(1990, 2021)
        ),
        selectInput(NS(id, "test"), "Test Input", choices = c("A", "B", "C"), selected = "A"),
        actionButton(NS(id, "explore_update"), "Update")
      ),
      mainPanel(
        tags$h2(""),
        tags$h3("Test Stacked Area Plot"),
        plotlyOutput(NS(id, "current_account_plot")),
        tags$h2(""),
        tags$h3("BBNN"),
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

    updateSelectizeInput(session, 'internal_balance', choices = labels, server = TRUE, selected = "wdi_fp_cpi_totl_zg")
    updateSelectizeInput(session, 'external_balance', choices = labels, server = TRUE, selected = "wdi_bn_cab_xoka_gd_zs")

db2 <- db %>%
  select(year, wdi_bx_gsr_mrch_cd, wdi_bx_gsr_nfsv_cd, wdi_bx_gsr_fcty_cd, wdi_bx_trf_curr_cd, wdi_bm_gsr_mrch_cd, wdi_bm_gsr_nfsv_cd, wdi_bm_gsr_fcty_cd, wdi_bm_trf_prvt_cd)

db2_long <- db2 %>%
  pivot_longer(cols = -year, names_to = "Category", values_to = "Value")

    output$current_account_plot <- renderPlot({
      db2_long %>%
        filter(weo_countrycodeiso == country()) %>%
        ggplot(aes_string(x = "year", y = input$Value, fill = input$Category)) +
        geom_line() +
        geom_point()
    })

    output$bbnn_plot <- renderPlotly({
      # show the bbnn plot based on indicators selected
      # for the year chosen
      bbnn_plot(input$internal_balance, input$external_balance, ccode=country(), df = db, fyear = input$year, reverse_labels = reverse_labels) %>%
        ggplotly()
    })

    output$internal_balance_plot <- renderPlot({
      db %>%
        filter(weo_countrycodeiso == country()) %>%
        ggplot(aes_string(x = "year", y = input$internal_balance)) +
        geom_line() +
        geom_point()
    })

    output$external_balance_plot <- renderPlot({
      db %>%
        filter(weo_countrycodeiso == country()) %>%
        ggplot(aes_string(x = "year", y = input$external_balance)) +
        geom_line() +
        geom_point()
    })

  })

}
