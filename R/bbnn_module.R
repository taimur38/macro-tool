bbnn_plot <- function(ib_ind, eb_ind, df=db, ccode=target, fyear=2020, reverse_labels=NULL) {

    return(
           df %>%
               filter(year == fyear) %>%
               ggplot(aes_string(x=ib_ind, y=eb_ind, label="weo_countrycodeiso")) +
               geom_point(data = . %>% filter(weo_countrycodeiso != ccode), alpha=0.4) +
               geom_point(data = . %>% filter(weo_countrycodeiso == ccode), color="orange", size=2) +
               geom_hline(yintercept=0, color="black", linetype="dashed") +
               geom_vline(xintercept=0, color="black", linetype="dashed") +
               labs(
                    x=reverse_labels[ib_ind],
                    y=reverse_labels[eb_ind]
               )
    )

}

bbnn_ui <- function(id, db) {

    tagList(
        tags$h2("BB-NN"),
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
                                    )
                        ),
                        mainPanel(
                                plotlyOutput(NS(id, "bbnn_plot")),
                                tags$h3("Internal Balance"),
                                plotOutput(NS(id, "internal_balance_plot")),
                                tags$h3("External Balance"),
                                plotOutput(NS(id, "external_balance_plot"))
                        )
        )

    )
}

bbnn_server <- function(id, country, comparators, db, labels, reverse_labels) {

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
            plot_compare(
                         input$internal_balance, 
                         ccode=country(), 
                         df=db, 
                         comparators=comparators(), 
                         range=input$year_range,
                         reverse_labels=reverse_labels
            ) 
        })

        output$external_balance_plot <- renderPlot({
            plot_compare(
                         input$external_balance, 
                         ccode=country(), 
                         df=db, 
                         comparators=comparators(), 
                         range=input$year_range,
                         reverse_labels=reverse_labels
            ) 
        })

    })

}
