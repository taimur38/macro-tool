
explore_ui <- function(id, db) {

    tagList(
            tags$h2("Explore Indicators"),
            sidebarLayout(
                            sidebarPanel(
                                        selectInput(NS(id, "plot_type"), "Plot Type",
                                                    choices=c("plot_compare", "plot_scatter", "plot_compare_lines"),
                                                    selected="plot_compare"
                                        ),
                                        selectizeInput(NS(id, "indicator"), "Indicator",
                                                    choices=c("weo_ngdp_rpc"),
                                                    selected="weo_ngdp_rpc"
                                        ),
                                        tabsetPanel(
                                                    id=NS(id, "explore_switcher"),
                                                    type="hidden",
                                                    
                                                    tabPanelBody("plot_compare", ""),
                                                    tabPanelBody("plot_compare_lines", ""),
                                                    tabPanelBody("plot_scatter",
                                                                selectizeInput("indicator2", "Indicator",
                                                                            choices=NULL,
                                                                )
                                                    )
                                        ),
                                        sliderInput(NS(id, "year_range"), "Year Range",
                                                        min=min(db$year),
                                                        max=max(db$year),
                                                        step = 1,
                                                        sep="",
                                                        value=c(1990, 2021)
                                        ),
                                        actionButton(NS(id, "explore_update"), "Update")
                            ),
                            mainPanel(
                                    plotlyOutput(NS(id, "explore_plot"))
                            )
            )

    )

}

explore_server <- function(id, country, comparators, db, labels, reverse_labels) {

    moduleServer(id, function(input, output, session) {
        updateSelectizeInput(session, 'indicator', choices=labels, server=TRUE, selected="weo_ngdp_rpch")
        updateSelectizeInput(session, 'indicator2', choices=labels, server=TRUE, selected="wdi_nv_agr_totl_zs")


        indicator1 <- eventReactive(input$explore_update, input$indicator)
        indicator2 <- eventReactive(input$explore_update, input$indicator2)
        year_range <- eventReactive(input$explore_update, input$year_range)

        output$explore_plot <- renderPlotly({

            if (input$plot_type == "plot_compare") {
                plot_compare(
                             indicator1(), 
                             ccode=input$country, 
                             df=db, 
                             comparators=input$comparators, 
                             range=year_range(), 
                             reverse_labels=reverse_labels
                ) %>%
                ggplotly()
            } else if (input$plot_type == "plot_compare_lines") {
                plot_compare_lines(
                                   indicator1(), 
                                   ccode=input$country, 
                                   df=db, 
                                   comparators=input$comparators, 
                                   range=year_range(),
                                   reverse_labels=reverse_labels
                ) %>%
                ggplotly()

            } else {
                plot_scatter(
                             indicator1(), 
                             indicator2(), 
                             df = db, 
                             ccode=input$country, 
                             comparators=input$comparators, 
                             range=year_range(), 
                             reverse_labels=reverse_labels
                ) %>%
                ggplotly()
            }
        })

    })
}
