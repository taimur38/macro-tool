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

plot_compare <- function(ind, df=db, ccode=target, comparators=main_comparators, labels=F, range=c(1990, 2021)) {

    return(df %>%
           filter(weo_countrycodeiso %in% c(ccode,comparators)) %>%
           filter(year >= range[1] & year <= range[2]) %>%
           ggplot(aes_string(x="year", y=ind, group="year", label="weo_countrycodeiso")) +
           geom_boxplot(outlier.shape = NA) +
           geom_text(alpha = ifelse(labels, 0.1, 0)) +
           geom_line(data = . %>% filter(weo_countrycodeiso == ccode), aes(group=NA), color="orange", size= 1) +
           geom_point(data = . %>% filter(weo_countrycodeiso == ccode), aes(group=NA), color="orange") +
           labs(x="Year", y=reverse_labels[ind])
    )
}

plot_compare_lines <- function(ind, df=db, ccode=target, comparators=main_comparators, labels=F, range=c(1990, 2021)) {

    return(df %>%
           filter(weo_countrycodeiso %in% c(ccode,comparators)) %>%
           filter(year >= range[1] & year <= range[2]) %>%
           ggplot(aes_string(x="year", y=ind, label="weo_countrycodeiso")) +
           geom_line(data = . %>% filter(weo_countrycodeiso != ccode), aes(color=weo_countrycodeiso), alpha=0.4) +
           geom_line(data = . %>% filter(weo_countrycodeiso == ccode), color="orange", size= 1) +
           geom_point(data = . %>% filter(weo_countrycodeiso == ccode), color="orange") +
           labs(x="Year", y=reverse_labels[ind])
    )

}

plot_scatter <- function(ind1, ind2, df=db, ccode=target, comparators=main_comparators, labels=F, range=c(1990, 2021)) {

    return(
           df %>%
               filter(weo_countrycodeiso %in% c(ccode, comparators)) %>%
               filter(year >= range[1] & year <= range[2]) %>%
               ggplot(aes_string(x=ind1, y=ind2, label="weo_countrycodeiso")) +
               geom_point(data = . %>% filter(weo_countrycodeiso != ccode), alpha=0.4) +
               geom_point(data = . %>% filter(weo_countrycodeiso == ccode), color="orange", size=2) +
               labs(
                    x=reverse_labels[ind1],
                    y=reverse_labels[ind2]
               )
    )

}

bbnn_plot <- function(ib_ind, eb_ind, df=db, ccode=target, fyear=2020) {

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
                           tabPanel("Explore",
                                    tags$h2("Explore Indicators"),
                                    sidebarLayout(
                                                  sidebarPanel(
                                                               selectInput("plot_type", "Plot Type",
                                                                           choices=c("plot_compare", "plot_scatter", "plot_compare_lines"),
                                                                           selected="plot_compare"
                                                               ),
                                                               selectizeInput("indicator", "Indicator",
                                                                           choices=c("weo_ngdp_rpc"),
                                                                           selected="weo_ngdp_rpc"
                                                               ),
                                                               tabsetPanel(
                                                                           id="explore_switcher",
                                                                           type="hidden",
                                                                           
                                                                           tabPanelBody("plot_compare", ""),
                                                                           tabPanelBody("plot_compare_lines", ""),
                                                                           tabPanelBody("plot_scatter",
                                                                                        selectizeInput("indicator2", "Indicator",
                                                                                                    choices=NULL,
                                                                                        )
                                                                           )
                                                               ),
                                                               sliderInput("year_range", "Year Range",
                                                                                min=min(db$year),
                                                                                max=max(db$year),
                                                                                step = 1,
                                                                                sep="",
                                                                                value=c(1990, 2021)
                                                               ),
                                                               actionButton("explore_update", "Update")
                                                  ),
                                                  mainPanel(
                                                            plotlyOutput("explore_plot")
                                                  )
                                    )
                            ),
                           tabPanel("BB-NN",
                                    tags$h2("BB-NN"),
                                    sidebarLayout(
                                                  sidebarPanel(
                                                               selectizeInput("internal_balance",
                                                                           "Internal Balance",
                                                                           choices=NULL
                                                               ),
                                                               selectizeInput("external_balance",
                                                                           "External Balance",
                                                                           choices=NULL
                                                               ),
                                                               selectInput("bbnn_year", "Year",
                                                                           choices=unique(db$year),
                                                                           selected=2020
                                                               ),
                                                               sliderInput("bbnn_year_range", "Year Range",
                                                                                min=min(db$year),
                                                                                max=max(db$year),
                                                                                sep="",
                                                                                step = 1,
                                                                                value=c(1990, 2021)
                                                               )
                                                    ),
                                                  mainPanel(
                                                            "BB-NN",
                                                            plotlyOutput("bbnn_plot"),
                                                            "Internal Balance",
                                                            plotOutput("internal_balance_plot"),
                                                            "External Balance",
                                                            plotOutput("external_balance_plot")
                                                  )
                                    )
                           )
                )

)

server <- function(input, output, session) {

    observeEvent(input$plot_type, {
        updateTabsetPanel(session, "explore_switcher", selected=input$plot_type)
    })

    updateSelectizeInput(session, 'indicator', choices=labels, server=TRUE, selected="weo_ngdp_rpch")
    updateSelectizeInput(session, 'indicator2', choices=labels, server=TRUE, selected="wdi_nv_agr_totl_zs")
    updateSelectizeInput(session, 'internal_balance', choices=labels, server=TRUE, selected="wdi_fp_cpi_totl_zg")
    updateSelectizeInput(session, 'external_balance', choices=labels, server=TRUE, selected="wdi_bn_cab_xoka_gd_zs")

    ?eventReactive

    indicator1 <- eventReactive(input$explore_update, input$indicator)
    indicator2 <- eventReactive(input$explore_update, input$indicator2)
    year_range <- eventReactive(input$explore_update, input$year_range)

    output$explore_plot <- renderPlotly({

        if (input$plot_type == "plot_compare") {
            plot_compare(indicator1(), ccode=input$country, comparators=input$comparators, range=year_range()) %>%
                ggplotly()
        } else if (input$plot_type == "plot_compare_lines") {
            plot_compare_lines(indicator1(), ccode=input$country, comparators=input$comparators, range=year_range()) %>%
                ggplotly()
        } else {
            plot_scatter(indicator1(), indicator2(), ccode=input$country, comparators=input$comparators, range=year_range()) %>%
                ggplotly()
        }
    })

    output$bbnn_plot <- renderPlotly({
        # show the bbnn plot based on indicators selected
        # for the year chosen
        bbnn_plot(input$internal_balance, input$external_balance, ccode=input$country, fyear=input$bbnn_year) %>%
            ggplotly()
    })

    output$external_balance_plot <- renderPlot({
        # show the external balance plot over time
        plot_compare(input$external_balance, ccode=input$country, comparators=input$comparators, range=input$bbnn_year_range) +
            geom_hline(yintercept=0, color="black", linetype="dashed")
    })

    output$internal_balance_plot <- renderPlot({
        # show the internal balance plot over time
        plot_compare(input$internal_balance, ccode=input$country, comparators=input$comparators, range=input$bbnn_year_range) + 
            geom_hline(yintercept=0, color="black", linetype="dashed")
    })

}

shinyApp(ui, server)

