library(shiny)
library(shinydashboard)
library(shinycssloaders)

dashboardPage(
    dashboardHeader(title = "Market Basket Analysis", disable = TRUE),
    dashboardSidebar(
        tags$style(HTML(".main-sidebar li a { font-size: 18px; }")),
        menuItem("Market Basket Analysis", tabName = "Market Basket Analysis", icon = icon("dashboard")),
        sliderInput("conf", "Confidence", 0.01, 0.9, 0.3),
        sliderInput("supp", "Support", 0.00001, 0.0001, 0.00005),
        checkboxInput("rr", "Remove Redundant Rules", FALSE),
        uiOutput("productSelector"),
        actionButton("refreshButton", "Generate Recommendations", width = "88%")
    ),
    dashboardBody(tabsetPanel(
        tabPanel("Rules Table", withSpinner(DT::dataTableOutput("rules_table"))),
        tabPanel("Scattered Graph", withSpinner(plotOutput("graph_scattered"))),
        tabPanel("Network Graph", withSpinner(plotOutput("graph_network"))),
        tabPanel("Raw Data", withSpinner(DT::dataTableOutput("data")))
    ))
)