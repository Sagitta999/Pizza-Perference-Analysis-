# ui #

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title="Analysis Team 9"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Hult-ComboMenu", icon = icon("th"), tabName = "widgets"),
      menuItem("Histogram", icon = icon("bar-chart-o"), startExpanded = TRUE,
               menuSubItem("Pizza topping preferences", tabName = "subitem1"),
               menuSubItem("How do you like your crust", tabName = "subitem2"),
               menuSubItem("Kind of beverage", tabName = "subitem3"),
               menuSubItem("Say yes to Pineapple", tabName = "subitem4"),
               menuSubItem("Health conscious", tabName = "subitem5")),
      menuItem("Sentiment", icon = icon("bar-chart-o"), startExpanded = TRUE,
               menuSubItem("Pizza topping preferences", tabName = "subitem6"),
               menuSubItem("How do you like your crust", tabName = "subitem7"),
               menuSubItem("Kind of beverage", tabName = "subitem8"),
               menuSubItem("Say yes to Pineapple", tabName = "subitem9")),
      
      
      menuItem("LDA", icon = icon("th"), tabName = "LDA"),
      
      menuItem("Network", icon = icon("th"), tabName = "Network" ,startExpanded = TRUE,
               menuSubItem("Pizza topping preferences", tabName = "subitem11"),
               menuSubItem("How do you like your crust", tabName = "subitem12"),
               menuSubItem("Kind of beverage", tabName = "subitem13"),
               menuSubItem("Say yes to Pineapple", tabName = "subitem14")),
      menuItem("TF-IDF", icon = icon("th"), tabName = "plottf_idf"),
      menuItem("WorldCloud", tabName = "worldcloud1", icon = icon("dashboard"))
    ),# closeSlider Menu
    textOutput("res")
  ),#close sidebar
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                box(title=" Prediction Model ",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("barchart", height =800, width=800)))),
      tabItem("widgets", "Widgets tab content"),
      
      tabItem("subitem1",
              fluidRow(
                box(title="Frequency",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("graph_1", height =500)),
                
                box(title = "Pizza Toppings",status = "warning",solidHeader = TRUE,
                    sliderInput("frequency", "Top Frequencies:", min=2, max=15, value=10)))),
      
      tabItem("subitem2",
              fluidRow(
                box(title="Frequency",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("graph_2", height =500)),
                
                box(title = "Most likely crust",status = "warning",solidHeader = TRUE,
                    sliderInput("frequency2", "Top Frequencies:", min=2, max=15, value=10)))),
      
      tabItem("subitem3",
              fluidRow(
                box(title="Frequency",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("graph_3", height =500)),
                
                box(title = "Prefered Beverages",status = "warning",solidHeader = TRUE,
                    sliderInput("frequency3", "Top Frequencies:", min=2, max=15, value=10)))),
      
      tabItem("subitem4", 
              fluidRow(
                box(title="Frequency",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("graph_4", height =500)),
                
                box(title = "Love for pineapple",status = "warning",solidHeader = TRUE,
                    sliderInput("frequency4", "Top Frequencies:", min=2, max=15, value=10)))),
      
      tabItem("subitem5",
              fluidRow(
                box(title="Frequency",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("graph_5", height =500)),
                
                box(title = "Health conscious???",status = "warning",solidHeader = TRUE,
                    sliderInput("frequency5", "Top Frequencies:", min=1, max=2, value=1)))),
      
      tabItem("subitem7", 
              fluidRow(
                box(title="Contribution to sentiment",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("plot7", height =500)),
              )),
      tabItem("subitem6", 
              fluidRow(
                box(title="Contribution to sentiment",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("plot8", height =500)),
              )),
      tabItem("subitem8", 
              fluidRow(
                box(title="Contribution to sentiment",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("plot9", height =500)),
              )),
      tabItem("subitem9",
              fluidRow(
                box(title="Contribution to sentiment",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("plot10", height =500)),
              )),
      tabItem("subitem10", "Sub-item 10 tab content"),
      
      tabItem("LDA",
              fluidRow(
                box(title="Linear Discriminant Analysis",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("plotlda", height =800, width=800)))),
      tabItem("subitem11",
              fluidRow(
                box(plotOutput("plot1", height =800 , width = 800)))),
      tabItem("subitem12",
              fluidRow(
                box(plotOutput("plot2", height =800 , width = 800)))),
      tabItem("subitem13",
              fluidRow(
                box(plotOutput("plot3", height =800 , width = 800)))),
      tabItem("subitem14",
              fluidRow(
                box(plotOutput("plot4", height =800 , width = 800)))),
      tabItem("plottf_idf",
              fluidRow(
                box(plotOutput("plottf_idf", height =800, width=800)))),
      tabItem("worldcloud1",
              fluidRow(
                box(selectInput("selection", "Choose a Question:",
                                choices = questions),
                    actionButton("update", "Change"),
                    
                    sliderInput("freq",
                                "Minimum Frequency:",
                                min = 1,  max = 50, value = 15),
                    sliderInput("max",
                                "Maximum Number of Words:",
                                min = 1,  max = 200,  value = 100),
                    mainPanel(plotOutput("worldcloud1")))))
    )#close DashboardBody
  )#close 
)#close DashboardPage