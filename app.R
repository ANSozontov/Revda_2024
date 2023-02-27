# load data & packages ----------------------------------------------------
library(shiny)
library(sf)
library(leaflet)
library(writexl)
library(tidyverse)

load(".RData")

# ui ----------------------------------------------------------------------
ui <- fluidPage(
    titlePanel("Полевые сборы лаборатории экотоксикологии популяций и сообществ"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("years.sel",
                        "Годы сборов:",
                        min = min(years.avl),
                        max = max(years.avl),
                        value = c(round(quantile(years.avl, 0.8), 0), 
                                  round(quantile(years.avl, 1.0), 0)
                                  ), 
                        step = 1,
                        sep = ""), 
            tags$br(),
            selectInput("name.sel", 
                        "Полевики:",
                        choices = c("Все", name.avl), 
                        selected = "Все",
                        multiple = TRUE,
                        selectize = FALSE
                        ), 
            tags$br(),
            selectInput("method.sel", 
                        "Методы:",
                        choices = c("Все", method.avl), 
                        selected = "Все",
                        multiple = TRUE,
                        selectize = FALSE
            ), 
            tags$br(),
            selectInput("tx0.sel", 
                        "Таксон общо",
                        choices = c("Все", tx0.avl), 
                        selected = "Все",
                        multiple = TRUE,
                        selectize = FALSE
            ), 
            tags$br(),
            selectInput("tx1.sel", 
                        "Таксон подробно",
                        choices = c("Все", tx1.avl),
                        selected = "Все",
                        multiple = TRUE,
                        selectize = FALSE
            ), 
            tags$i("Для множественного выбора: клик мышью с Ctrl"), 
            tags$br(),
            tags$br(),
            checkboxGroupInput("geometrytype", label = "Геометрия", 
                choices = c(Точки = "points", Линии = "line", Полигоны = "polygon"),
                selected = c("points", "line", "polygon"))
    ),
        # Show a plot of the generated distribution
        mainPanel(
            # textOutput("yyyy"),
            leafletOutput("map1"),
            tags$br(),
            downloadButton("dl", "Скачать образец таблицы")
        )
    )
)


# server ------------------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {
    load(".RData")
    
    output$yyyy <- renderPrint({input$geometrytype})
    
    output$map1 <- renderLeaflet({
        df4 <- df %>% 
            allfilter("years", fill_years(paste0(input$years.sel, collapse = "-"))) %>% 
            allfilter("name",  input$name.sel)   %>% 
            allfilter("method",input$method.sel) %>% 
            allfilter("taxa0", input$tx0.sel)    %>% 
            allfilter("taxa1", input$tx1.sel) %>% 
            allfilter("type", input$geometrytype)
        
        leaflet() %>% 
            addTiles() %>% 
            addPolygons(popup = ~rem2, 
                        data = filter(df4, grepl("POLYGON", st_geometry_type(geometry)))) %>% 
            addPolylines(popup = ~rem2, 
                         data = filter(df4, grepl("LINE", st_geometry_type(geometry)))) %>% 
            addCircleMarkers(popup = ~rem2, lng = ~E, lat = ~N,
                             data = mutate_at(filter(df4, grepl("POINT", st_geometry_type(geometry))), 
                                              c("N", "E"), as.numeric))
    })
    
    output$dl <- downloadHandler(
        filename = function() { "Образец.xlsx"},
        content = function(file) {write_xlsx(Example, path = file)}
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
