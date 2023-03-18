# load data & packages ----------------------------------------------------
library(shiny)
library(sf)
library(leaflet)
library(writexl)
library(tidyverse)

allfilter <- function(df, where, what){ 
    if(str_detect(paste0(what, collapse = ""), "Все"))
    {
        df
    } else {
        df %>% 
        filter(str_detect(pull(df, where), paste0(what, collapse = "|")))
    }
}

mask <- function(DF, where, what) { # name = deparse(substitute(what))
    # input <- list()
    # input$name.sel <- c("Все", "Давыдова Ю.А.")
    # DF <- df$points
    if(sum(str_detect(what, "Все")) > 0){
        return(DF)
    } else { 
        DF %>% 
            filter(str_detect(pull(DF, quote(where)), paste0(what, collapse = "|")))
    }
}
    
# data load -------------------------------------------------------
latest <- dir() %>% 
    str_subset(".RData") %>% 
    str_replace_all(".RData", "") %>% 
    str_subset("[:digit:]{4}") %>% 
    as.Date() %>% 
    max() %>% 
    paste0(".RData")
load(latest)

# ui ----------------------------------------------------------------------
ui <- fluidPage(
    titlePanel("Полевые сборы лаборатории экотоксикологии популяций и сообществ"),
    # Левый блок --------------------------------------------------------------
    sidebarLayout(sidebarPanel(
        actionButton("go", "Применить фильтры"), 
        HTML("<br>"),
        sliderInput("years.sel",
                    "Годы сборов:",
                    min = min(choices$years.avl),
                    max = max(choices$years.avl),
                    value = c(min(choices$years.avl), 
                              max(choices$years.avl)
                    ), 
                    step = 1,
                    sep = ""), 
        checkboxInput("photos", "Только с фото площадок"),
        tabsetPanel(
            # Табличная фильтрация ----------------------------------------------------      
            tabPanel("Фильтрация текста", 
                     HTML("<br>"),
                     selectInput("names.sel", 
                                 "Полевики:",
                                 choices = c("Все", choices$name.avl), 
                                 selected = "Все",
                                 multiple = TRUE,
                                 selectize = FALSE
                     ), 
                     HTML("<br>"),
                     selectInput("methods.sel", 
                                 "Методы:",
                                 choices = c("Все", choices$method.avl), 
                                 selected = "Все",
                                 multiple = TRUE,
                                 selectize = FALSE
                     ), 
                     tags$br(),
                     selectInput("tx0.sel", 
                                 "Таксон общо",
                                 choices = c("Все", choices$tx0.avl), 
                                 selected = "Все",
                                 multiple = TRUE,
                                 selectize = FALSE
                     ), 
                     tags$br(),
                     selectInput("tx1.sel", 
                                 "Таксон подробно",
                                 choices = c("Все", choices$tx1.avl),
                                 selected = "Все",
                                 multiple = TRUE,
                                 selectize = FALSE
                     ), 
                     tags$br(),
                     selectInput("prj.sel", 
                                 "Проект",
                                 choices = c("Все", choices$project.avl),
                                 selected = "Все",
                                 multiple = TRUE,
                                 selectize = FALSE
                     ), 
                     tags$i("Для множественного выбора: клик мышью с Ctrl")
            ),
            
            # Пространственная фильтрация ---------------------------------------------
            tabPanel("Фильтрация координат", 
                     HTML("<br>"),
                     checkboxGroupInput("geotype", 
                                        label = "Отображаемая геометрия", 
                                        # choices = c(Точки = "POINT", Линии = "INESTRING", Полигоны = "POLYGON"),
                                        # selected = c("POINT", "INESTRING", "POLYGON")
                                        choices = c(Точки = "point", Линии = "line", Полигоны = "polygon"),
                                        selected = c("point", "line", "polygon")
                     ), 
                     radioButtons("geofilter", 
                                  label = "Границы прстранственного фильтра", 
                                  choices = c(отсутствуют = "no", отрезают = "crop", 
                                              выделяют = "touch"), selected = "no"),
                     h4("Центр и радиус области поиска"),
                     flowLayout(
                         numericInput("NN.sel", "Широта (N)", value = 56.856553, 
                                      min = 50, max = 60, step = 0.01), 
                         numericInput("EE.sel", "Долгота (E)", value = 59.816164 , 
                                      min = 50, max = 60, step = 0.01),
                         numericInput("RR.sel", "Радиус поиска, м", value = 5000, 
                                      min = 100, max = 100000, step = 100)
                     ), 
                     HTML("<br>"), 
                     h4("Дополнительные параметры отображения"),
                     checkboxInput("oopt.on", "Охраняемые природные территории", value = TRUE),
                     checkboxInput("urban.on", "Урбанизированные территории"),
                     checkboxInput("toxic.on", "'Грязные' территории", value = TRUE), 
                     radioButtons("basismap", 
                                  label = "Подложка для карты", 
                                  choices = c(`Open street map` = "osm", 
                                              `ESRI World Imagery` = "esri",
                                              `Topographical map` = "top"), 
                                  selected = "osm")
            )
        )
    ), 
    
    # Основной блок -----------------------------------------------------------
    mainPanel(
        tabsetPanel(
            tabPanel("Карта", 
                     leafletOutput("map1", height = 560)), 
            tabPanel("Результаты поиска", 
                     HTML("<br>"), 
                     verbatimTextOutput("tab1"),
                     downloadButton("dl.results", "Скачать полную таблицу с результатами")),
            tabPanel("Поисковый запрос", 
                     HTML("<br>"), 
                     verbatimTextOutput("years.toprint"),
                     verbatimTextOutput("names.toprint"), 
                     verbatimTextOutput("methods.toprint"), 
                     verbatimTextOutput("tx0.toprint"), 
                     verbatimTextOutput("tx1.toprint"),
                     verbatimTextOutput("prj.toprint"),
                     verbatimTextOutput("geotype.toprint"),
                     verbatimTextOutput("geofilter.toprint"),
                     verbatimTextOutput("coords.toprint")
            ),
            tabPanel("Прочее", 
                     HTML("<br>"),
                     downloadButton("dl.example", "Скачать образец таблицы"),
                     HTML("<br><br>"),
                     # tags$p("download button here"), 
                     tags$p("Руководство (ваши предложения?)"),
                     HTML("<br>"),
                     tags$p("Инфо & контакты. Какую информацию стоит указать? Контакт - кабинет 115, первый этаж ГБК")
            )
        )
    )
    )
)

# server ------------------------------------------------------------------
server <- function(input, output) {
    latest <- dir() %>% 
        str_subset(".RData") %>% 
        str_replace_all(".RData", "") %>% 
        str_subset("[:digit:]{4}") %>% 
        as.Date() %>% 
        max() %>% 
        paste0(".RData")
    load(latest)
    rm(latest)


# reactive input content --------------------------------------------------
    
    years.re <- eventReactive(input$go, {input$years.sel})
    output$years.toprint <- renderPrint({
        cat("Years selected: from ", min(years.re()), " to ", max(years.re()))
    })
    
    names.re <- eventReactive(input$go, {
        if(str_detect(paste0(input$names.sel, collapse = ""), "Все")) {
            choices$name.avl} else { input$names.sel}
    })
    output$names.toprint <- renderPrint({
        cat("Fieldworkers selected: ", paste0(sort(names.re()), collapse = ", "))
    })
    
    methods.re <- eventReactive(input$go, {
        if(str_detect(paste0(input$methods.sel, collapse = ""), "Все")) {
            choices$method.avl} else { input$methods.sel}
    })
    output$methods.toprint <- renderPrint({
        cat("Methods selected: ", paste0(sort(methods.re()), collapse = ", "))
    })
    
    # tx0.re <- eventReactive(input$go, {input$tx0.sel})
    tx0.re <- eventReactive(input$go, {
        if(str_detect(paste0(input$tx0.sel, collapse = ""), "Все")) {
            choices$tx0.avl} else {input$tx0.sel}
    })
    output$tx0.toprint <- renderPrint({
        cat("Taxa 0 selected: ", paste0(sort(tx0.re()), collapse = ", "))
    })
    
    prj.re <- eventReactive(input$go, {
        if(str_detect(paste0(input$prj.sel, collapse = ""), "Все")) {
            choices$project.avl} else { input$prj.sel}
    })
    output$prj.toprint <- renderPrint({
        cat("Project selected: ", paste0(sort(prj.re()), collapse = ", "))
    })
    
    # tx1.re <- eventReactive(input$go, {input$tx1.sel})
    tx1.re <- eventReactive(input$go, {
        if(str_detect(paste0(input$tx1.sel, collapse = ""), "Все")) {
            choices$tx1.avl} else {input$tx1.sel}
    })
    output$tx1.toprint <- renderPrint({
        cat("Taxa 1 selected: ", paste0(sort(tx1.re()), collapse = ", "))
    })
    
    geotype.re <- eventReactive(input$go, {input$geotype})
    output$geotype.toprint <- renderPrint({
        cat("Geometry type selected: ", paste0(geotype.re(), collapse = ", "))
    })
    
    geofilter.re <- eventReactive(input$go, {input$geofilter})
    output$geofilter.toprint <- renderPrint({
        cat("Geofilter applied: ", paste0(sort(geofilter.re()), collapse = ", "))
    })
    
    NN.re <- eventReactive(input$go, {input$NN.sel})
    EE.re <- eventReactive(input$go, {input$EE.sel})
    RR.re <- eventReactive(input$go, {input$RR.sel})
    output$coords.toprint <- renderPrint({
        cat("Center of spatial filtration area (if applied)
:", round(NN.re(), 2), " N, ", round(EE.re(), 2), " E 
in radius of ", round(RR.re()), " m")
    })

# leaflet_base ------------------------------------------------------------
B <- eventReactive(input$go, {
    B <- leaflet()
    if(input$basismap == "osm") { 
        B <- B %>% addTiles() 
    } else if (input$basismap == "esri"){ 
        B <- B %>% addProviderTiles('Esri.WorldImagery')
    } else {
        B <- B %>% addProviderTiles('OpenTopoMap')
    } 
    
    if(input$oopt.on){ 
        B <- B %>% addPolygons(data = basics$oopt, color = "green", fillOpacity = 0, popup = ~name)
    }
    if(input$urban.on){ 
        B <- B %>% addPolygons(data = basics$urban, color = "orange", fillOpacity = 0, popup = ~name)
    }   
    if(input$toxic.on){ 
        B <- B %>% addPolygons(data = basics$toxic, color = "red", fillOpacity = 0, popup = ~name)
    }
    
    B <- B %>% 
        addScaleBar(options = scaleBarOptions(imperial = FALSE))
})

# Table -------------------------------------------------------------------
    
    df2 <- eventReactive(input$go, {
        if(input$photos){
            df.ph <- filter(df1, !is.na(photo))
        } else { 
            df.ph <- df1
        }
        df2 <- df.ph %>% 
            filter(str_detect(.$years, fill_years(paste0(years.re(), collapse = "-")))) %>% 
            filter(str_detect(.$name, paste0(names.re(), collapse = "|"))) %>% 
            filter(str_detect(.$method, paste0(methods.re(), collapse = "|"))) %>% 
            filter(str_detect(.$taxa0, paste0(tx0.re(), collapse = "|"))) %>% 
            filter(str_detect(.$taxa1, paste0(tx1.re(), collapse = "|"))) %>%
            filter(str_detect(.$project, paste0(prj.re(), collapse = "|"))) %>%
            filter(str_detect(.$type, paste0(geotype.re(), collapse = "|"))) 
        if(geofilter.re() == "no") {
            df2
        } else if(geofilter.re() == "crop") {
            circle <- data.frame(x = EE.re(), y = NN.re()) %>%
                st_as_sf(coords = c("x", "y")) %>%
                st_set_crs(4326) %>%
                st_buffer(units::as_units(RR.re(), "m"))
            suppressWarnings(st_intersection(df2, circle))
        } else if(geofilter.re() == "touch"){ 
            circle <- data.frame(x = EE.re(), y = NN.re()) %>%
                st_as_sf(coords = c("x", "y")) %>%
                st_set_crs(4326) %>%
                st_buffer(units::as_units(RR.re(), "m"))
            filter(df2, as_vector(st_intersects(df2, circle, sparse = FALSE))) #touch
        }
        
    })
    
    output$tab1 <- renderPrint({
        df2() %>% 
            as_tibble() %>% 
            st_drop_geometry() %>% 
            select(-geometry, -rem2, -photo) %>% 
            # as.data.frame() %>% 
            str()
    })
    
    output$map1 <- renderLeaflet({
        df3 <- df2()
        df3 <- split(df3, df3$type)
        L <- B()
        if(geofilter.re() != "no") { 
            L <- L %>% addCircles(lng = EE.re(), lat = NN.re(), radius = RR.re(), 
                                  fillOpacity = 0, color = "orange") %>% 
                addCircleMarkers(lng = EE.re(), lat = NN.re(), color = "orange", 
                                 fillOpacity = 0, 
                                 popup = "Центр области поиска")
        }
        if(!is.null(df3$polygon)){if(nrow(df3$polygon) > 0){
            L <- L %>% addPolygons(popup = ~rem2, data = df3$polygon)
        }}
        if(!is.null(df3$line)){if(nrow(df3$line) > 0){
            L <- L %>% addPolylines(popup = ~rem2, data = df3$line)
        }}
        if(!is.null(df3$point)){if(nrow(df3$point) > 0){
            L <- L %>% addCircleMarkers(popup = ~rem2, data = df3$point)
        }}
        L 
    })
    
    output$dl.example <- downloadHandler(
        filename = function() { "Образец.xlsx"},
        content = function(file) {write_xlsx(Example, path = file)}
    )
    output$dl.results <- downloadHandler(
        filename = function() {
            paste0("Результат поиска ", format(Sys.time(), "%d %B %Y, %Hч %Mм"), ".xlsx")
        },
        content = function(file) {write_xlsx(select(as_tibble(df2()), -geometry, -rem2, -photo), path = file)}
    )
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(port = 4004))
