# load data & packages ----------------------------------------------------
library(tidyverse)
library(shiny)
library(sf)
# oopt <- sf::st_read("ural_oopt.gpkg", "all")
library(leaflet)
library(writexl)
fill_years <- function(a){
    str_extract_all(a, "[:digit:]{4}-[:digit:]{4}") %>% 
        map(~str_split(.x, "-")) %>% 
        lapply(function(x)lapply(x, as.numeric)) %>% 
        lapply(function(x){sapply(x, function(z){min(z):max(z)})}) %>% 
        lapply(function(x){if(is.list(x)){x}else{as.list(x)}}) %>% 
        lapply(flatten_int) %>% 
        list(long = ., 
             short = lapply(str_extract_all(a, "[:digit:]{4}"), as.numeric)) %>% 
        transpose() %>% 
        lapply(flatten_int) %>% 
        map(~sort(unique(.x))) %>% 
        map_chr(~paste(.x, collapse = "|"))
}

flt <- function(basal, filtral){ 
    if("Все" %in% filtral){
        rep(T,length(basal))
    }else{
        str_detect(basal, paste0(filtral, collapse = "|"))
    }
}

# data manipulation -------------------------------------------------------
# Ex <- data.frame(who = c("Абвгде Ж.З.", "Ийклмн О.П., Рстуф Х."),
#            N = c("59.876521", "56.123876"),
#            E = c("56.123876", "59.876521"),
#            years = c("2015-2018", "2022"),
#            taxa0 = c("Растения", "Грибы"),
#            taxa1 = c("Травянистые растения", "Шляпочные грибы"),
#            method = c("Геоботанические описания луговой растительности",
#                       "Сбор шляпочных грибов (плодовые тела)"),
#            metadata = c("Учтено общее ПП, высота травостоя, содержание Zn и Cu в почве (А2)",
#                         "Известен общий вес собранных грибов и цена, которую за них дали на рынке"),
#            available = c("Обращайтесь :)", "Нет, сотрудничать не буду"),
#            remarks = c("Описания по травянистому ярусу на площадках 2х2 м",
#                 "Собирали в случаному порядке с помощью корзинки и ножа. Данных нет, грибы съедены")
# )
           

# suppressMessages(
# df <- googlesheets4::range_read(
#     "https://docs.google.com/spreadsheets/d/1tapI6VE2XCQKf5Qw8ypoXJR92RrBorRbAuxYa5zm8IU") %>%
#     mutate_at(c("N","E"), as.numeric) %>%
#     mutate(years2 = fill_years(years),
#         rem2 = paste0(
#         "<b>Собирал кто: </b>", name,
#         "; <br><b>Когда: </b>", years,
#         "; <br><b>Кого: </b>", taxa1, " (", taxa0,")",
#         "; <br><b>Как: </b>", method,
#         "; <br><b>Доступность: </b>", available,
#         "; <br><b>Примечания: </b>", rem, ", ", metadata)) )
# human <- df$name %>%
#     unique() %>%
#     str_split(., ", ") %>%
#     unlist() %>%
#     sort()
# year <- as.numeric(unlist(strsplit(df$years2, "\\|")))
# tx0 <- df$taxa0 %>%
#     unique() %>%
#     str_split(., ", ") %>%
#     unlist() %>%
#     sort()
# tx1 <- df$taxa1 %>%
#     unique() %>%
#     str_split(., ", ") %>%
#     unlist() %>%
#     sort()
# mt <- df$method %>%
#     unique() %>%
#     str_split(., ", ") %>%
#     unlist() %>%
#     sort()
# save.image()

load(".RData")

# ui ----------------------------------------------------------------------
ui <- fluidPage(
    titlePanel("Полевые сборы лаборатории экотоксикологии популяций и сообществ"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("YEAR",
                        "Годы сборов:",
                        min = min(year),
                        max = max(year),
                        value = c(round(quantile(year, 0.1), 0), 
                                  round(quantile(year, 0.9), 0)
                                  ), 
                        step = 1, 
                        sep = ""), 
            tags$br(),
            selectInput("WHO", 
                        "Полевики:",
                        choices = c("Все", human), 
                        selected = "Все",
                        multiple = TRUE,
                        selectize = FALSE
                        ), 
            tags$br(),
            selectInput("MT", 
                        "Методы:",
                        choices = c("Все", mt), 
                        selected = "Все",
                        multiple = TRUE,
                        selectize = FALSE
            ), 
            tags$br(),
            selectInput("TX0", 
                        "Таксон общо",
                        choices = c("Все", tx0), 
                        selected = "Все",
                        multiple = TRUE,
                        selectize = FALSE
            ), 
            tags$br(),
            selectInput("TX1", 
                        "Таксон подробно",
                        choices = c("Все", tx1),
                        selected = "Все",
                        multiple = TRUE,
                        selectize = FALSE
            ), 
            tags$i("Для множественного выбора: клик мышью с Ctrl"), 
            tags$br(), 
            tags$br(), 
            downloadButton("dl", "Скачать образец таблицы")
    ),
        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(
            tabPanel("Finally! Map",
                     # tags$br(),
                     leafletOutput("map1", height = 600),
                     tags$br()),
            tabPanel("Filters applied", 
                tags$h4("years (start...end)"),
                textOutput("aaa"),
                tags$br(),
                tags$h4("name"),
                textOutput("bbb"),
                tags$br(),
                tags$h4("methodology"),
                textOutput("ccc"),
                tags$br(),
                tags$h4("taxa general"),
                textOutput("ddd"),
                tags$br(),
                tags$h4("taxa detailed"),
                textOutput("eee"),
                tags$br(),
                tags$h4("total rows in table was 233, now is:"),
                textOutput("fff"),
                tags$br()),
            tabPanel("Filtered table",
                     tableOutput("tttt"),
                     tags$br())
            
        )
            
        )
    )
)


# server ------------------------------------------------------------------


server <- function(input, output) {
    load(".RData")
    
    DF <- reactive({
        # Y <- c(2012, 2016)
        Y <- paste0(min(input$YEAR):max(input$YEAR), collapse = "|")
        df %>% 

            filter(str_detect(df$years2, Y), 
                   flt(basal = df$name,   filtral = input$WHO),
                   flt(basal = df$method, filtral = input$MT),
                   flt(basal = df$taxa0,  filtral = input$TX0),
                   flt(basal = df$taxa1,  filtral = input$TX1)
            )
        
    })
    
    output$aaa <- renderText({input$YEAR})
    output$bbb <- renderText({input$WHO})
    output$ccc <- renderText({input$MT})
    output$ddd <- renderText({input$TX0})
    output$eee <- renderText({input$TX1})
    output$fff <- renderText({nrow(DF())})
    output$tttt <- renderTable({select(DF(), -rem2, -years2)})
    
    output$map1 <- renderLeaflet({
        leaflet(DF()) %>% 
            addTiles() %>% 
            addPolygons(data = oopt, color = "green", fill = "green") %>%
            addCircleMarkers(lng = ~E, lat = ~N, label = ~name, popup = ~rem2)
    })
    output$dl <- downloadHandler(
        filename = function() { "Образец.xlsx"},
        content = function(file) {write_xlsx(Ex, path = file)}
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
