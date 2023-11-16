#lapply(c("tidyverse", "rvest", "janitor", "scales", "lubridate", "shiny", "rsconnect", "ggiraph"), require, character.only=TRUE)
library(tidyverse)
library(rvest)
library(janitor)
library(scales)
library(lubridate)
library(shiny)
library(rsconnect)
library(ggiraph)
library(textclean)

tablas <- read_html("https://es.wikipedia.org/wiki/Anexo:Encuestas_de_intenci%C3%B3n_de_voto_para_las_elecciones_presidenciales_de_Argentina_de_2023") %>%
 html_table(fill=TRUE)

spanish_to_english_month <- function(spanish_month) {
 month_map <- c("enero" = "January", "febrero" = "February", "marzo" = "March",
                "abril" = "April", "mayo" = "May", "junio" = "June",
                "julio" = "July", "agosto" = "August", "septiembre" = "September",
                "octubre" = "October", "noviembre" = "November", "diciembre" = "December")
 return(month_map[tolower(spanish_month)])
}

extract_and_parse_date <- function(date_text) {
 if (str_detect(date_text, "-")) {
  date_text <- str_split(date_text, "-", simplify = TRUE)[, 2]
 } else if (!str_detect(date_text, "^\\d+ de [[:alpha:]]+ de \\d{4}$")) {
  date_text <- paste("14 de", date_text)
 }

 date_text <- str_trim(date_text)
 spanish_month <- str_extract(date_text, "(?<=de )[[:alpha:]]+")
 english_month <- spanish_to_english_month(spanish_month)
 date_text <- str_replace(date_text, spanish_month, english_month)
 parsed_date <- dmy(date_text)

 return(parsed_date)
}


primera <- tablas[1] %>% as.data.frame() %>%
 slice(1:nrow(.)) %>%
 subset(select=1:12) %>%
 setNames(c("fecha", "encuestadora", "muestra", "fdt", "jxc",
            "lla", "cf", "fit", "otros", "blanco", "indecisos", "ventaja")) %>%
 filter(encuestadora != "5 de agosto de 2023" ) %>% filter(encuestadora != "14 de octubre de 2023" ) %>%
 mutate(fecha = sapply(fecha, extract_and_parse_date) %>% as.Date(origin = "1970-01-01")) %>% filter( !is.na(fecha)) %>%
 mutate(encuestadora = replace_non_ascii(encuestadora))

cuarta <- tablas[7] %>% as.data.frame() %>%
 slice(2:nrow(.)) %>%
 setNames(c("fecha", "encuestadora", "muestra", "lla",
            "fdt", "blanco", "indecisos", "ventaja")) %>%
 mutate(fecha = sapply(fecha, extract_and_parse_date) %>% as.Date(origin = "1970-01-01")) %>% filter( !is.na(fecha)) %>%
 filter(fecha >as.Date("2023-10-22") & fecha != as.Date("2023-12-14")) %>%
 mutate(encuestadora = replace_non_ascii(encuestadora)) %>%
 mutate_at(vars(4:8), ~ifelse(. == "-", 0, as.numeric(gsub(",", ".", .)))) %>%
 mutate_at(vars(3), ~ifelse(. == "-", 0, as.numeric(gsub("\\.", "", gsub(",", ".", .))))) %>%
 mutate(pos=(100-(blanco+indecisos))/100) %>%
 mutate(lla=as.character(lla/pos),fdt=as.character(fdt/pos),muestra=as.character(muestra),blanco=as.character("0"),indecisos=as.character("0"),ventaja=as.character(ventaja))

segunda <- tablas[2] %>% as.data.frame() %>%
 slice(1:nrow(.)) %>%
 setNames(c("fecha", "encuestadora", "muestra", "fdt", "jxc",
            "lla", "fit", "otros", "blanco", "indecisos", "ventaja")) %>%
 mutate(fecha = sapply(fecha, extract_and_parse_date) %>% as.Date(origin = "1970-01-01")) %>% filter( !is.na(fecha)) %>%
 mutate(encuestadora = replace_non_ascii(encuestadora))

tercera <- tablas[3] %>% as.data.frame() %>%
 slice(1:nrow(.)) %>%
 setNames(c("fecha", "encuestadora", "muestra", "fdt", "jxc",
            "lla", "fit", "otros", "blanco", "indecisos", "ventaja")) %>%
 mutate(fecha = sapply(fecha, extract_and_parse_date) %>% as.Date(origin = "1970-01-01")) %>% filter( !is.na(fecha)) %>%
 mutate(encuestadora = replace_non_ascii(encuestadora))

encuestas <- primera %>%
 bind_rows(segunda) %>%
 bind_rows(tercera) %>%
 bind_rows(cuarta) %>%
 #Filtro filas con fechas vacías
 filter(!is.na(fecha)) %>%
 #Filtro encuesta que no es de intención de voto
 filter(fecha!="2023-08-31") %>%
 mutate_at(vars(4:12), ~ifelse(. == "-", NA, as.numeric(gsub(",", ".", .)))) %>%
 mutate_at(vars(3), ~ifelse(. == "-", NA, as.numeric(gsub("\\.", "", gsub(",", ".", .))))) %>%
 mutate(across(c(otros, blanco, indecisos), ~ ifelse(is.na(.), 0, .)),
        obi = otros+blanco+indecisos) %>%
 select(-c(otros, blanco, indecisos, ventaja))

#Proyecto indecisos
encuestas2 <- primera %>%
 bind_rows(segunda) %>%
 bind_rows(tercera) %>%
 bind_rows(cuarta) %>%
 #Filtro filas con fechas vacías
 filter(!is.na(fecha)) %>%
 #Filtro encuesta que no es de intención de voto
 filter(fecha!="2021-12-21") %>%
 mutate_at(vars(4:12), ~ifelse(. == "-", NA, as.numeric(gsub(",", ".", .)))) %>%
 mutate_at(vars(3), ~ifelse(. == "-", NA, as.numeric(gsub("\\.", "", gsub(",", ".", .))))) %>%
 mutate(across(c(otros, blanco, indecisos), ~ ifelse(is.na(.), 0, .)), ob =otros+blanco,
        noind = 100-indecisos,fdtn=fdt/noind*100,jxcn=jxc/noind*100,llan=lla/noind*100,fitn=fit/noind*100,cfn=cf/noind*100,obn=ob/noind*100) %>%
 select(-c(fdt,jxc,lla,fit,cf,indecisos,ob,noind,otros, blanco, ventaja))



datos_sin_proyectar_indecisos <- encuestas %>%
 pivot_longer(cols =4:9,
              names_to = "party",
              values_to = "percentage_points")%>%
 mutate(party = case_when(party=="cf" ~ "Hacemos por Nuestro País",
                          party=="fdt" ~ "Unión por la Patria",
                          party=="fit" ~ "Frente de Izquierda",
                          party=="jxc" ~ "Juntos por el Cambio",
                          party=="lla" ~ "La Libertad Avanza",
                          party=="obi" ~ "Otros - Blanco - Indecisos")) %>%
 mutate(encuestadora = gsub("\\[\\d+\\]", "", encuestadora))

#Copio el dataframe para que fluidPage tenga de donde sacar los datos
encuestas_long<-datos_sin_proyectar_indecisos
#Armo un segundo dataframe con los datos proyectados
datos_proyeccion_indecisos <- encuestas2 %>%
 pivot_longer(cols =4:9,
              names_to = "party",
              values_to = "percentage_points")%>%
 mutate(party = case_when(party=="cfn" ~ "Hacemos por Nuestro País",
                          party=="fdtn" ~ "Unión por la Patria",
                          party=="fitn" ~ "Frente de Izquierda",
                          party=="jxcn" ~ "Juntos por el Cambio",
                          party=="llan" ~ "La Libertad Avanza",
                          party=="obn" ~ "Otros - Blanco - Indecisos")) %>%
 mutate(encuestadora = gsub("\\[\\d+\\]", "", encuestadora))

df_list<-list("datos_proyeccion_indecisos","datos_sin_proyectar_indecisos")
#No funciona el filtro, no sé por qué
enc3<-datos_proyeccion_indecisos %>% select(fecha, encuestadora) %>% unique() %>% group_by(encuestadora) %>% count() %>% filter( n >2 ) %>% pull(encuestadora)
n_post_paso <- encuestas %>% group_by(encuestadora,fecha, muestra) %>% filter(encuestadora %in% enc3 & fecha>=as.Date("2023-08-13")) %>% ungroup() %>% count()
nover25<-if_else(n_post_paso>25, TRUE,FALSE)
ui <- fluidPage(
 sidebarLayout(
  sidebarPanel(
   #Agrego selector de dataframe
   selectInput("data", "Elegir datos",
               choices=df_list, selected=df_list[[1]]),
   checkboxGroupInput("partyInput", "Seleccione partidos",
                      choices = c("Unión por la Patria", "Juntos por el Cambio", "La Libertad Avanza", "Frente de Izquierda", "Hacemos por Nuestro País", "Otros - Blanco - Indecisos"),
                      selected = c("Unión por la Patria", "Juntos por el Cambio", "La Libertad Avanza", "Frente de Izquierda", "Hacemos por Nuestro País", "Otros - Blanco - Indecisos")),
   dateRangeInput("dateRange", "Seleccione fechas",
                  start = as.Date("2023-09-01"),
                  end = max(encuestas_long$fecha, na.rm=TRUE),
                  min = min(encuestas_long$fecha, na.rm=TRUE),
                  max = max(encuestas_long$fecha, na.rm=TRUE)),
   checkboxInput("showSE", "Mostrar intervalos de confianza", TRUE),
   #agrego slider para controlar el suavizado LOESS
   sliderInput("slider", "Suavizado LOESS (span)", 0.1, 0.9, 0.5, step = 0.1),
   sliderInput("slider2", "Suavizado LOESS (span) post PASO", 0.1, 0.9, 0.9, step = 0.1),
   #agrego opción para mostrar aparte los resultados electorales 2021
   checkboxInput("showElection", "Mostrar resultados elecciones 2021 y PASO 2023", FALSE),
   checkboxInput("showGeneral", "Mostrar resultados elecciones generales 2023", TRUE),
   checkboxInput("separate_trends", "Separar tendencias encuestas pre y post elecciones primarias 2023", TRUE),
   checkboxInput("ballotage", "Mostrar tendencias ballotage", TRUE),
   actionButton("selectMore3", "Incluir enc con 3 o + encuestas"),
   actionButton("selectAll", "Incluir todas las encuestadoras (default)"),
   actionButton("unselectAll", "No incluir ninguna encuestadora"),
   checkboxGroupInput("pollsterInput", "Seleccione encuestadoras",
                      choices = sort(unique(encuestas_long$encuestadora)),
                      selected = encuestas_long %>% filter(encuestadora!="Altica") %>% pull(encuestadora) %>% unique()) ,
   p("La fuente de los datos es ",
     a("este artículo.",
       href = "https://es.wikipedia.org/wiki/Anexo:Encuestas_de_intenci%C3%B3n_de_voto_para_las_elecciones_presidenciales_de_Argentina_de_2023",
       target = "_blank"),
     "El código del scrapeo del artículo y la app está disponible en",
     a("Github.",
       href = "https://github.com/rquiroga7/encuestas_app/blob/main/app_encuestas.R",
       target = "_blank"), " Código mantenido por Rodrigo Quiroga, forkeado del repositorio original de ",
     a("Federico Tiberti",
       href = "https://github.com/fedetiberti/encuestas_app/blob/main/app_encuestas.R",
       target = "_blank"),
     "al cuál agregué las opciones de proyectar votos indecisos, comparar con los resultados electorales 2021 y 2023,un slider para ajustar el suavizado, y la posibilidad de aplicar un ajuste separado para las encuestas post-PASO 2023. La inclusión de las encuestas en este agregador no implica un respaldo a sus metodologías ni a la verosimilitud de sus resultados. En las encuestas pre-ballotage se proyectan los blancos e indecisos, y por default no se incluye a la encuestadora Altica, que da resultados muy diferentes del resto y publicó encuestas por primera vez en el período pre-ballotage.")
  ),
  mainPanel(
   ggiraphOutput("pollPlot"),
   downloadButton("downloadData", "Descargar datos"),
   downloadButton("downloadPlot", "Descargar gráfico"),
   p("Pasando el cursor sobre los puntos del gráfico se puede ver detalles de cada encuesta.")#,
   #DTOutput("pollTable")
  )
 )
)


server <- function(input, output, session) {


 observeEvent(input$selectMore3, {
  mydata <- req(encuestas_long())
  updateCheckboxGroupInput(session, "pollsterInput",
                           selected = mydata %>% filter( encuestadora %in% enc3 | encuestadora %in% c("Elecciones legislativas","Elecciones primarias" ,"Elecciones generales") ) %>% pull(encuestadora) %>% unique())
 })
 observeEvent(input$selectAll, {
  mydata <- req(encuestas_long())
  updateCheckboxGroupInput(session, "pollsterInput",
                           selected = sort(unique(mydata$encuestadora)))
 })

 observeEvent(input$unselectAll, {
  updateCheckboxGroupInput(session, "pollsterInput",
                           selected = mydata %>% filter( encuestadora %in% c("Elecciones legislativas","Elecciones primarias" ,"Elecciones generales") ) %>% pull(encuestadora) %>% unique())
 })
 #eventReactive que actua sobre el dataframe seleccionado
 encuestas_long <- eventReactive(input$data, {
  get(input$data)
 })
 # Create a reactive expression for filtered_data
 filtered_data <- reactive({
  req(encuestas_long()) %>%
   filter(party %in% input$partyInput,
          fecha >= input$dateRange[1],
          fecha <= input$dateRange[2],
          encuestadora %in% input$pollsterInput)
 })

 output$pollPlot <- renderggiraph({
  filtered_data <-  req(encuestas_long()) %>%
   filter(party %in% input$partyInput,
          fecha >= input$dateRange[1],
          fecha <= input$dateRange[2],
          encuestadora %in% input$pollsterInput)

  if (nrow(filtered_data) == 0) {
   p <- ggplot() +
    labs(title = "No hay datos con esos filtros",
         x = "Fecha",
         y = "Puntos porcentuales") +
    theme_minimal()

   return(ggiraph(code = print(p)))
  } else {
   p <- ggplot(filtered_data(), aes(x = fecha, y = percentage_points, color = party, group = party, tooltip = paste("Partido:", party, "<br>",
                                                                                                                    "Porcentaje:", round(percentage_points, 2), "%<br>",
                                                                                                                    "Encuestadora:", encuestadora, "<br>",
                                                                                                                    "Fecha:", format(fecha, "%Y-%m-%d")))) +
    geom_point_interactive(data=subset(filtered_data(), (fecha<as.Date("2023-10-23") & encuestadora!="Elecciones legislativas" & encuestadora != "Elecciones generales" & encuestadora!= "Elecciones primarias")),alpha = 0.5) +
    geom_point_interactive(data=subset(filtered_data(), (fecha>as.Date("2023-10-23") & input$ballotage==TRUE & encuestadora!="Elecciones legislativas" & encuestadora != "Elecciones generales" & encuestadora!= "Elecciones primarias")),alpha = 0.5) +
    geom_point_interactive(data=subset(filtered_data(), (encuestadora=="Elecciones legislativas" |  encuestadora=="Elecciones primarias") & input$showElection == TRUE),alpha = 1,size=4,shape=19) +
    geom_point_interactive(data=subset(filtered_data(), (encuestadora=="Elecciones legislativas" |  encuestadora=="Elecciones primarias") & input$showElection == TRUE),alpha = 1,size=6,shape=1) +
    geom_point_interactive(data=subset(filtered_data(), ( encuestadora=="Elecciones generales" ) & input$showGeneral == TRUE),alpha = 1,size=4,shape=19) +
    geom_point_interactive(data=subset(filtered_data(), ( encuestadora=="Elecciones generales" ) & input$showGeneral == TRUE),alpha = 1,size=6,shape=1) +

    geom_smooth(data=subset(filtered_data(), (input$separate_trends==FALSE & encuestadora!="Elecciones legislativas" & encuestadora != "Elecciones generales" & encuestadora!= "Elecciones primarias")),method = "loess", se = input$showSE, aes(fill = party), show.legend = FALSE, span=input$slider) +
    geom_smooth(data=subset(filtered_data(), (fecha<=as.Date("2023-08-13") & input$separate_trends==TRUE & encuestadora!="Elecciones legislativas" & encuestadora != "Elecciones generales" & encuestadora!= "Elecciones primarias")),method = "loess", se = input$showSE, aes(fill = party), show.legend = FALSE, span=input$slider) +
    geom_smooth(data=subset(filtered_data(), (fecha>as.Date("2023-08-13") & fecha<as.Date("2023-10-23")  & input$separate_trends==TRUE & encuestadora!="Elecciones legislativas" & encuestadora != "Elecciones generales" & encuestadora!= "Elecciones primarias")),method = "loess", se = input$showSE, aes(fill = party), show.legend = FALSE, span=input$slider2,family = "symmetric") +
    geom_smooth(data=subset(filtered_data(), (fecha>as.Date("2023-10-23")  & input$separate_trends==TRUE & input$ballotage==TRUE & encuestadora!="Elecciones legislativas" & encuestadora != "Elecciones generales" & encuestadora!= "Elecciones primarias")),method = "loess", se = input$showSE, aes(fill = party), show.legend = FALSE, span=input$slider2,family = "symmetric") +

    scale_color_manual(breaks = c("Juntos por el Cambio", "Unión por la Patria", "La Libertad Avanza", "Frente de Izquierda", "Hacemos por Nuestro País", "Otros - Blanco - Indecisos"),
                       values = c("#FFDA00", "#009FE3", "purple", "tomato3", "#283084", "gray66")) +
    scale_fill_manual(breaks = c("Juntos por el Cambio", "Unión por la Patria", "La Libertad Avanza", "Frente de Izquierda", "Hacemos por Nuestro País", "Otros - Blanco - Indecisos"),
                      values = c("#FFDA00", "#009FE3", "purple", "tomato3", "#283084", "gray66")) +
    theme_light() +
    scale_y_continuous(breaks = seq(0, 60, by = 5), minor_breaks = NULL ,labels = scales::label_number(suffix = "%")) +
    scale_x_date(date_labels = "%d-%b-%y") +
    labs(x = "", y = "", color = "", title = "Tendencias del agregado de Encuestas Electorales  2023") +
    theme(plot.title = element_text(face="bold", hjust=0.5),
          legend.position = "bottom",
          axis.text = element_text(face="bold"),
          axis.title = element_text(face="bold"),
          legend.margin = margin(t = -20, b = 0, l = 0, r = 0, unit = "pt"))
   if (input$showElection == TRUE){
    p<-p + annotate("text", x = as.Date("2023-08-13"), y = 18,label="Elecciones primarias",angle=90,vjust=0.5)
   }
   if (input$showGeneral == TRUE & input$ballotage==FALSE ){
    p<-p + annotate("text", x = as.Date("2023-10-22"), y = 15.5,label="Elecciones generales",angle=90,vjust=0.5)
   }
   if (input$showGeneral == TRUE & input$ballotage==TRUE ){
    p<-p + annotate("text", x = as.Date("2023-10-22"), y = 48,label="Elecciones generales",angle=90,vjust=0.5)
   }
   rendered_plot <- ggiraph(code = print(p), width_svg = 9, height_svg = 6)
   rendered_plot <- girafe_options(rendered_plot, opts_tooltip(use_fill=TRUE))
  }
 })


 output$downloadData <- downloadHandler(
  filename = function() {
   paste("datos_filtrados_encuestas_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
   write.csv(filtered_data(), file, row.names = FALSE)
  }
 )

 output$downloadPlot <- downloadHandler(
  filename = function() {
   paste("plot_encuestas_", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
   plot_data<-req(encuestas_long()) %>%
    filter(party %in% input$partyInput,
           fecha >= input$dateRange[1],
           fecha <= input$dateRange[2],
           encuestadora %in% input$pollsterInput)


   p <- ggplot(filtered_data(), aes(x = fecha, y = percentage_points, color = party, group = party)) +
    geom_point_interactive(data=subset(filtered_data(), (fecha<as.Date("2023-10-23") & encuestadora!="Elecciones legislativas" & encuestadora != "Elecciones generales" & encuestadora!= "Elecciones primarias")),alpha = 0.5) +
    geom_point_interactive(data=subset(filtered_data(), (fecha>as.Date("2023-10-23") & input$ballotage==TRUE & encuestadora!="Elecciones legislativas" & encuestadora != "Elecciones generales" & encuestadora!= "Elecciones primarias")),alpha = 0.5) +
    geom_point_interactive(data=subset(filtered_data(), (encuestadora=="Elecciones legislativas" |  encuestadora=="Elecciones primarias") & input$showElection == TRUE),alpha = 1,size=4,shape=19) +
    geom_point_interactive(data=subset(filtered_data(), (encuestadora=="Elecciones legislativas" |  encuestadora=="Elecciones primarias") & input$showElection == TRUE),alpha = 1,size=6,shape=1) +
    geom_point_interactive(data=subset(filtered_data(), ( encuestadora=="Elecciones generales" ) & input$showGeneral == TRUE),alpha = 1,size=4,shape=19) +
    geom_point_interactive(data=subset(filtered_data(), ( encuestadora=="Elecciones generales" ) & input$showGeneral == TRUE),alpha = 1,size=6,shape=1) +
    geom_smooth(data=subset(filtered_data(), (input$separate_trends==FALSE & encuestadora!="Elecciones legislativas" & encuestadora != "Elecciones generales" & encuestadora!= "Elecciones primarias")),method = "loess", se = input$showSE, aes(fill = party), show.legend = FALSE, span=input$slider) +
    geom_smooth(data=subset(filtered_data(), (fecha<=as.Date("2023-08-13") & input$separate_trends==TRUE & encuestadora!="Elecciones legislativas" & encuestadora != "Elecciones generales" & encuestadora!= "Elecciones primarias")),method = "loess", se = input$showSE, aes(fill = party), show.legend = FALSE, span=input$slider) +
    geom_smooth(data=subset(filtered_data(), (fecha>as.Date("2023-08-13") & fecha<as.Date("2023-10-23")  & input$separate_trends==TRUE & encuestadora!="Elecciones legislativas" & encuestadora != "Elecciones generales" & encuestadora!= "Elecciones primarias")),method = "loess", se = input$showSE, aes(fill = party), show.legend = FALSE, span=input$slider2,family = "symmetric") +
    geom_smooth(data=subset(filtered_data(), (fecha>as.Date("2023-10-23")  & input$separate_trends==TRUE & input$ballotage==TRUE & encuestadora!="Elecciones legislativas" & encuestadora != "Elecciones generales" & encuestadora!= "Elecciones primarias")),method = "loess", se = input$showSE, aes(fill = party), show.legend = FALSE, span=input$slider2,family = "symmetric") +
    scale_color_manual(breaks = c("Juntos por el Cambio", "Unión por la Patria", "La Libertad Avanza", "Frente de Izquierda", "Hacemos por Nuestro País", "Otros - Blanco - Indecisos"),
                       values = c("#FFDA00", "#009FE3", "purple", "tomato3", "#283084", "gray66")) +
    scale_fill_manual(breaks = c("Juntos por el Cambio", "Unión por la Patria", "La Libertad Avanza", "Frente de Izquierda", "Hacemos por Nuestro País", "Otros - Blanco - Indecisos"),
                      values = c("#FFDA00", "#009FE3", "purple", "tomato3", "#283084", "gray66")) +
    theme_light() +
    scale_y_continuous(breaks = seq(0, 60, by = 5), minor_breaks = NULL ,labels = scales::label_number(suffix = "%")) +
    scale_x_date(date_labels = "%d-%b-%y") +
    labs(x = "", y = "", color = "", title = "Encuestas electorales sobre la primera vuelta presidencial de 2023") +
    theme(plot.title = element_text(face="bold", hjust=0.5),
          legend.position = "bottom",
          axis.text = element_text(face="bold"),
          axis.title = element_text(face="bold"),
          legend.margin = margin(t = -20, b = 0, l = 0, r = 0, unit = "pt"))
   if (input$showElection == TRUE){
    p<-p + annotate("text", x = as.Date("2023-08-13"), y = 18,label="Elecciones primarias",angle=90,vjust=0.5)
   }
   if (input$showGeneral == TRUE & input$ballotage==FALSE ){
    p<-p + annotate("text", x = as.Date("2023-10-22"), y = 15.5,label="Elecciones generales",angle=90,vjust=0.5)
   }
   if (input$showGeneral == TRUE & input$ballotage==TRUE ){
    p<-p + annotate("text", x = as.Date("2023-10-22"), y = 48,label="Elecciones generales",angle=90,vjust=0.5)
   }
   ggsave(file, plot = p, width = 9, height = 6, units = "in", dpi = 300)
  }
 )

 reshaped_filtered_data <- reactive({
  filtered_data() %>%
   rename(Fecha = fecha, Encuestadora = encuestadora) %>%
   group_by(Fecha, party, Encuestadora) %>%
   mutate(row_num = row_number()) %>%
   ungroup() %>%
   pivot_wider(names_from = party,
               values_from = percentage_points,
               id_cols = c(Fecha, Encuestadora, row_num)) %>%
   relocate(Fecha, Encuestadora) %>%
   select(-row_num)
 })

 output$pollTable <- DT::renderDataTable({
  datatable_to_display <- DT::datatable(reshaped_filtered_data() %>% mutate_if(is.numeric, ~round(., digits = 1)),
                                        options = list(order = list(list(0, "desc")),
                                                       pageLength=20),
                                        rownames = FALSE,
                                        filter = "none")
 })
}

shinyApp(ui = ui, server = server)
