lapply(c("tidyverse", "rvest", "janitor", "scales", "lubridate", "DT",
         "shiny", "rsconnect", "ggiraph"), require, character.only=TRUE)

tablas <- read_html("https://es.wikipedia.org/wiki/Anexo:Encuestas_de_intenci%C3%B3n_de_voto_para_las_elecciones_presidenciales_de_Argentina_de_2023") %>% 
  html_table(fill=TRUE)

# Function to convert Spanish month names to English
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
    date_text <- paste("7 de", date_text)
  }
  
  date_text <- str_trim(date_text)
  spanish_month <- str_extract(date_text, "(?<=de )[[:alpha:]]+")
  english_month <- spanish_to_english_month(spanish_month)
  date_text <- str_replace(date_text, spanish_month, english_month)
  parsed_date <- dmy(date_text)
  
  return(parsed_date)
}


primera <- tablas[1] %>% as.data.frame() %>% 
  slice(2:nrow(.)) %>% 
  setNames(c("fecha", "encuestadora", "muestra", "fdt", "jxc",
             "lla", "cf", "fit", "otros", "blanco", "indecisos", "ventaja")) %>% 
  mutate(fecha = sapply(fecha, extract_and_parse_date) %>% as.Date(origin = "1970-01-01")) %>% 
  mutate(fecha = if_else(fecha>Sys.Date(), Sys.Date()-days(1), fecha))

segunda <- tablas[2] %>% as.data.frame() %>% 
  slice(2:nrow(.)) %>% 
  setNames(c("fecha", "encuestadora", "muestra", "fdt", "jxc",
             "lla", "fit", "otros", "blanco", "indecisos", "ventaja")) %>% 
  slice(-87)  %>% 
  mutate(fecha = sapply(fecha, extract_and_parse_date) %>% as.Date(origin = "1970-01-01"))

ultima <- tablas[3] %>% as.data.frame() %>% 
  slice(2:nrow(.)) %>% 
  setNames(c("fecha", "encuestadora", "muestra", "fdt", "jxc",
             "lla", "fit", "otros", "blanco", "indecisos", "ventaja")) %>% 
  slice(-c(4:6))  %>% 
  mutate(fecha = sapply(fecha, extract_and_parse_date) %>% as.Date(origin = "1970-01-01"))

encuestas <- primera %>% 
  bind_rows(segunda) %>% 
  bind_rows(ultima) %>% 
  mutate_at(vars(4:12), ~ifelse(. == "-", NA, as.numeric(gsub(",", ".", .)))) %>% 
  mutate_at(vars(3), ~ifelse(. == "-", NA, as.numeric(gsub("\\.", "", gsub(",", ".", .))))) %>% 
  mutate(across(c(otros, blanco, indecisos), ~ ifelse(is.na(.), 0, .)),
         obi = otros+blanco+indecisos) %>% 
  select(-c(otros, blanco, indecisos, ventaja))

encuestas_long <- encuestas %>% 
  pivot_longer(cols =4:9, 
               names_to = "party", 
               values_to = "percentage_points")%>%
  mutate(party = case_when(party=="cf" ~ "Hacemos por Nuestro Pais", 
                           party=="fdt" ~ "Unión por la Patria", 
                           party=="fit" ~ "Frente de Izquierda", 
                           party=="jxc" ~ "Juntos por el Cambio", 
                           party=="lla" ~ "La Libertad Avanza", 
                           party=="obi" ~ "Otros - Blanco - Indecisos")) %>% 
  mutate(encuestadora = gsub("\\[\\d+\\]", "", encuestadora))

tercera <- tablas[4] %>% as.data.frame() %>% 
  slice(2:nrow(.)) %>% 
  setNames(c("fecha", "encuestadora", "muestra", "massa", "grabois",
             "bullrich", "larreta", "milei", "bregman", "solano", "schiaretti","moreno", "otros", "blanco", "indecisos")) %>% 
  mutate(fecha = sapply(fecha, extract_and_parse_date) %>% as.Date(origin = "1970-01-01")) %>% 
  mutate(fecha = if_else(fecha>Sys.Date(), Sys.Date()-days(1), fecha)) %>% 
  mutate_at(vars(4:11), ~ifelse(. == "-", NA, as.numeric(gsub(",", ".", .)))) %>% 
  mutate_at(vars(3), ~ifelse(. == "-", NA, as.numeric(gsub("\\.", "", gsub(",", ".", .))))) %>% 
  select(c("fecha", "encuestadora",  "massa", "grabois",
           "larreta", "bullrich")) %>% 
  na.omit() %>% 
  pivot_longer(cols =3:6, 
               names_to = "party", 
               values_to = "percentage_points")%>%
  mutate(coalition = case_when(party=="massa" ~ "Unión por la Patria", 
                               party=="grabois" ~ "Unión por la Patria", 
                               party=="larreta" ~ "Juntos por el Cambio", 
                               party=="bullrich" ~ "Juntos por el Cambio"),
         party = case_when(party=="massa" ~ "Sergio Massa", 
                           party=="grabois" ~ "Juan Grabois", 
                           party=="larreta" ~ "Horacio Rodríguez Larreta", 
                           party=="bullrich" ~ "Patricia Bullrich")) %>% 
  mutate(encuestadora = gsub("\\[\\d+\\]", "", encuestadora))



ui <- fluidPage(
  titlePanel("Visualizador de encuestas - Argentina 2023"),
  tabsetPanel(
    tabPanel("Por partido", 
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("partyInput", "Seleccione partidos", 
                                    choices = c("Unión por la Patria", "Juntos por el Cambio", "La Libertad Avanza", "Frente de Izquierda", "Hacemos por Nuestro Pais", "Otros - Blanco - Indecisos"), 
                                    selected = c("Unión por la Patria", "Juntos por el Cambio", "La Libertad Avanza", "Frente de Izquierda", "Hacemos por Nuestro Pais", "Otros - Blanco - Indecisos")),
                 dateRangeInput("dateRange", "Seleccione fechas",
                                start = min(encuestas_long$fecha, na.rm=TRUE),
                                end = max(encuestas_long$fecha, na.rm=TRUE),
                                min = min(encuestas_long$fecha, na.rm=TRUE),
                                max = max(encuestas_long$fecha, na.rm=TRUE)),
                 checkboxInput("showSE", "Mostrar intervalos de confianza", TRUE),
                 checkboxGroupInput("pollsterInput", "Seleccione encuestadoras", 
                                    choices = unique(encuestas_long$encuestadora),
                                    selected = unique(encuestas_long$encuestadora)),
                 actionButton("selectAll", "Seleccionar todas"),
                 actionButton("unselectAll", "Eliminar todas"),
                 p("La fuente de los datos es ", 
                   a("este artículo.", 
                     href = "https://es.wikipedia.org/wiki/Anexo:Encuestas_de_intenci%C3%B3n_de_voto_para_las_elecciones_presidenciales_de_Argentina_de_2023", 
                     target = "_blank"), 
                   "El código del scrapeo del artículo y la app está disponible en", 
                   a("Github.", 
                     href = "https://github.com/fedetiberti/encuestas_app/blob/main/app_encuestas.R", 
                     target = "_blank"), " Si tenés comentarios o sugerencias, contactame por ", 
                   a("Twitter", 
                     href = "https://twitter.com/fedetiberti", 
                     target = "_blank"), 
                   " o a través de mi ", 
                   a("página web.", 
                     href = "https://fedetiberti.com", 
                     target = "_blank"), 
                   "La inclusión de las encuestas en este agregador no implica un respaldo a sus metodologías ni a la verosimilitud de sus resultados.")
               ),
               mainPanel(
                 ggiraphOutput("pollPlot"),
                 downloadButton("downloadData", "Descargar datos"), 
                 downloadButton("downloadPlot", "Descargar gráfico"),
                 p("Pasando el cursor sobre los puntos del gráfico se puede ver detalles de cada encuesta."),
                 DTOutput("pollTable")
               )
             )
    ),
    tabPanel("Por candidato", 
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("partyInput_internas", "Seleccione el partido", 
                                    choices = c("Juntos por el Cambio", "Unión por la Patria"), 
                                    selected = c("Juntos por el Cambio")),
                 dateRangeInput("dateRange_internas", "Seleccione fechas",
                                start = min(encuestas_long$fecha, na.rm=TRUE),
                                end = max(encuestas_long$fecha, na.rm=TRUE),
                                min = min(encuestas_long$fecha, na.rm=TRUE),
                                max = max(encuestas_long$fecha, na.rm=TRUE)),
                 #    checkboxInput("showSE_internas", "Mostrar intervalos de confianza", FALSE),
                 checkboxGroupInput("pollsterInput_internas", "Seleccione encuestadoras", 
                                    choices = unique(tercera$encuestadora),
                                    selected = unique(tercera$encuestadora)),
                 actionButton("selectAll_internas", "Seleccionar todas"),
                 actionButton("unselectAll_internas", "Eliminar todas"),
                 p("La fuente de los datos es ", 
                   a("este artículo.", 
                     href = "https://es.wikipedia.org/wiki/Anexo:Encuestas_de_intenci%C3%B3n_de_voto_para_las_elecciones_presidenciales_de_Argentina_de_2023", 
                     target = "_blank"), 
                   "El código del scrapeo del artículo y la app está disponible en", 
                   a("Github.", 
                     href = "https://github.com/fedetiberti/encuestas_app/blob/main/app_encuestas.R", 
                     target = "_blank"), " Si tenés comentarios o sugerencias, contactame por ", 
                   a("Twitter", 
                     href = "https://twitter.com/fedetiberti", 
                     target = "_blank"), 
                   " o a través de mi ", 
                   a("página web.", 
                     href = "https://fedetiberti.com", 
                     target = "_blank"), 
                   "La inclusión de las encuestas en este agregador no implica un respaldo a sus metodologías ni a la verosimilitud de sus resultados.")
               ),
               mainPanel(
                 ggiraphOutput("pollPlot_internas"),
                 downloadButton("downloadData_internas", "Descargar datos"), 
                 downloadButton("downloadPlot_internas", "Descargar gráfico"),
                 p("Pasando el cursor sobre los puntos del gráfico se puede ver detalles de cada encuesta."),
                 DTOutput("pollTable_internas")
               )
             )
    ),
    
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$selectAll, {
    updateCheckboxGroupInput(session, "pollsterInput",
                             selected = unique(encuestas_long$encuestadora))
  })
  
  observeEvent(input$unselectAll, {
    updateCheckboxGroupInput(session, "pollsterInput",
                             selected = character(0))
  })
  
  observeEvent(input$selectAll, {
    updateCheckboxGroupInput(session, "pollsterInput_internas",
                             selected = unique(tercera$encuestadora))
  })
  
  observeEvent(input$unselectAll, {
    updateCheckboxGroupInput(session, "pollsterInput_internas",
                             selected = character(0))
  })
  
  # Create a reactive expression for filtered_data
  filtered_data <- reactive({
    encuestas_long %>%
      filter(party %in% input$partyInput,
             fecha >= input$dateRange[1],
             fecha <= input$dateRange[2],
             encuestadora %in% input$pollsterInput)
  })
  
  filtered_data_internas <- reactive({
    tercera %>%
      filter(coalition %in% input$partyInput_internas,
             fecha >= input$dateRange_internas[1],
             fecha <= input$dateRange_internas[2],
             encuestadora %in% input$pollsterInput_internas)
  })
  
  output$pollPlot <- renderggiraph({
    filtered_data <- encuestas_long %>%
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
      p <- ggplot(filtered_data, aes(x = fecha, y = percentage_points, color = party, group = party, tooltip = paste("Partido:", party, "<br>",
                                                                                                                     "Porcentaje:", round(percentage_points, 2), "%<br>",
                                                                                                                     "Encuestadora:", encuestadora, "<br>",
                                                                                                                     "Fecha:", format(fecha, "%Y-%m-%d")))) +
        geom_point_interactive(alpha = 0.5) +
        geom_smooth(method = "loess", se = input$showSE, aes(fill = party), show.legend = FALSE, span=0.5) +
        scale_color_manual(breaks = c("Juntos por el Cambio", "Unión por la Patria", "La Libertad Avanza", "Frente de Izquierda", "Hacemos por Nuestro Pais", "Otros - Blanco - Indecisos"),
                           values = c("yellow3", "steelblue3", "black", "tomato3", "springgreen3", "gray66")) +
        scale_fill_manual(breaks = c("Juntos por el Cambio", "Unión por la Patria", "La Libertad Avanza", "Frente de Izquierda", "Hacemos por Nuestro Pais", "Otros - Blanco - Indecisos"),
                          values = c("yellow3", "steelblue3", "black", "tomato3", "springgreen3", "gray66")) +
        theme_light() +
        scale_y_continuous(labels = scales::label_number(suffix = "%")) +
        scale_x_date(date_labels = "%b-%y") +
        labs(x = "", y = "", color = "", title = "Encuestas electorales sobre las elecciones presidenciales 2023, por partido") +
        theme(plot.title = element_text(face="bold", hjust=0.5),
              legend.position = "bottom", 
              axis.text = element_text(face="bold"), 
              axis.title = element_text(face="bold"), 
              legend.margin = margin(t = -20, b = 0, l = 0, r = 0, unit = "pt")) 
      
      rendered_plot <- ggiraph(code = print(p), width_svg = 9, height_svg = 6)
      rendered_plot <- girafe_options(rendered_plot, opts_tooltip(use_fill=TRUE))
    }
  })
  
  output$pollPlot_internas <- renderggiraph({
    filtered_data <- tercera %>%
      filter(coalition %in% input$partyInput_internas,
             fecha >= input$dateRange_internas[1],
             fecha <= input$dateRange_internas[2],
             encuestadora %in% input$pollsterInput_internas)
    
    if (nrow(filtered_data) == 0) {
      p <- ggplot() +
        labs(title = "No hay datos con esos filtros",
             x = "Fecha",
             y = "Puntos porcentuales") +
        theme_minimal()
      
      return(ggiraph(code = print(p)))
    } else {
      p <- ggplot(filtered_data, aes(x = fecha, y = percentage_points, color = party, group = party, tooltip = paste("Candidato:", party, "<br>",
                                                                                                                     "Porcentaje:", round(percentage_points, 2), "%<br>",
                                                                                                                     "Encuestadora:", encuestadora, "<br>",
                                                                                                                     "Fecha:", format(fecha, "%Y-%m-%d")))) +
        geom_point_interactive(alpha = 0.5, size=3) +
        #   geom_smooth(method = "loess", se = input$showSE_internas, aes(fill = party), show.legend = FALSE, span=0.5) +
        theme_light() +
        scale_y_continuous(labels = scales::label_number(suffix = "%")) +
        scale_x_date(date_labels = "%d-%b") +
        labs(x = "", y = "", color = "", title = "Encuestas electorales sobre la primarias presidenciales de 2023") +
        theme(plot.title = element_text(face="bold", hjust=0.5),
              legend.position = "bottom", 
              axis.text = element_text(face="bold"), 
              axis.title = element_text(face="bold"), 
              legend.margin = margin(t = -20, b = 0, l = 0, r = 0, unit = "pt")) 
      
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
  
  
  output$downloadData_internas <- downloadHandler(
    filename = function() {
      paste("datos_filtrados_encuestas_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data_internas(), file, row.names = FALSE)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot_encuestas_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      filtered_data <- encuestas_long %>%
        filter(party %in% input$partyInput,
               fecha >= input$dateRange[1],
               fecha <= input$dateRange[2],
               encuestadora %in% input$pollsterInput)
      
      
      p <- ggplot(filtered_data, aes(x = fecha, y = percentage_points, color = party, group = party)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "loess", se = input$showSE, aes(fill = party), show.legend = FALSE, span=0.5) +
        scale_color_manual(breaks = c("Juntos por el Cambio", "Unión por la Patria", "La Libertad Avanza", "Frente de Izquierda", "Hacemos por Nuestro Pais", "Otros - Blanco - Indecisos"),
                           values = c("yellow3", "steelblue3", "black", "tomato3", "springgreen3", "gray66")) +
        scale_fill_manual(breaks = c("Juntos por el Cambio", "Unión por la Patria", "La Libertad Avanza", "Frente de Izquierda", "Hacemos por Nuestro Pais", "Otros - Blanco - Indecisos"),
                          values = c("yellow3", "steelblue3", "black", "tomato3", "springgreen3", "gray66")) +
        theme_light() +
        scale_y_continuous(labels = scales::label_number(suffix = "%")) +
        scale_x_date(date_labels = "%b-%y") +
        labs(x = "", y = "", color = "", title = "Encuestas electorales sobre las elecciones presidenciales 2023, por partido") +
        theme(plot.title = element_text(face="bold", hjust=0.5),
              legend.position = "bottom", 
              axis.text = element_text(face="bold"), 
              axis.title = element_text(face="bold"), 
              legend.margin = margin(t = -20, b = 0, l = 0, r = 0, unit = "pt")) 
      ggsave(file, plot = p, width = 9, height = 6, units = "in", dpi = 300)
    }
  )
  
  output$downloadPlot_internas <- downloadHandler(
    filename = function() {
      paste("plot_encuestas_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      filtered_data <- tercera %>%
        filter(coalition %in% input$partyInput_internas,
               fecha >= input$dateRange_internas[1],
               fecha <= input$dateRange_internas[2],
               encuestadora %in% input$pollsterInput_internas)
      
      
      p <- ggplot(filtered_data, aes(x = fecha, y = percentage_points, color = party, group = party)) +
        geom_point(alpha = 0.5, size=3) +
        #   geom_smooth(method = "loess", se = input$showSE_internas, aes(fill = party), show.legend = FALSE, span=0.5) +
        theme_light() +
        scale_y_continuous(labels = scales::label_number(suffix = "%")) +
        scale_x_date(date_labels = "%d-%b") +
        labs(x = "", y = "", color = "", title = "Encuestas electorales sobre la primarias presidenciales de 2023") +
        theme(plot.title = element_text(face="bold", hjust=0.5),
              legend.position = "bottom", 
              axis.text = element_text(face="bold"), 
              axis.title = element_text(face="bold"), 
              legend.margin = margin(t = -20, b = 0, l = 0, r = 0, unit = "pt")) 
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
  
  reshaped_filtered_data_internas <- reactive({
    filtered_data_internas() %>%
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
  
  output$pollTable_internas <- DT::renderDataTable({
    datatable_to_display <- DT::datatable(reshaped_filtered_data_internas() %>% mutate_if(is.numeric, ~round(., digits = 1)),
                                          options = list(order = list(list(0, "desc")), 
                                                         pageLength=20),
                                          rownames = FALSE,
                                          filter = "none")
  })
  
}
