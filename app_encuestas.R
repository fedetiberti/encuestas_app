lapply(c("tidyverse", "rvest", "janitor", "scales", "lubridate", 
         "shiny", "rsconnect", "ggiraph"), require, character.only=TRUE)

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
  slice(2:nrow(.)) %>% 
  setNames(c("fecha", "encuestadora", "muestra", "fdt", "jxc",
             "lla", "fit", "cf", "otros", "blanco", "indecisos", "ventaja")) %>% 
  mutate(fecha = sapply(fecha, extract_and_parse_date) %>% as.Date(origin = "1970-01-01"))

segunda <- tablas[2] %>% as.data.frame() %>% 
  slice(2:nrow(.)) %>% 
  setNames(c("fecha", "encuestadora", "muestra", "fdt", "jxc",
             "lla", "fit", "otros", "blanco", "indecisos", "ventaja")) %>% 
  slice(-87)  %>% 
  mutate(fecha = sapply(fecha, extract_and_parse_date) %>% as.Date(origin = "1970-01-01"))


encuestas <- primera %>% 
  bind_rows(segunda) %>% 
  mutate_at(vars(4:12), ~ifelse(. == "-", NA, as.numeric(gsub(",", ".", .)))) %>% 
  mutate_at(vars(3), ~ifelse(. == "-", NA, as.numeric(gsub("\\.", "", gsub(",", ".", .))))) %>% 
  mutate(across(c(otros, blanco, indecisos), ~ ifelse(is.na(.), 0, .)),
         obi = otros+blanco+indecisos) %>% 
  select(-c(otros, blanco, indecisos, ventaja))

encuestas_long <- encuestas %>% 
  pivot_longer(cols =4:9, 
               names_to = "party", 
               values_to = "percentage_points")%>%
mutate(party = case_when(party=="cf" ~ "Consenso Federal", 
                         party=="fdt" ~ "Frente de Todos", 
                         party=="fit" ~ "Frente de Izquierda", 
                         party=="jxc" ~ "Juntos por el Cambio", 
                         party=="lla" ~ "La Libertad Avanza", 
                         party=="obi" ~ "Otros - Blanco - Indecisos")) %>% 
  mutate(encuestadora = gsub("\\[\\d+\\]", "", encuestadora))


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("partyInput", "Seleccione partidos", 
                         choices = c("Frente de Todos", "Juntos por el Cambio", "La Libertad Avanza", "Frente de Izquierda", "Consenso Federal", "Otros - Blanco - Indecisos"), 
                         selected = c("Frente de Todos", "Juntos por el Cambio", "La Libertad Avanza", "Frente de Izquierda", "Consenso Federal", "Otros - Blanco - Indecisos")),
      dateRangeInput("dateRange", "Seleccione fechas",
                     start = min(encuestas_long$fecha),
                     end = max(encuestas_long$fecha),
                     min = min(encuestas_long$fecha),
                     max = max(encuestas_long$fecha)),
      checkboxInput("showSE", "Mostrar intervalos de confianza", TRUE),
      checkboxGroupInput("pollsterInput", "Seleccione encuestadoras", 
                         choices = unique(encuestas_long$encuestadora),
                         selected = unique(encuestas_long$encuestadora)),
      actionButton("selectAll", "Seleccionar todas"),
      actionButton("unselectAll", "Eliminar todas")
    ),
    mainPanel(
      ggiraphOutput("pollPlot"),
      downloadButton("downloadData", "Descargar datos"), 
      downloadButton("downloadPlot", "Descargar gráfico"),
      p("Pasando el cursor sobre los puntos del gráfico se puede ver detalles de cada encuesta. La fuente de los datos es ", 
        a("este artículo de Wikipedia", 
          href = "https://es.wikipedia.org/wiki/Anexo:Encuestas_de_intenci%C3%B3n_de_voto_para_las_elecciones_presidenciales_de_Argentina_de_2023", 
          target = "_blank"), 
        ". El código del scrapeo de Wikipedia y la app está disponible en", 
        a("Github.", 
          href = "https://github.com/fedetiberti/encuestas_app/blob/main/app_encuestas.R", 
          target = "_blank")),
      DTOutput("pollTable")
    )
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
  
  # Create a reactive expression for filtered_data
  filtered_data <- reactive({
    encuestas_long %>%
      filter(party %in% input$partyInput,
             fecha >= input$dateRange[1],
             fecha <= input$dateRange[2],
             encuestadora %in% input$pollsterInput)
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
        geom_smooth(method = "loess", se = input$showSE, aes(fill = party), show.legend = FALSE) +
        scale_color_manual(breaks = c("Juntos por el Cambio", "Frente de Todos", "La Libertad Avanza", "Frente de Izquierda", "Consenso Federal", "Otros - Blanco - Indecisos"),
                           values = c("yellow3", "steelblue3", "black", "tomato3", "springgreen3", "gray66")) +
        scale_fill_manual(breaks = c("Juntos por el Cambio", "Frente de Todos", "La Libertad Avanza", "Frente de Izquierda", "Consenso Federal", "Otros - Blanco - Indecisos"),
                          values = c("yellow3", "steelblue3", "black", "tomato3", "springgreen3", "gray66")) +
        theme_light() +
        scale_y_continuous(labels = scales::label_number(suffix = "%")) +
        scale_x_date(date_labels = "%b-%y") +
        labs(x = "", y = "", color = "", title = "Encuestas electorales sobre la primera vuelta presidencial de 2023") +
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
        geom_smooth(method = "loess", se = input$showSE, aes(fill = party), show.legend = FALSE) +
        scale_color_manual(breaks = c("Juntos por el Cambio", "Frente de Todos", "La Libertad Avanza", "Frente de Izquierda", "Consenso Federal", "Otros - Blanco - Indecisos"),
                           values = c("yellow3", "steelblue3", "black", "tomato3", "springgreen3", "gray66")) +
        scale_fill_manual(breaks = c("Juntos por el Cambio", "Frente de Todos", "La Libertad Avanza", "Frente de Izquierda", "Consenso Federal", "Otros - Blanco - Indecisos"),
                          values = c("yellow3", "steelblue3", "black", "tomato3", "springgreen3", "gray66")) +
        theme_light() +
        scale_y_continuous(labels = scales::label_number(suffix = "%")) +
        scale_x_date(date_labels = "%b-%y") +
        labs(x = "", y = "", color = "", title = "Encuestas electorales sobre la primera vuelta presidencial de 2023") +
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
      group_by(fecha, party, encuestadora) %>%
      summarize(percentage_points = mean(percentage_points, na.rm = TRUE)) %>%
      ungroup() %>%
      spread(key = party, value = percentage_points) %>%
      rename(JxC = `Juntos por el Cambio`,
             FdT = `Frente de Todos`,
             LLA = `La Libertad Avanza`,
             FI = `Frente de Izquierda`,
             CF = `Consenso Federal`,
             `Otros/Indecisos` = `Otros - Blanco - Indecisos`) %>% 
      relocate(fecha, encuestadora, JxC, FdT, LLA, FI, CF,`Otros/Indecisos` )
  })
  
  output$pollTable <- DT::renderDataTable({
    datatable_to_display <- DT::datatable(reshaped_filtered_data(),
                                          options = list(order = list(list(0, "desc")), 
                                                         pageLength=20),
                                          rownames = FALSE,
                                          filter = "none")
    
    DT::formatRound(datatable_to_display, columns = 3:8, digits = 1)
  })
}
