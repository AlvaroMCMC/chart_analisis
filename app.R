library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(shinycssloaders)
library(bslib)
library(RColorBrewer)
library(viridis)

# Tema moderno usando Bootstrap
theme <- bs_theme(
  version = 4,
  bootswatch = "flatly",  # Tema moderno de Bootstrap
  primary = "#2C3E50",    # Color primario (botones, encabezados)
  secondary = "#18BC9C",  # Color secundario (detalles y resaltados)
  success = "#2ECC71",    # Colores adicionales para éxito
  warning = "#F39C12",
  danger = "#E74C3C"
)

ui <- fluidPage(
  theme = theme,  # Aplicar el tema

  # Centramos la imagen y los textos
  tags$head(
    tags$style(HTML("
      .main-panel {
        text-align: center;
      }
      .shiny-plot-output {
        margin: 0 auto;
      }
    "))
  ),

  titlePanel(
    div(
      style = "text-align: center; color: #2C3E50; font-weight: bold;",
      "Visualización de Datos de Excel"
    )
  ),

  sidebarLayout(
    sidebarPanel(
      h4("Configuración de Datos", style = "color: #2C3E50;"),

      # Sección para subir archivo
      fileInput("file1", "Selecciona un archivo Excel",
                accept = c(".xlsx"),
                buttonLabel = "Cargar...",
                placeholder = "No hay archivo seleccionado"),
      tags$hr(style = "border-color: #2C3E50;"),

      # Sección de filtros
      h4("Filtros", style = "color: #2C3E50;"),
      uiOutput("dynamicDateRange"),  # Selector de rango de fechas
      uiOutput("stationFilter"),     # Selector de estaciones

      # Selector de paleta de colores
      tags$hr(style = "border-color: #2C3E50;"),
      h4("Paleta de colores del gráfico", style = "color: #2C3E50;"),
      selectInput("colorPalette", "Elige una paleta de colores:",
                  choices = list("Predeterminada" = "default",
                                 "Viridis" = "viridis",
                                 "Plasma" = "plasma",
                                 "Cividis" = "cividis",
                                 "Brewer: Set1" = "Set1",
                                 "Brewer: Dark2" = "Dark2",
                                 "Brewer: Paired" = "Paired")),

      # Sección para ajustar el gráfico
      tags$hr(style = "border-color: #2C3E50;"),
      h4("Personalización del gráfico", style = "color: #2C3E50;"),
      numericInput("plotWidth", "Ancho del gráfico (cm):",
                   value = 20, min = 5, max = 50, step = 0.5),
      numericInput("plotHeight", "Alto del gráfico (cm):",
                   value = 15, min = 5, max = 50, step = 0.5),
      numericInput("dpi", "Resolución (DPI) para descarga:",
                   value = 300, min = 72, max = 600),
      downloadButton("downloadPlot", "Descargar Gráfico", class = "btn-success"),

      tags$hr(),
      textOutput("errorMessage")  # Mensaje de error si el archivo no es correcto
    ),

    mainPanel(
      div(class = "main-panel",
          h4("Gráfico generado", style = "color: #2C3E50; text-align: center;"),
          plotOutput("plot1", height = "auto", width = "auto") %>% withSpinner(color = "#2C3E50"),

          # Texto automático generado con estadísticas
          h4("Estadísticas del Conjunto de Datos", style = "color: #2C3E50; text-align: center;"),
          textOutput("summaryStats")
      )
    )
  )
)

server <- function(input, output, session) {

  # Leer los datos del archivo Excel y verificar las columnas
  dataInput <- reactive({
    req(input$file1)  # Asegurarse de que el archivo está cargado
    data <- read_excel(input$file1$datapath)

    # Control de calidad de las columnas
    if (!all(c("estacion", "valor", "fecha") %in% colnames(data))) {
      return(NULL)  # Si las columnas necesarias no están, retornar NULL
    }

    # Verificar que la columna valor sea numérica
    if (!is.numeric(data$valor)) {
      return(NULL)
    }

    # Verificar que la columna fecha sea de tipo fecha
    if (!all(class(data$fecha) %in% c("Date", "POSIXct", "POSIXt"))) {
      return(NULL)
    }

    # Verificar que la columna estacion sea de tipo texto
    if (!is.character(data$estacion)) {
      return(NULL)
    }

    # Si pasa las verificaciones, devolver los datos con el formato correcto
    data %>%
      mutate(fecha = as.Date(fecha))  # Asegurarse de que la columna fecha sea Date
  })

  # Mostrar un mensaje de error si los datos no son correctos
  output$errorMessage <- renderText({
    req(input$file1)
    if (is.null(dataInput())) {
      return("Error: Las columnas del archivo deben ser 'estacion' (texto), 'valor' (numérico) y 'fecha' (fecha). Verifica tu archivo.")
    }
    return(NULL)
  })

  # Actualizar dinámicamente el rango de fechas basado en los datos cargados
  output$dynamicDateRange <- renderUI({
    req(dataInput())
    dateRangeInput("dateRange", "Rango de Fechas:",
                   start = min(dataInput()$fecha, na.rm = TRUE),
                   end = max(dataInput()$fecha, na.rm = TRUE))
  })

  # Crear el filtro dinámico para seleccionar estaciones
  output$stationFilter <- renderUI({
    req(dataInput())
    selectInput("selectedStations", "Selecciona Estaciones:",
                choices = c("Todas" = "all", unique(dataInput()$estacion)),
                selected = "all",
                multiple = TRUE)
  })

  # Función para obtener la paleta de colores compatible con ggplot2
  getPalette <- function(palette_name, n) {
    if (palette_name == "default") {
      return(scale_color_discrete())  # Paleta predeterminada de ggplot
    } else if (palette_name %in% c("viridis", "plasma", "cividis")) {
      return(scale_color_viridis_d(option = palette_name))  # Paletas de viridis
    } else if (palette_name %in% rownames(brewer.pal.info)) {
      return(scale_color_brewer(palette = palette_name))  # Paletas de RColorBrewer
    }
  }

  # Filtrar los datos por el rango de fechas y las estaciones seleccionadas
  filteredData <- reactive({
    req(dataInput())
    req(input$dateRange)

    # Filtrar por rango de fechas
    data <- dataInput() %>%
      filter(fecha >= input$dateRange[1], fecha <= input$dateRange[2])

    # Filtrar por estaciones seleccionadas
    if (!"all" %in% input$selectedStations) {
      data <- data %>%
        filter(estacion %in% input$selectedStations)
    }

    return(data)
  })

  # Crear el gráfico con ggplot2
  output$plot1 <- renderPlot({
    req(filteredData())

    # Crear gráfico con ggplot2
    p <- ggplot(filteredData(), aes(x = fecha, y = valor, color = estacion)) +
      geom_point(size = 3) +
      geom_line(aes(group = estacion)) +
      labs(x = "Fecha", y = "Valor", color = "Estación") +
      theme_minimal(base_size = 15) +
      theme(
        plot.background = element_rect(fill = "white", color = "transparent"),
        panel.background = element_rect(fill = "#F7F7F7"),
        panel.grid.major = element_line(color = "#DDDDDD")
      )

    # Aplicar la paleta de colores seleccionada
    p + getPalette(input$colorPalette, length(unique(filteredData()$estacion)))
  }, height = function() { input$plotHeight * 37.7953 },  # Convertir cm a px (1 cm = 37.7953 px)
  width = function() { input$plotWidth * 37.7953 })

  # Generar texto automático con estadísticas (en tono natural y humano)
  output$summaryStats <- renderText({
    req(filteredData())

    data <- filteredData()

    max_val <- max(data$valor, na.rm = TRUE)
    min_val <- min(data$valor, na.rm = TRUE)
    mean_val <- mean(data$valor, na.rm = TRUE)
    count_val <- nrow(data)
    estaciones <- paste(unique(data$estacion), collapse = ", ")

    paste0(
      "Este conjunto de datos contiene ", count_val, " valores registrados. ",
      "El valor más alto observado es de ", max_val, ", mientras que el más bajo es de ", min_val, ". ",
      "En promedio, los valores son de alrededor de ", round(mean_val, 2), ". ",
      "Las estaciones presentes en los datos son: ", estaciones, "."
    )
  })

  # Permitir descargar el gráfico con la misma escala y resolución
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("grafico_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Guardar el gráfico manteniendo las proporciones y la resolución ajustada
      ggsave(file, plot = last_plot(),
             width = input$plotWidth,  # Ancho en cm
             height = input$plotHeight,  # Alto en cm
             dpi = input$dpi, units = "cm")
    }
  )
}

shinyApp(ui = ui, server = server)


