# a. cargar libs
library(shiny)
library(tidyverse)
library(stringi)
library(stringr)
library(lubridate)

# a. leer db
nombres <- names(datos_raw <- read_csv("Basecovid.txt", locale = locale(encoding = "UTF-8"), col_types = cols(.default = "c")))

# habia un desplazamiento de columnas, use map_dfc para correrlas y dejar la que sobraba al final
df <- map_dfc(seq_len(ncol(datos_raw)-1), ~ datos_raw[[.x + 1]]) %>%
  set_names(nombres[1:(length(nombres)-1)])
df <- bind_cols(df, ultima = datos_raw[[1]])
names(df)[ncol(df)] <- nombres[length(nombres)]

# test
print("primeras columnas tras el corrimiento")
print(head(df[1:5]))

# limpiar edad y texto
# pasar fechas a formato dd/mm/aa con lubridate
fechas_cols <- c("fecha_inicio_sintomas", "fecha_apertura", "fecha_internacion", "fecha_cui_intensivo", "fecha_fallecimiento", "fecha_diagnostico")

df_clean <- df %>%
  mutate(
    edad = if_else(str_detect(edad, "^\\d+$"), edad, edad_años_meses),
    edad = str_remove_all(edad, "\\s*años?"),
    edad = str_replace_all(edad, "\\.", ""),
    edad = as.numeric(edad)
  ) %>%
  filter(!is.na(edad)) %>%
  mutate(
    provincia = residencia_provincia_nombre %>% stri_trans_general("Latin-ASCII") %>% tolower() %>% str_trim(),
    pais = residencia_pais_nombre %>% stri_trans_general("Latin-ASCII") %>% tolower() %>% str_trim(),
    sexo = sexo %>% stri_trans_general("Latin-ASCII") %>% tolower() %>% str_trim()
  ) %>%
  mutate(across(all_of(fechas_cols), ~ format(ymd(.x), "%d/%m/%Y")))

# test a
print("edades limpias")
print(head(df_clean$edad))
print("formato DD/MM/AA fechas limpias:")
print(head(df_clean[, fechas_cols]))

# b. columnas de interes, por nombre y pos
base_select <- df_clean %>%
  select(
    sexo,             
    edad,             
    provincia,        
    5,         # (residencia_pais_nombre)
    9          # (fecha_inicio_sintomas)
  )

# test b
print("Contenido de base_select:")
print(head(base_select))

# c. 3 provs y sexo f
base_3prov <- df_clean %>%
  filter(
    provincia %in% c("buenos aires", "caba", "neuquen"),
    sexo == "f"
  )

# test c
print("Datos filtrados (base_3prov):")
print(head(base_3prov))

# d. rangos etarios
dbase_rangos <- base_3prov %>%
  mutate(
    rango_edad = case_when(
      edad <= 17 ~ "0-17",
      edad <= 30 ~ "18-30",
      edad <= 45 ~ "31-45",
      edad <= 64 ~ "46-64",
      TRUE ~ "65+"
    )
  )

# test d
print("Datos con rango_edad:")
print(head(dbase_rangos))

# e. edad promedio min max por prov
estadisticas_provincias <- dbase_rangos %>%
  group_by(provincia) %>%
  summarise(
    edad_media = mean(edad, na.rm = TRUE),
    edad_minima = min(edad, na.rm = TRUE),
    edad_maxima = max(edad, na.rm = TRUE),
    .groups = "drop"
  )

# test e
print("estadisticas por provincia")
print(estadisticas_provincias)

# f. contar y peso relativo por prov
base_3_provincias <- dbase_rangos %>%
  group_by(provincia) %>%
  summarise(
    cantidad_personas = n(),
    .groups = "drop"
  ) %>%
  mutate(
    peso_relativo = cantidad_personas / sum(cantidad_personas)
  )

# test f
print("peso relativo por provincia")
print(base_3_provincias)

#############################################################################################

# ui
ui <- fluidPage(
  titlePanel("Análisis COVID por Provincia"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("prov_seleccionadas", "Seleccionar Provincias", 
                         choices = unique(df_clean$provincia),
                         selected = c("buenos aires", "caba", "neuquen")),
      selectInput("sexo", "Sexo", choices = c("f", "m"), selected = "f")
    ),
    
    mainPanel(
      uiOutput("mensaje_error"),
      
      h3("Distribución de Rangos Etarios"),
      plotOutput("plot_rangos"),
      
      h3("Estadísticas por Provincia"),
      tableOutput("tabla_estadisticas"),
      
      h3("Peso Relativo por Provincia"),
      tableOutput("tabla_peso")
    )
  )
)

server <- function(input, output) {
  datos_filtrados <- reactive({
    # detiene y muestra mensaje si no hay provincias seleccionadas
    req(length(input$prov_seleccionadas) > 0)
    df_clean %>%
      filter(provincia %in% input$prov_seleccionadas, sexo == input$sexo) %>%
      mutate(
        rango_edad = case_when(
          edad <= 17 ~ "0-17",
          edad <= 30 ~ "18-30",
          edad <= 45 ~ "31-45",
          edad <= 64 ~ "46-64",
          TRUE ~ "65+"
        )
      )
  })
  
  # render del mensaje de error
  output$mensaje_error <- renderUI({
    if (length(input$prov_seleccionadas) == 0) {
      tags$div(
        "⚠️ Por favor seleccione al menos una provincia para ver los gráficos y tablas.",
        style = "color: red; font-weight: bold; margin-bottom: 20px;"
      )
    }
  })
  
  output$plot_rangos <- renderPlot({
    datos_filtrados() %>%
      count(provincia, rango_edad) %>%
      ggplot(aes(x = rango_edad, y = n, fill = provincia)) +
      geom_col(position = "dodge") +
      labs(x = "Rango Etario", y = "Cantidad", title = "Distribución de edades")
  })
  
  output$tabla_estadisticas <- renderTable({
    datos_filtrados() %>%
      group_by(provincia) %>%
      summarise(
        edad_media = mean(edad, na.rm = TRUE),
        edad_minima = min(edad, na.rm = TRUE),
        edad_maxima = max(edad, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$tabla_peso <- renderTable({
    datos_filtrados() %>%
      group_by(provincia) %>%
      summarise(
        cantidad_personas = n(),
        .groups = "drop"
      ) %>%
      mutate(
        peso_relativo = cantidad_personas / sum(cantidad_personas)
      )
  })
}

shinyApp(ui = ui, server = server)

