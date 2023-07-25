# Chargement des packages
library(shiny)
library(leaflet)

# Importation des données
library(readr)
ACLED_Western_Africa <- read_csv("ACLED-Western_Africa.csv")
# Conversion de la colonne "date" en type de données Date
data <- read.csv("ACLED-Western_Africa.csv", stringsAsFactors = FALSE)
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# Définition des types de filtres possibles
filter_types <- c("Pays", "Type", "Année", "Localisation")


# Définition de l'interface utilisateur
ui <- fluidPage(
   titlePanel("Visualisation des événements en Afrique de l'Ouest"),
   sidebarLayout(
      sidebarPanel(
         selectInput("filter_type", "Filtrer par :", choices = filter_types),
         uiOutput("filter_ui")
      ),
      mainPanel(
         leafletOutput("map")
      )
   )
)

# Définition de la logique côté serveur
server <- function(input, output, session) {
   # Génération dynamique des options de filtre en fonction du type sélectionné
   observeEvent(input$filter_type, {
      filter_type <- input$filter_type
      
      if (filter_type == "Pays") {
         filter_choices <- unique(data$pays)
      } else if (filter_type == "Type") {
         filter_choices <- unique(data$type)
      } else if (filter_type == "Année") {
         filter_choices <- unique(data$annee)
      } else if (filter_type == "Localisation") {
         filter_choices <- c("Tous", "Précise")
      }
      
      output$filter_ui <- renderUI({
         selectInput("filter_value", paste("Sélectionnez", tolower(filter_type), ":"), choices = filter_choices)
      })
   })
   
   # Filtrage des données en fonction des sélections
   filtered_data <- reactive({
      filter_type <- input$filter_type
      filter_value <- input$filter_value
      
      if (filter_type == "Pays") {
         data %>%
            filter(pays == filter_value)
      } else if (filter_type == "Type") {
         data %>%
            filter(type == filter_value)
      } else if (filter_type == "Année") {
         data %>%
            filter(annee == filter_value)
      } else if (filter_type == "Localisation") {
         if (filter_value == "Précise") {
            data %>%
               filter(!is.na(latitude) & !is.na(longitude))
         } else {
            data
         }
      } else {
         data
      }
   })
   
   # Affichage de la carte avec les marqueurs des événements
   output$map <- renderLeaflet({
      leaflet() %>%
         addTiles() %>%
         addMarkers(
            data = filtered_data(),
            lng = ~longitude,
            lat = ~latitude,
            popup = ~paste("Pays:", pays, "<br>Type:", type, "<br>Année:", annee)
         )
   })
}

# Exécution de l'application Shiny
shinyApp(ui = ui, server = server)
