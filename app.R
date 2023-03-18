#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load required packages
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(htmltools)
library(ggthemes)

# get data
dd <- read_csv("https://assets-decodeurs.lemonde.fr/decodeurs/medias/foreverpollution/expert_dataset.csv") %>% 
  filter(
    !is.na(country)
  ) %>% 
  mutate(
    category = str_replace_all(
      category,
      "Known",
      "Known contamination"
    ),
    category = str_replace_all(
      category,
      "Presumptive",
      "Presumptive contamination"
    ),
    matrix_unit = case_when(
      str_detect(matrix, "water") ~ "ng/L",
      matrix == "Unknown" ~ "ng/L or ng/kg",
      .default = "ng/kg"
    )
  )




# ------------------ USER INTERFACE  ------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # use a gradient in background
  setBackgroundColor(
    # color = c("#F0F0F0")
    color = c("#D9D9D9")
    # gradient = "linear",
    # direction = "bottom"
  ),
  
  # Application title
  titlePanel( div(HTML("<b>THE FOREVER POLUTION PROJECT</b>&emsp; Journalists tracking PFAS across Europe")) ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    # ------------------ sidebar  ------------------
    sidebarPanel(
      
      # pickerInput(
      #   "category",
      #   label = "Select category",
      #   choices = dd$category %>% unique(),
      #   options = list(`actions-box` = TRUE),
      #   multiple = TRUE,
      #   selected = "Known contamination"
      # ),
      radioButtons(
        "category",
        label = "Select category",
        choices = dd$category %>% unique(),
        selected = "Known contamination"
      ),
      pickerInput(
        "country", 
        label = "Select country",
        choices = dd$country %>% unique(),
        options = list(`actions-box` = TRUE),
        multiple = TRUE,
        selected = "Germany"
      ),
      pickerInput(
        "type", 
        label = "Select type",
        choices = dd$type %>% unique(),
        options = list(`actions-box` = TRUE),
        multiple = TRUE,
        selected = dd$type %>% unique()
      ),
      searchInput(
        "city", 
        label = "Search for city",
        value = "",
        placeholder = NULL,
        btnSearch = NULL,
        btnReset = NULL,
        resetValue = "",
        width = NULL
      )
      # plotOutput(
      #   outputId = "plot_bar_top_5",
      #   width = "100%", 
      #   height = "50vh"
      # )
      # selectInput(
      #   "metric", 
      #   label = "Select metric",
      #   choices = names(dd %>% select(starts_with("pf"))),
      #   multiple = FALSE,
      #   selected = "pfas_sum"
      # )
    ),
    
    # ------------------ main panel  ------------------
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput(
        outputId = "plot_map",
        width = "100%", 
        height = "90vh"
      )
    )
    
  )
)



# ------------------ SERVER  ------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # creating data used in map (applying filters...)
  dd_map <- reactive({ 
    
    req(input$category)
    req(input$country)
    req(input$type)
    
    dd %>% 
      filter(
        category %in% input$category,
        country %in% input$country,
        type %in% input$type,
        str_detect(tolower(city), ifelse(
          tolower(input$city) != "", 
          tolower(input$city), 
          "[a-zA-Z]")
        )
      )
    # filter(
    #   city == ifelse(
    #     input$city != "",
    #     str_detect(city, input$city),
    #     city
    #   )
    # )
  })
  
  # creating a continuous palette function
  # pal <- colorNumeric(
  #   palette = "Reds",
  #   # domain = input$metric
  #   domain = dd_map()$pfas_sum
  #   )
  pal <- colorNumeric(
    palette = colorRampPalette(c('#CE3818', '#F9F54B'))(round(length(dd$pfas_sum)/500, 0)), 
    domain = dd$pfas_sum)
  
  # creating the actual map
  output$plot_map <- renderLeaflet({
    
    # leaflet plot with markers and popups
    leaflet(
      data = dd_map()
    ) %>%
      addTiles(attribution = 'Forever Polution Project') %>% 
      addProviderTiles(
        providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE)
      ) %>% 
      addCircleMarkers(
        # addMarkers(
        ~ lon, 
        ~ lat, 
        radius = ~ case_when(
          input$category == "Known contamination" ~ (sqrt(dd_map()$pfas_sum) / max(sqrt(dd_map()$pfas_sum), na.rm = TRUE) * 10 + 3), 
          input$category == "Known contamination PFAS user" ~ 5,
          input$category == "Presumptive contamination" ~ 4
        ),
        stroke = TRUE,
        # color = "#666666",
        color = ~ case_when(
          input$category == "Known contamination" ~ pal(pfas_sum), #"#CE3818",
          input$category == "Known contamination PFAS user" ~ "#1CC5DC",
          input$category == "Presumptive contamination" ~ "#EB455F"
        ),
        weight = 2,
        fill = TRUE,
        fillColor = ~ case_when(
          input$category == "Known contamination" ~ pal(pfas_sum), #"#CE3818", 
          input$category == "Known contamination PFAS user" ~ "#1CC5DC",
          input$category == "Presumptive contamination" ~ "#EB455F"
        ), #dd_map()$category, #"#CE3818", #~pal(pfas_sum),
        # fillOpacity = 0.6,
        fillOpacity = ~ case_when(
          input$category == "Known contamination" ~ 0.7, 
          input$category == "Known contamination PFAS user" ~ 0.9,
          input$category == "Presumptive contamination" ~ 0.7
        ),
        popup = paste(
          "<strong>", dd_map()$name, "</strong><br>",
          dd_map()$matrix, "<br><br>",
          "<strong>PFAS sum:   ", format(
            round(dd_map()$pfas_sum, 1), 
            trim = TRUE,
            big.mark = "'", 
            na.encode = TRUE, 
            scientific = FALSE
          ), dd_map()$matrix_unit, "</strong><br><br>",
          # ), "</strong><br>ng/L (water) or ng/kg (soil)<br><br>",
          "City:   ", dd_map()$city, "<br>", 
          "Country:   ", dd_map()$country, "<br>",
          "Sector:   ", dd_map()$sector, "<br>",
          "Type:   ", dd_map()$type, "<br>",
          "Year:   ", dd_map()$year, "<br><br>",
          "Source type:   ", dd_map()$source_type, "<br>",
          "Source text:   ", dd_map()$source_text, "<br>",
          "<a href = \"", dd_map()$source_url, "\"> Source URL </a><br><br>", 
          "<a href = \"", "https://foreverpollution.eu/", "\"> foreverpollution.eu </a>"
          
          # '<a href = "https://rstudio.github.io/leaflet/"> R </a>'
        )
        # label = labels_map
        # clusterOptions = markerClusterOptions()
      )
    
    # output$plot_bar_top_5 <- renderPlot(
    # 
    #   ggplot(
    #     data = dd_map() %>%
    #       arrange(pfas_sum) %>%
    #       slice_head(n = 5)
    #   ) +
    #     geom_bar(
    #       x = paste0(name, " (", city, ")"),
    #       y = pfas_sum
    #     ) +
    #     coord_flip() +
    #     theme_fivethirtyeight()
    # 
    # )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




# ggplot(
#   data = dd,
# ) +
#   geom_histogram(
#     aes(pfas_sum),
#     binwidth = 500000
#   ) +
#   theme_five
