################### step 1: make group checkbox scatterplot with GO button for region:


install.packages("viridis")
library(viridis)

# Load data
G5.ID3 <- read.csv("G5_complete.Animal.Region.Company.new.csv") # huge dataset: headers are "Sample_Id, Animal, region_ company, 1,2,3,4......
names(G5.ID3)

# Apply apply principal component analysis
pca <- prcomp(G5.ID3[,-c(1:4)], center = TRUE, scale = TRUE)

autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == c("USA", "Canterbury", "Nelson"), use_group_by = FALSE,use_direct_label = FALSE, label_key = Region)

#code for R to tell you the codes for colours from a palette:
#colours_plot <- library(paletteer) ; paletteer_c(package = viridis, palette = "viridis", n = 16)

# Create regions including all regions
allRegions <- autoplot(pca, data = G5.ID3, colour = 'Region', width = "800px", height = "400px") + 
  scale_color_manual(values = c("#440154FF", "#481A6CFF", "#472F7DFF", 
                                "#414487FF", "#39568CFF", "#31688EFF", 
                                "#2A788EFF", "#23888EFF", "#1F988BFF", 
                                "#22A884FF", "#35B779FF", "#54C568FF", 
                                "#f66e08", "#A5DB36FF", "#D2E21BFF", "#FDE725FF"))
allRegions

# Create a function that will generate a region-specific plot
# To get this to work, for example, Waikato type in: regionPlot("Waikato")

regionPlot <- function(Location, colour){
  autoplot(pca, data = G5.ID3, colour = 'Region', width = "800px", height = "400px") + 
    #scale_color_viridis(discrete=TRUE,option="D") +
    scale_color_manual(values = colour) +
    gghighlight(Region == Location, use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
    annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label= NULL) #Location
}


ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("New Zealand honeybee"),
  navbarPage("",
             tabPanel("Region",
                      sidebarLayout(
                        sidebarPanel(width =3,
                         checkboxGroupInput(inputId = "RegionFinder", #inputId
                           label = "Region", #label
                            choices = list("All", "Canterbury", "Central Otago","Coastal Otago",
                                "East Cape", "Hawke's Bay", "Manawatu","Marlborough",
                                "Nelson", "Northland","Queenstown Lakes","Tasman", 
                                "Taupo", "USA", "Waikato", "Wairarapa", "West Coast"),
                                                        selected = "All")), 
       mainPanel (width =8, 
                  plotOutput("regPlot", width = "800px", height = "400px"))))))



server <- function(input, output){
  
  regPlot <- reactive({
   filtered <-
     G5.ID3 %>%
     filter(Region %in% input$RegionFinder)
    
      })
  output$Regionplot <- renderPlot({
    ggplot(regPlot(), aes(color=Region))})}
    
    
    
    req(input$RegionFinder)
    filter(G5.ID3, Region %in% input$RegionFinder) 
   # if("Canterbury" %in% input$checkGroup) return(RegCant)
  })
  output$regPlot <- renderPlot({
    input$RegionFinder
    dataplots <- regPlot()
    if("All" %in% input$RegionFinder) return(allRegions)
    print(dataplots)
  })}




# Run the application 
shinyApp(ui = ui, server = server)




##############################################################################################33

#install.packages("shiny")
library(shiny)
#install.packages("shinyWidgets")
library(shinyWidgets)
#install.packages("dslabs")
library(dslabs)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("plotly")
library(plotly)

# data("us_contagious_diseases")
# disease <- us_contagious_diseases
# disease <- mutate(disease, percapita = count/(population/100000)) %>% 
#   pivot_longer(cols = c(count, percapita), 
#                names_to = "data", values_to = "value")
# 
# 
# 
# ui <- fluidPage(
#   titlePanel("Diseases in the US 1928-2011"),
#     sidebarPanel( width = 3,
#       # inputs
#       checkboxGroupInput("diseaseInput", "Disease",
#                          choices = c("Hepatitis A",
#                                      "Measles",
#                                      "Mumps", "Pertussis",
#                                      "Polio", "Rubella", 
#                                      "Smallpox"),
#                          selected = c("Hepatitis A", "Polio")),
#     mainPanel(width =8, 
#       plotOutput("diseaseplot"),
#       br(), br(),
#     )))  
# 
# 
# 
# 
# server <- function(input, output) {
#   
#   d <- reactive({
#     disease %>%
#       filter(
#              disease %in% input$diseaseInput)
#   }) 
#   
# output$diseaseplot <- renderPlot({
#     ggplot(d(), aes(x=disease, y = value, color=disease)) +
#       geom_line() + 
#       theme_bw() +
#       xlab("") +
#       ylab(input$dataInput) +
#       ggtitle("Cases over time")})}
# 
# shinyApp(ui=ui, server=server)
