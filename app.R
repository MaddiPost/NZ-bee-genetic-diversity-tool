
# Initialise libraries
install.packages("pca3d")
library(pca3d)
install.packages("ggfortify")
library(ggfortify)
install.packages("devtools")
library(devtools)
install_github('ggfortify/sinhrks')

install.packages("ggplot2")
library(ggplot2)
install.packages("viridis")
library(viridis)

install.packages("gghighlight")
library(gghighlight)

install.packages("markdown")
library(markdown)
install.packages("shinythemes")
library(shinythemes)
install.packages("patchwork")
library(patchwork)

# Load data
G5.ID3 <- read.csv("G5_complete.Animal.Region.Company.new.csv") # huge dataset: headers are "Sample_Id, Animal, region_ company, 1,2,3,4......
names(G5.ID3)

# Apply apply principal component analysis
pca <- prcomp(G5.ID3[,-c(1:4)], center = TRUE, scale = TRUE)

# Create regions including all regions
allRegions <- autoplot(pca, data = G5.ID3, colour = 'Region', width = "800px", height = "400px") + 
    scale_color_manual(values = c("#440154FF", "#481A6CFF", "#472F7DFF", 
                                  "#414487FF", "#39568CFF", "#31688EFF", 
                                  "#2A788EFF", "#23888EFF", "#1F988BFF", 
                                  "#22A884FF", "#35B779FF", "#54C568FF", 
                                  "#f66e08", "#A5DB36FF", "#D2E21BFF", "#FDE725FF"))


#ggplot(df, aes(x=x, y=y)) + geom_point() + coord_fixed()


# Create a function that will generate a region-specific plot
# To get this to work, for example, Waikato type in: regionPlot("Waikato")

regionPlot <- function(Location, colour){
    autoplot(pca, data = G5.ID3, colour = 'Region', width = "800px", height = "400px") + 
        #scale_color_viridis(discrete=TRUE,option="D") +
        scale_color_manual(values = colour) +
        gghighlight(Region == Location, use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
        #theme(axis.title.x=element_blank(),
        #      axis.text.x=element_blank(),
        #      axis.ticks.x=element_blank(),
       #      axis.title.y=element_blank(),
       #       axis.text.y=element_blank(),
        #      axis.ticks.y=element_blank()) + 
        annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label= NULL) #Location
}



regionPlot <- function(Location, colour){
    autoplot(pca, data = G5.ID3, colour = 'Region', width = "800px", height = "400px") + 
        #scale_color_viridis(discrete=TRUE,option="D") +
        scale_color_manual(values = colour) +
        gghighlight(Region == c(Location), use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
        #theme(axis.title.x=element_blank(),
        #      axis.text.x=element_blank(),
        #      axis.ticks.x=element_blank(),
        #      axis.title.y=element_blank(),
        #       axis.text.y=element_blank(),
        #      axis.ticks.y=element_blank()) + 
        annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label= NULL) #Location
}

#From Gertje:
#autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
#gghighlight(Region == c("USA", "Canterbury", "Nelson"), use_group_by = FALSE,use_direct_label = FALSE, label_key = Region)

#concatenate


# Preload plot objects for each region
RegCant <- regionPlot(Location = "Canterbury", "#440154FF")      #still a function 
RegCent <- regionPlot(Location = "Central Otago","#481A6CFF" )
RegCoast <- regionPlot(Location = "Coastal Otago", "#472F7DFF")
RegEast <- regionPlot(Location = "East Cape", "#414487FF")

RegHawk <- regionPlot(Location = "Hawke's Bay", "#39568CFF")
RegMana <- regionPlot(Location = "Manawatu", "#31688EFF")
RegMarl <- regionPlot(Location = "Marlborough", "#2A788EFF")s
RegNels <- regionPlot(Location = "Nelson", "#23888EFF")

RegNorth <- regionPlot(Location = "Northland", "#1F988BFF")
RegQueen <- regionPlot(Location = "Queenstown Lakes", "#22A884FF")
RegTas <- regionPlot(Location = "Tasman", "#35B779FF")
RegTaup <- regionPlot(Location = "Taupo", "#54C568FF")

RegUSA <- regionPlot(Location = "USA", "#f66e08")
RegWaik <- regionPlot(Location = "Waikato", "#A5DB36FF")
RegWair <- regionPlot(Location = "Wairarapa", "#D2E21BFF")
RegWest <- regionPlot(Location = "West Coast", "#FDE725FF")


#SHINY###################

library(shiny)


ui <- fluidPage(
    theme = shinytheme("flatly"),
    
    titlePanel("New Zealand honeybee"),
    navbarPage("",
               tabPanel("Region",
                        sidebarLayout(
                            sidebarPanel(width =3,
                                         checkboxGroupInput("checkGroup", #inputId
                                                            h3("Region"), #label
                                                            choices = list("All", "Canterbury", "Central Otago","Coastal Otago",
                                                                           "East Cape", "Hawke's Bay", "Manawatu","Marlborough",
                                                                           "Nelson", "Northland","Queenstown Lakes","Tasman", 
                                                                           "Taupo", "USA", "Waikato", "Wairarapa", "West Coast"),
                                                                            selected = "All")), 
                            mainPanel (width =8, plotOutput("regPlot", width = "800px", height = "400px"))))))




regionPlot <- function(Location, colour){
    autoplot(pca, data = G5.ID3, colour = 'Region', width = "800px", height = "400px") + 
        scale_color_manual(values = colour) +
        gghighlight(Region == c(Location), use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
        annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label= NULL) #Location
}


server = function(input, output){
    regPlot <- reactive({
        if("Region" %in% input$checkGroup) return(regionPlot)
        if("All" %in% input$checkGroup) return(allRegions)
    })
    output$regPlot <- renderPlot({
        dataplots <- regPlot()
        print(dataplots)
    })}






server = function(input, output){
    regPlot <- reactive({
        if("Canterbury" %in% input$checkGroup) return(RegCant)
        if("USA" %in% input$checkGroup) return(RegUSA)
        if("Northland" %in% input$checkGroup) return(RegNorth)
        if("Waikato" %in% input$checkGroup) return(RegWaik)
        if("Taupo" %in% input$checkGroup) return(RegTaup)
        if("East Cape" %in% input$checkGroup) return(RegEast)
        if("Hawke's Bay" %in% input$checkGroup) return(RegHawk)
        if("Manawatu" %in% input$checkGroup) return(RegMana)
        if("Wairarapa" %in% input$checkGroup) return(RegWair)
        if("Tasman" %in% input$checkGroup) return(RegTas)
        if("Nelson" %in% input$checkGroup) return(RegNels)
        if("Marlborough" %in% input$checkGroup) return(RegMarl)
        if("West Coast" %in% input$checkGroup) return(RegWest)
        if("Queenstown Lakes" %in% input$checkGroup) return(RegQueen)
        if("Canterbury" %in% input$checkGroup) return(RegCant)
        if("Central Otago" %in% input$checkGroup) return(RegCent)
        if("Coastal Otago" %in% input$checkGroup) return(RegCoast)
        if("All" %in% input$checkGroup) return(allRegions)
    })
    output$regPlot <- renderPlot({
        dataplots <- regPlot()
        print(dataplots)
    })}

# Run the application 
shinyApp(ui = ui, server = server)


# dashboard skins: https://rstudio.github.io/shinydashboard/appearance.html

# multiple select 

#https://stackoverflow.com/questions/67697905/using-checkboxgroupinput-to-show-only-certain-bars-on-a-plot-shiny

#gghighlight: highlight multiple datasets

#for company use other colour palate (viridis) --> see pca code from Gertje
#must have multiple select and dropdown menu




