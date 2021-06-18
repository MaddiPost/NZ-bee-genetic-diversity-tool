
# NZ bee genetics map with reactive shiny map and group checkbox!!!


# Initialise libraries
install.packages("pca3d")
install.packages("ggfortify")
install.packages("devtools")
install_github('ggfortify/sinhrks')
install.packages("ggplot2")
install.packages("viridis")
install.packages("gghighlight")
install.packages("markdown")
install.packages("shinythemes")
install.packages("patchwork")
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
install.packages("viridisLite") 
install.packages("viridis")
install.packages("paletteer")

library(pca3d)
library(ggfortify)
library(devtools)
library(gghighlight)
library(markdown)
library(shinythemes)
library(patchwork)

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(raster)
library(dplyr)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(shiny)
library(viridisLite)
library(viridis)
library(paletteer)


############################


G5.ID3 <- read.csv("G5_complete.Animal.Region.Company.new.csv") # huge dataset: headers are "Sample_Id, Animal, region_ company, 1,2,3,4......

data.pca <- G5.ID3[,-c(1:4)] #tell it to look at column 4 onwards

pca.obj <- prcomp(data.pca)  #do principal component analysis on selected columns
autoplot(prcomp(data.pca), center = TRUE, scale = TRUE)


dtp <- data.frame('Region' = G5.ID3$Region, pca.obj$x[,1:2]) # the first two components are selected (NB: you can also select 3 for 3D plottings or 3+)

ggplot(data = dtp) + 
  geom_point(aes(x = PC1, y = PC2, col = Region)) + 
  theme_minimal() 

#WORKS
# Now try to tell r to generate plot depending on what data we've selected aka USA and Nelson



####################### FIND A WAY TO SELECT ALL


ui=shinyUI(fluidPage(
  
  checkboxGroupInput("region_choose", label = "Choose a region",
                                        choices = c("Canterbury", "Central Otago","Coastal Otago",
                                                    "East Cape", "Hawke's Bay", "Manawatu","Marlborough",
                                                    "Nelson", "Northland","Queenstown Lakes","Tasman",
                                                    "Taupo", "USA", "Waikato", "Wairarapa", "West Coast")),

plotOutput("RegionPlot")
))




server=function(input,output){
  

#data manipulation
  RegionData=reactive({
    
    return(dtp[dtp$Region %in% input$region_choose,])
  })
  
  #plot
  output$RegionPlot <- renderPlot({
    ggplot(data=RegionData(), aes(x=PC1, y=PC2, group=Region, colour=Region)) +
      geom_point()
  })
}

shinyApp(ui,server)



