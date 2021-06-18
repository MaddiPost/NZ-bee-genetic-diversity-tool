
# Initialise libraries
# install.packages("pca3d")
library(pca3d)
# install.packages("ggfortify")
library(ggfortify)
# install.packages("devtools")
library(devtools)
# install_github('ggfortify/sinhrks')

# install.packages("ggplot2")
library(ggplot2)
# install.packages("viridis")
library(viridis)

# install.packages("gghighlight")
library(gghighlight)

# install.packages("markdown")
library(markdown)
# install.packages("shinythemes")
library(shinythemes)
# install.packages("patchwork")
library(patchwork)


# tidyverse solution
library(sf)
library(spData)
library(readr)
library(shiny)


# Load data
G5.ID3 <- read.csv("G5_complete.Animal.Region.Company.new.csv") # huge dataset: headers are "Sample_Id, Animal, region_ company, 1,2,3,4......
names(G5.ID3)

# Apply apply principal component analysis
pca <- prcomp(G5.ID3[,-c(1:4)], center = TRUE, scale = TRUE)
summary(pca)


head(dataReg)

# Create regions including all regions
allRegions <- autoplot(pca, data = G5.ID3, colour = 'Region', width = "700px", height = "400px") +
  scale_color_manual(values = c("#440154FF", "#481A6CFF", "#472F7DFF",
                                "#414487FF", "#39568CFF", "#31688EFF",
                                "#2A788EFF", "#23888EFF", "#1F988BFF",
                                "#22A884FF", "#35B779FF", "#54C568FF",
                                "#f66e08", "#A5DB36FF", "#D2E21BFF", "#FDE725FF"))

allRegions

# Create a function that will generate a region-specific plot
# To get this to work, for example, Waikato type in: regionPlot("Waikato")

regionPlot <- function(Location, colour){
  autoplot(pca, data = G5.ID3, colour = 'Region', width = "700px", height = "400px") +
    #scale_color_viridis(discrete=TRUE,option="D") +
    scale_color_manual(values = colour) +
    gghighlight(Region == Location, use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) +
    annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label= NULL) #Location
}


# Preload plot objects for each region
RegCant <- regionPlot(Location = "Canterbury", "#440154FF") 
RegCent <- regionPlot(Location = "Central Otago","#481A6CFF" )
RegCoast <- regionPlot(Location = "Coastal Otago", "#472F7DFF")
RegEast <- regionPlot(Location = "East Cape", "#414487FF")

RegHawk <- regionPlot(Location = "Hawke's Bay", "#39568CFF")
RegMana <- regionPlot(Location = "Manawatu", "#31688EFF")
RegMarl <- regionPlot(Location = "Marlborough", "#2A788EFF")
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
install.packages("shinyWidgets")
library(shinyWidgets)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("New Zealand honeybee"),
  navbarPage("",
             tabPanel("Region",
                      sidebarLayout(
                        sidebarPanel(width =3,
                                     awesomeRadio("checkGroup", #inputId
                                                  h3("Region"), #label
                                                  c("All", "Canterbury", "Central Otago","Coastal Otago",
                                                    "East Cape", "Hawke's Bay", "Manawatu","Marlborough",
                                                    "Nelson", "Northland","Queenstown Lakes","Tasman",
                                                    "Taupo", "USA", "Waikato", "Wairarapa", "West Coast"),
                                                  selected = "All")),
                        mainPanel (width =8, plotOutput("regPlot", width = "700px", height = "400px"))))))

# regionPlot <- function(Location, colour){
#   autoplot(pca, data = G5.ID3, colour = 'Region', width = "700px", height = "400px") +
#     #scale_color_viridis(discrete=TRUE,option="D") +
#     scale_color_manual(values = colour) +
#     gghighlight(Region == Location, use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) +
#     annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label= NULL) #Location
# }



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


################################################################## generate map
# tidyverse solution
library(ggplot2)
library(sf)
library(spData)
library(readr)
library(dplyr)

library(sp)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(classInt)


#use authorities shapefile
mymap <- st_read("territorial-authority-2021-clipped-generalised.shp",
                 stringsAsFactors = TRUE)

View(mymap)
class(mymap)
str(mymap)

#from tutorial:
plot(st_geometry(mymap), axes = TRUE)

cent_nz <- st_centroid(st_geometry(mymap))  

ggplot() +
  geom_sf(data = mymap, aes(fill = )) +
  theme_bw() +
  coord_sf(xlim = c(166, 179.5), ylim = c(-48, -34))


ggplot() +
  geom_sf(data = mymap, aes(fill =  TA2021_V_1)) +
  #geom_sf(data = cent_nz, pch = 16, cex = 4,  color = "red") +
  theme_bw() + 
  theme (legend.position = "none") + #removes the figure legend
coord_sf(xlim = c(166, 179.5), ylim = c(-48, -34)) #set coordinates to nz to remove unwanted island 
         
         
head(dataReg)

mapview::mapview(mymap)


# ggplot()  +
#   geom_sf(data = mymap) +
#   geom_sf(data = cent_nz, aes(size = 3)) + 
#   scale_size_area(max_size = 10) +
#   labs(x = "Longitude", y = "Latitude", title = "") +
#   theme_bw()

# Region Observations Dispersion
# 1    Canterbury          160   33.66034
# 2 Central Otago           57   29.94028
# 3 Coastal Otago          113   33.44141
# 4     East Cape          161   36.36489
# 5   Hawke's Bay           83   27.20767
# 6      Manawatu          224   30.13834


#TRY THIS: https://pjbartlein.github.io/GeogDataAnalysis/lec06.html

# RegCant <- regionPlot(Location = "Canterbury", "#440154FF") 
# RegCent <- regionPlot(Location = "Central Otago","#481A6CFF" )
# RegCoast <- regionPlot(Location = "Coastal Otago", "#472F7DFF")
# RegEast <- regionPlot(Location = "East Cape", "#414487FF")
# 
# RegHawk <- regionPlot(Location = "Hawke's Bay", "#39568CFF")
# RegMana <- regionPlot(Location = "Manawatu", "#31688EFF")
# RegMarl <- regionPlot(Location = "Marlborough", "#2A788EFF")
# RegNels <- regionPlot(Location = "Nelson", "#23888EFF")
# 
# RegNorth <- regionPlot(Location = "Northland", "#1F988BFF")
# RegQueen <- regionPlot(Location = "Queenstown Lakes", "#22A884FF")
# RegTas <- regionPlot(Location = "Tasman", "#35B779FF")
# RegTaup <- regionPlot(Location = "Taupo", "#54C568FF")
# 
# RegUSA <- regionPlot(Location = "USA", "#f66e08")
# RegWaik <- regionPlot(Location = "Waikato", "#A5DB36FF")
# RegWair <- regionPlot(Location = "Wairarapa", "#D2E21BFF")
# RegWest <- regionPlot(Location = "West Coast", "#FDE725FF")


# ggplot() +
#   geom_sf(data = nz, aes(fill = )) +
#   theme_bw() +
#   coord_sf(xlim = c(150.97, 180), ylim = c(-33.98, -33.79)) +
#   geom_sf(data = cent_nz, pch = 16, cex = 4, bg = "red", color = "red")



############################################

#using https://www.r-graph-gallery.com/330-bubble-map-with-ggplot2
## generating data for centroid cirlces

RegList <- c("Canterbury", "Central Otago","Coastal Otago",
             "East Cape", "Hawke's Bay", "Manawatu","Marlborough",
             "Nelson", "Northland","Queenstown Lakes","Tasman",
             "Taupo", "USA", "Waikato", "Wairarapa", "West Coast")
RegList[1]

#Create function:

#RegData <- function(Region){
#  G5.ID3 %>% group_by(Region == "Region") %>% prcomp(Region[, -c(1:4)], 
#                                                     center = TRUE, scale = TRUE)}

RegGroup <- G5.ID3 %>% group_by(Region)

Nels <- subset(RegGroup, Region == "Nelson")
group_size(Nels) #78
PCANel <-  prcomp(Nels[,-c(1:4)], center = TRUE, scale = TRUE)
summary(PCANel) #use standard deviation of PC1 and PC2 for circle size?
PCANel$sdev[1]

Cant <- subset(RegGroup, Region =="Canterbury")
group_size(Cant) #160
PCACant <-  prcomp(Cant[,-c(1:4)], center = TRUE, scale = TRUE)
summary(PCACant)
PCACant$sdev[1] 





#####################################################

reg_grp <- G5.ID3 %>% group_by(Region)

regData <- function(region){
  grp <- subset(RegGroup, Region==region)
  pca <- prcomp(grp[,-c(1:4)], center = TRUE, scale = TRUE)
  sd1 <- pca$sdev[1]
  grpsize <- group_size(grp)
  dataList <- list(region, grpsize, sd1)
  return(dataList)
}

regList = c("Canterbury", "Central Otago","Coastal Otago",
            "East Cape", "Hawke's Bay", "Manawatu","Marlborough",
            "Nelson", "Northland","Queenstown Lakes","Tasman",
            "Taupo", "USA", "Waikato", "Wairarapa", "West Coast")


dataReg <- data.frame(x1 = character(),    # Create empty data frame
                      x2 = numeric(0),
                      x3 = numeric(0))


for(i in 1:length(regList)){
  #print(regList[i])
  regionRow <- list(regData(regList[i])[1], regData(regList[i])[2], regData(regList[i])[3])
  print(regionRow)
  dataReg <- rbind(dataReg, regionRow)
}

columns = c("Name", "Observations", "Dispersion")
colnames(dataReg) = columns

#How to access the data of a particular region such as Taupo
temp <- dataReg[dataReg$Region=="Taupo",]
temp$Observations #Number of observations in Taupo
temp$Dispersion #The standard deviation of PC1 for Taupo

