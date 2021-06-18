#example function (wrappers can take all sorts of forms, it is good to be specific :) ):


G5.ID3 <- read.csv("G5_complete.Animal.Region.Company.new.csv") # huge dataset: headers are "Sample_Id, Animal, region_ company, 1,2,3,4......
names(G5.ID3)

pca <- prcomp(G5.ID3[,-c(1:4)], center = TRUE, scale = TRUE) #tells us what data to look 
#3d PCA plot
install.packages("pca3d")
library(pca3d)
regions <- G5.ID3[,3]
pca3d(pca, group = regions)
# autoplot(pca) #No colouring
#install.packages("ggfortify")
library(ggfortify)
install.packages("viridis")
library(viridis)


library(devtools)
install_github('sinhrks/ggfortify')
 library(ggplot2)


#get viridis plot colours to be able to change single colours:
#colours_plot <- library(paletteer) ; paletteer_c(package = "viridis", palette = "viridis", n = 16)
# plot with distinguishable outgroup (in orange)
autoplot(pca, data = G5.ID3, colour = 'Region') + 
  scale_color_manual(values = c("#440154FF", "#481A6CFF", "#472F7DFF", 
                                "#414487FF", "#39568CFF", "#31688EFF", 
                                "#2A788EFF", "#23888EFF", "#1F988BFF", 
                                "#22A884FF", "#35B779FF", "#54C568FF", 
                                "#f66e08", "#A5DB36FF", "#D2E21BFF", "#FDE725FF")) 



install.packages("gghighlight")
library(gghighlight)
#highlight outgroup:
autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="C") +
  gghighlight(Region == "USA", use_group_by = FALSE)
library(gridExtra)

#By region
# create elements first, then arrange in grid to produce figure
all <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="C")
#USA <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
#  gghighlight(Region == "USA", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region)
CB <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "Canterbury", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Canterbury")

Central <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "Central Otago", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Central Otago")



#####example wrapper:

plot_ndvi <- function(save=FALSE, show_flight=TRUE, latest_year=TRUE) { 
  require(ggplot2)
  NDVI_by_park <- readr::read_csv("data_out/NDVI_by_park_by_year.csv")
  flight <- readr::read_csv("data_out/flight_hours_normalized.csv") #then add PCA
  p <- NDVI_by_park %>% 
    filter(year < 2020) %>% 
    group_by(park, week) %>% 
    summarise(mean_ndvi = mean(mean)) %>% 
    ggplot(aes(x=week, y=mean_ndvi, color="mean_historical_ndvi")) + 
    geom_line() 
  
  if (show_flight) { 
    p <- p + geom_line(data=flight, aes(x=week, y=weekly_flight_idx, color="flight_hours") ) 
  }
  if (latest_year) {
    p <- p + geom_line(data=NDVI_by_park %>% filter(year==2020), aes(x=week, y=mean, color="2020_ndvi")) 
  }
  p <- p + facet_wrap(~park) +  ggtitle("NDVI time series")
  ggsave("plots/NDVI_with_flight_hours.pdf", p)
  return(p) 
}


#####################################
WRAP <- function(
  G5.ID3 <- read:: read.csv("G5_complete.Animal.Region.Company.new.csv")
  pca <- prcomp(G5.ID3[,-c(1:4)], center = TRUE, scale = TRUE)
  
  
)



#########################################all code from Gertjes G5_complete...
  
  G5.ID3 <- read.csv("G5_complete.Animal.Region.Company.new.csv")

pca <- prcomp(G5.ID3[,-c(1:4)], center = TRUE, scale = TRUE)
#3d PCA plot
library(pca3d)
regions <- G5.ID3[,3]
pca3d(pca, group = regions) #pca is grouped by region
# autoplot(pca) #No colouring
library(ggfortify)
library(viridis)
#get viridis plot colours to be able to change single colours:
#colours_plot <- library(paletteer) ; paletteer_c(package = "viridis", palette = "viridis", n = 16)
# plot with distinguishable outgroup (in orange)
autoplot(pca, data = G5.ID3, colour = 'Region') + 
  scale_color_manual(values = c("#440154FF", "#481A6CFF", "#472F7DFF", 
                                "#414487FF", "#39568CFF", "#31688EFF", 
                                "#2A788EFF", "#23888EFF", "#1F988BFF", 
                                "#22A884FF", "#35B779FF", "#54C568FF", 
                                "#f66e08", "#A5DB36FF", "#D2E21BFF", "#FDE725FF")) 

library(gghighlight)
#highlight outgroup:
autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "USA", use_group_by = FALSE)
library(gridExtra)

#By region
# create elements first, then arrange in grid to produce figure
all <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="C")
#USA <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
#  gghighlight(Region == "USA", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region)
CB <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "Canterbury", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Canterbury")

Central <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "Central Otago", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Central Otago")

Coastal <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "Coastal Otago", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Coastal Otago")
EastCape <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "East Cape", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="East Cape")
HawkesBay <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "Hawke's Bay", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Hawke's Bay")
Manawatu <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "Manawatu", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Manawatu")
Marlborough <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "Marlborough", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Marlborough")
Nelson <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "Nelson", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Nelson")
Northland <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "Northland", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Northland")
Queenstown <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "Queenstown Lakes", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Queenstown Lakes")
Tasman <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "Tasman", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Tasman")
Taupo <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "Taupo", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Taupo")
Waikato <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "Waikato", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Waikato")
Wairarapa <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "Wairarapa", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Wairarapa")
WestCoast <- autoplot(pca, data = G5.ID3, colour = 'Region') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Region == "West Coast", use_group_by = FALSE, use_direct_label = FALSE, label_key = Region) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="West Coast")
# arrange in geographically logical way... 
grid.arrange(Northland, Waikato, Taupo, EastCape, HawkesBay, Manawatu, Wairarapa, Tasman, Nelson, Marlborough, WestCoast, Queenstown, CB, Central, Coastal, ncol=5)

xx# By Company
all <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D")
KiwiBee <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "KiwiBee", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
Oha <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Oha Honey", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
Wild_Cape <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Wild Cape Manuka", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
B_Farm <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Bee Farm", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
B_Quest <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "BeeQuest", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
Kereru <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Kereru Queens", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
BettaBees <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Betta Bees Research", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
TaylorPass <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Taylor Pass Honey", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
RBQB <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Ruby Bay Queens", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
TaiTokerau <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Tai Tokerau Honey", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
Marsh <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Marsh's Honey", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
TreesBees <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Trees & Bees", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
Daykel <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Daykel", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
CoastCoast <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Coast to Coast", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
GaryJeffery <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Gary Jeffery", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
B_Smart <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Bee Smart Breeding", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
SouthernSun <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Southern Sun Queens", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
Midlands <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Midlands Apiaries", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
Cornell <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Cornell Bee Lab", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
grid.arrange(KiwiBee, Oha, Wild_Cape, B_Farm, B_Quest, Kereru, BettaBees, TaylorPass, RBQB, TaiTokerau, Marsh, TreesBees, Daykel, CoastCoast,GaryJeffery, B_Smart, SouthernSun, Midlands)


# Breeders with Names
Kereru_labeled <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Kereru Queens", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+ 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Kereru Queens")
BettaBees_labeled <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Betta Bees Research", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+ 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Betta Bees Research")
B_Smart_labeled <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Bee Smart Breeding", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+ 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Bee Smart Breeding")
SouthernSun_labeled <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Southern Sun Queens", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+ 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Southern Sun Queens")
RBQB_labeled <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Ruby Bay Queens", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+ 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Ruby Bay Queen Bees")
Daykel_labeled <- autoplot(pca, data = G5.ID3, colour = 'Company') + scale_color_viridis(discrete=TRUE,option="D") +
  gghighlight(Company == "Daykel", use_group_by = FALSE, use_direct_label = FALSE, label_key = Company) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+ 
  annotate(geom="text", x=-Inf, y=Inf, hjust=-0.1, vjust=1.6, size = 4, label="Daykel Apiaries")
grid.arrange(Daykel_labeled, Kereru_labeled, SouthernSun_labeled, RBQB_labeled, B_Smart_labeled, BettaBees_labeled), ncol=3


