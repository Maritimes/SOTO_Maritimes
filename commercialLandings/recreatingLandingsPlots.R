
setwd("C:/git/Maritimes/SOTO_Maritimes/")
load("data/data_priv/SSdata/data/landings/landings.RData") #gets loaded as 'landings'
source("C:/git/Maritimes/SOTO_Maritimes/commercialLandings/getCommercialSpp_SOTO.R")
source("commercialLandings/getCommercialSpp_SOTO.R")

#limit to NAFOS we care about
tomatch=c('4VN','4VS','4W','4X')
landings = landings[ grep(paste(tomatch, collapse = '|'),landings$NAFO_UNIT),]

# add the general NAFO areas (e.g. want 4X, not 4XM)
landings$NAFO_GENERAL <- regmatches(landings$NAFO_UNIT, regexpr(paste(tomatch, collapse = "|"), landings$NAFO_UNIT))
landings$NAFO_UNIT <- NULL


sppPelagic<- getPelagicSpp_SOTO()
sppDemersal <- getDemersalSpp_SOTO()
sppBenthicInverts <- getBenthicInvertsSpps_SOTO()

landings$SPECIES_CATEGORY <- NA
landings[landings$SPECIES %in% sppPelagic, "SPECIES_CATEGORY"] <- "Pelagic Fish"
landings[landings$SPECIES %in% sppDemersal, "SPECIES_CATEGORY"] <- "Demersal Fish"
landings[landings$SPECIES %in% sppBenthicInverts, "SPECIES_CATEGORY"] <- "Benthic Invertebrates"

# # 4VN 1992 ~2000 demersal

landings<-landings[,c("SPECIES_CATEGORY", "CATCH", "NAFO_GENERAL", "YEAR")]
landings<-landings[!is.na(landings$SPECIES_CATEGORY),]
landings<-landings[landings$YEAR >= 1970 & landings$YEAR < 2022, ]

library(dplyr)
all<- landings %>%
  group_by(YEAR, SPECIES_CATEGORY, NAFO_GENERAL) %>%
  summarise(TONNES = sum(CATCH), .groups = 'drop')
rm(landings)


all$SPECIES_CATEGORY <- factor(all$SPECIES_CATEGORY , levels=c("Benthic Invertebrates", "Pelagic Fish", "Demersal Fish") )

all_4VN <- all[all$NAFO_GENERAL == "4VN",]
all_4VS <- all[all$NAFO_GENERAL == "4VS",]
all_4W <- all[all$NAFO_GENERAL == "4W",]
all_4X <- all[all$NAFO_GENERAL == "4X",]


library(ggplot2)
library(gridExtra)
makeThePlot <- function(data = NULL, title = NULL){
  #set up decent x axis labels
  xMin <- seq(1970, 2021, 1)
  xlabels <- c("1970","","","","","1975","","","","","1980","","","","","1985","","","","","1990","","","","","1995","","","","","2000","","","","","2005","","","","","2010","","","","","2015","","","","","2020","")
  
  #look at y data, figure out padding and decent breaks up decent x axis labels
  maxD1 <- data %>% group_by(YEAR) %>% summarise(annual=sum(TONNES),.groups='drop')
  maxD <- max(maxD1$annual) + 0.05*max(maxD1$annual)
  yBreaks<- pretty(x = c(0,maxD1$annual), min.n = 4, n=8)

  p <- ggplot(data = data, aes(fill=SPECIES_CATEGORY, y=TONNES, x=YEAR)) +
    ggtitle(title) +
    geom_bar(position='stack', stat='identity')+ylab("Landings (tonnes)") + xlab("") +
    scale_fill_manual(values = c("#7E62A1","#9BBB59","#C0514E")) +
    scale_y_continuous(breaks=yBreaks,labels = function(x) format(x, scientific = FALSE), expand = c(0, 0),limits=c(0,maxD))+ 
    scale_x_continuous(breaks=xMin, labels = xlabels, expand=c(0,0)) +
    theme(legend.position = c(0.82,0.85), legend.title=element_blank(), plot.title = element_text(vjust = - 8, hjust = 0.5),  
          panel.border = element_blank(),axis.line = element_line(color = 'black'),
          plot.background = element_rect(color = "black")) 
  return(p)
}

all_4VN_p<- makeThePlot(all_4VN, title= "4VN")
all_4VS_p<- makeThePlot(all_4VS, title= "4VS")
all_4W_p<- makeThePlot(all_4W, title= "4W")
all_4X_p<- makeThePlot(all_4X, title= "4X")



g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

mylegend<-g_legend(all_4VN_p)



final <- grid.arrange(all_4VN_p,
                      all_4W_p + theme(legend.position="none"),
                      all_4VS_p + theme(legend.position="none"),
                      all_4X_p + theme(legend.position="none"),
                      nrow=2)








