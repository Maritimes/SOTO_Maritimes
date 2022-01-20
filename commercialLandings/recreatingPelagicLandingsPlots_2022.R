# library(RODBC)
# library(SSdata)
# channel <<- odbcConnect("PTRAN_64", uid = "mcmahonm", pwd = oracle.password)
# # 
# # #extract the data
# SSdata::extractLAND(path = "C:/DFO-MPO/SSdata/", e.year = 2021)

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(scales)
# MAR <- read_csv("MAR-pelagics.csv") $#$old data
setwd("C:/git/Maritimes/SOTO_Maritimes/")
load("data/data_priv/SSdata/data/landings/landings.RData") #gets loaded as 'landings'

#limit to NAFOS we care about
tomatch=c('4VN','4VS','4W','4X')
MAR = landings[ grep(paste(tomatch, collapse = '|'),landings$NAFO_UNIT),]
sppPelagic<-c(202, 204, 256, 260, 263, 264, 272, 278, 280, 282, 284, 289, 299, 302, 310, 312, 340, 462, 468, 469, 474)
MAR<-MAR[MAR$YEAR >= 1970 & MAR$YEAR < 2022, ]
MAR <- MAR[MAR$SPECIES %in% sppPelagic,]
MAR$CATCH <- MAR$CATCH/1000
MAR <- MAR %>% group_by(YEAR) %>% summarise(CATCH=sum(CATCH),.groups='drop')


#look at y data, figure out padding and decent breaks up decent x axis labels
maxD <- max(MAR$CATCH) + 0.05*max(MAR$CATCH)
yBreaks<- pretty(x = c(0,maxD), min.n = 4, n=6)

P2 <-  ggplot(NULL) +
  geom_col(data = MAR, aes(x = YEAR, y=CATCH),
           fill="slategray2") +
  labs(y = "Commercial Landings\n (thousands of tonnes)") +
  scale_y_continuous(breaks=yBreaks, expand = c(0,0), labels = function(x) format(x, scientific = FALSE), limits=c(0,maxD)) +
  scale_x_continuous(expand = c(0,0.5)) #, limits = c(1970, 2021)

P2 +  ggtitle("Scotian Shelf Pelagic Fish Commercial Landings") +
  theme(plot.title = element_text(size=12),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=12),
        axis.text.y = element_text(size=10),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "black", size = 1.5))