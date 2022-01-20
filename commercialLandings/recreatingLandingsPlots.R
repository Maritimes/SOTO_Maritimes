# library(RODBC)
# library(SSdata)
# channel <<- odbcConnect("PTRAN_64", uid = oracle.username, pwd = oracle.password)
# 
# #extract the data
# SSdata::extractLAND(path = "C:/DFO-MPO/SSdata/", e.year = 2021)
setwd("C:/git/Maritimes/SOTO_Maritimes/")
load("data/data_priv/SSdata/data/landings/landings.RData") #gets loaded as 'landings'

#limit to NAFOS we care about
tomatch=c('4VN','4VS','4W','4X')
landings = landings[ grep(paste(tomatch, collapse = '|'),landings$NAFO_UNIT),]

# add the general NAFO areas (e.g. want 4X, not 4XM)
landings$NAFO_GENERAL <- regmatches(landings$NAFO_UNIT, regexpr(paste(tomatch, collapse = "|"), landings$NAFO_UNIT))
landings$NAFO_UNIT <- NULL

sppPelagic<-c(202, 204, 256, 260, 263, 264, 272, 278, 280, 282, 284, 289, 299, 302, 310, 312, 340, 462, 468, 469, 474)
sppDemersal <- c(101,102,103,104,105,106,112,114,116,118,120,122,124,129,132,144,154,168,169,174,182,186,188,220, 274,459,479)
sppBenthicInverts <- c(529, 535, 536, 564, 602, 608, 610, 612, 614, 619, 620, 622, 639, 652)

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


# PELAGIC SPECIES
## Pelagics spp pulled from a document called "LandingsPelagic_SOTO.r/LPelagic_nafo.csv", provcided by Catalina Gomez
# LPelagic_SOTO <- read.csv("C:/Users/McMahonM/OneDrive - DFO-MPO/SOTO/SOTO/data/LandingsPelagic_SOTO.r/LPelagic_nafo.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
# sppPelagic <- unique(LPelagic_SOTO[,c("SppCodes", "Species")])
#
# SppCodes               Species
# 202      ATLANTIC HERRING
# 204     ATLANTIC MACKEREL
# 256 ATLANTIC WHITE MARLIN
# 260  ATLANTIC BLUE MARLIN
# 263   MAHI MAHI (DOLPHIN)
# 264             SWORDFISH
# 272         ALBACORE TUNA
# 278           BIGEYE TUNA
# 280 NORTHERN BLUEFIN TUNA
# 282         SKIPJACK TUNA
# 284        YELLOWFIN TUNA
# 289            TUNAS (NS)
# 299   PELAGIC FISHES (NS)
# 302               ALEWIFE
# 310         AMERICAN SHAD
# 312       ARGENTINES (NS)
# 340               CAPELIN
# 462             PORBEAGLE
# 468      GREAT BLUE SHARK
# 469     LARGE SHARKS (NS)
# 474         BASKING SHARK

# DEMERSAL SPP
# Demersal spp pulled from "SourceofIndicators.xlsx", provided by Catalina Gomez (doc was attachment of email "FW:fish.msg")
# SPECIES              ALLNAMES
# 101              ATLANTIC COD
# 102                   HADDOCK
# 103   ATLANTIC REDFISHES (NS)
# 104               SILVER HAKE
# 105                  RED HAKE
# 106          POLLOCK (SAITHE)
# 112           AMERICAN PLAICE
# 114            WITCH FLOUNDER
# 116       YELLOWTAIL FLOUNDER
# 118         GREENLAND HALIBUT
# 120          ATLANTIC HALIBUT
# 122           WINTER FLOUNDER
# 124           SUMMER FLOUNDER
# 129           FLATFISHES (NS)
# 132           AMERICAN ANGLER
# 144               CUSK (TUSK)
# 154     LUMPFISH (LUMPSUCKER)
# 168       ROUNDNOSE GRENADIER
# 169       ROUGHHEAD GRENADIER
# 174             SCULPINS (NS)
# 182                  TILEFISH
# 186                WHITE HAKE
# 188           WOLFFISHES (NS)
# 459            DOGFISHES (NS)
# 479               SKATES (NS)

#BENTHIC INVERTEBRATES
#These were all of the species tagged as invertebrates from the extracted "landings.RData" file, with the exception of "SQUIDS (NS)"
# SPECIES                 ALLNAMES
# 529               CLAMS (NS)
# 535        ICELANDIC SCALLOP
# 536              SEA SCALLOP
# 564         PERIWINKLES (NS)
# 602       ATLANTIC ROCK CRAB
# 608               JONAH CRAB
# 610               QUEEN CRAB
# 612                 RED CRAB
# 614          STONE KING CRAB
# 619        MARINE CRABS (NS)
# 620             SEA CUCUMBER
# 622         AMERICAN LOBSTER
# 639 PINK (=PANDALID) SHRIMPS
# 652               SEA URCHIN

