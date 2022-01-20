# Calculate biomass for all demersal species in 4VWX

library (lubridate)
library(plyr)
library (dplyr)
library (ggplot2)
library(rgdal)
library(rgeos)
library (reshape2)
library (stringr)
library(ROracle)
library(readr)
library(tidyr)
library(cowplot)
library(scales)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, "PTRAN")

# load raw data from Oracle Database
gsinf <- ROracle::dbGetQuery(channel, paste("select MISSION, SETNO, SDATE,STRAT, SLAT, SLONG,DIST,DUR from groundfish.gsinf where type =1"))
gscat <- ROracle::dbGetQuery(channel, paste ("select MISSION, SETNO, TOTWGT, SPEC, SAMPWGT, SIZE_CLASS from groundfish.gscat"))
# Information on stratum (area)
gsstratum <- ROracle::dbGetQuery(channel, paste ("select * from groundfish.gsstratum"))

#### Setting strata groupings ####
#Assign strata to different stock components
s.4X <- "4X"
STRAT <- c(seq (470, 495))
s.4X <- data.frame (s.4X, STRAT)

s.4VW <- "4VW"
STRAT <- c(seq (440, 466))
s.4VW <- data.frame (s.4VW, STRAT)

rm (STRAT)

# Create DF with all zones
# Merge with subsetted data later
strattemp <- subset (gsstratum, select = "STRAT") 
strattemp <- subset (strattemp, (STRAT %in% c(seq (440, 495)))) 
stratzones <- merge (strattemp, s.4X, all.x = T)
stratzones <- merge (stratzones, s.4VW, all.x = T)

stratzones$s.4X <- as.factor (ifelse (is.na (stratzones$s.4X), "NO", "4X"))
stratzones$s.4VW <- as.factor (ifelse (is.na (stratzones$s.4VW), "NO", "4VW"))
rm (strattemp)


#### Merge gsinf with all strata groupings ####
gsinf1 <- merge (gsinf, s.4X, all.x = T)
gsinf1$s.4X <- as.factor (ifelse (is.na (gsinf1$s.4X), "NO", "4X"))

gsinf2 <- merge (gsinf1, s.4VW, all.x = T)
gsinf2$s.4VW <- as.factor (ifelse (is.na (gsinf2$s.4VW), "NO", "4VW"))

## Dataframe with all strata groupings
gsinf.sgroup <- gsinf2
# Change AREA to NAFO AREA to avoid confusion with stratum area in gsstratum
names (gsinf.sgroup) [names (gsinf.sgroup) == "AREA"] <- "NAFOAREA"

rm (gsinf1, gsinf2, s.4X, s.4VW)


#### Set up main df's with catch records ####
## Building data frame with catch info
gsweight <- merge (gscat, gsinf)
gsweight$year <- year (gsweight$SDATE)
gsweight$month <- month (gsweight$SDATE)


# Retain only summer survey data (Drops problems in 82/83)
SSweight <- gsweight [gsweight$month > 5,]
SSweight <- SSweight [SSweight$month < 9,]
# Split time series by trawl type
SSweight$Trawl <- ifelse (SSweight$year < 1982, "Yank", "WIIA")
SSYank <- SSweight [SSweight$Trawl == "Yank",]
# Removes WIIA trawls prior to 1982 
SSYank2 <- SSYank [SSYank$Trawl == "Yank",]
SSWIIA <- SSweight [SSweight$Trawl == "WIIA",]
# Join time series back together
SSweight2 <- rbind (SSYank2, SSWIIA)

# Standardize weight for distance of tow (1.75nm)
SSweight2$std.WGT <- SSweight2$TOTWGT * (1.75/SSweight2$DIST)
SSweight2$MISSIONSET = paste(SSweight2$MISSION, SSweight2$SETNO, sep="_")

nsets <- ddply (SSweight2,. (year, STRAT), summarize, nsets = (length (unique (MISSIONSET))))

# Add number of sets to data frame 
SSweight3 <- merge (SSweight2, nsets)

### Weight Mean column from STRANAL
annualWGT1 <- ddply (SSweight3,. (year, SPEC, STRAT), summarize, meanWGT = sum (std.WGT/nsets))

#Only retain strata for 4VWX
stratsel <- c (seq (440, 495))
stratsel <- as.factor (stratsel)
annualWGT1 <- annualWGT1 [annualWGT1$STRAT %in% stratsel,]
annualWGT1[is.na(annualWGT1)] <- 0

# Add 0 data for any missing years/stratum (by species)
# list of all years (regardless of species) with all species (regardless of years observed)
years <- expand.grid (year = unique (annualWGT1$year),
                      SPEC = unique (annualWGT1$SPEC),
                      STRAT = unique (annualWGT1$STRAT))
# Add to annualWGT1 with NA's
annualWGT2 <- merge (years, annualWGT1, all = TRUE)
# Select summer survey strata for 4VWX
annualWGT2 <- subset (annualWGT2, (STRAT %in% c (seq (440, 495))))
# replace NA's with 0
annualWGT2 [is.na (annualWGT2)] <- 0

# Add Strat info 
strat <- subset (gsstratum, select=c("STRAT", "AREA", "NAME")) 
strat <- subset (strat, (STRAT %in% c (seq (440, 495))))

# merge strat info with summarized weight file
annualWGT3 <- merge (annualWGT2, strat)

# Calculate trawlable units in each strat (WS = 41 ft converted to nm)
annualWGT3$CF.Strat <- annualWGT3$AREA / 0.011801

# Mean weight of fish captured in sampling area 
# Weight Total column from STRANAL
annualWGT3$strat.WGT <- annualWGT3$meanWGT * annualWGT3$CF.Strat 

# Apply species specific gear conversion factors
CFgear <- c (1.2, 2.3, 1.82, 0.8, 0.8, 0.8, 0.8)
SPEC <- c (11, 14, 23, 40, 41, 42, 43)
CFgear.df <- data.frame (SPEC, CFgear)
# Merge with main df and separate to years where conversion is/isnt applied
annualWGT3.70 <- annualWGT3 [annualWGT3$year < 1983,]
annualWGT3.70 <- merge (annualWGT3.70, CFgear.df, all = T)
annualWGT3.70$CFgear [is.na (annualWGT3.70$CFgear)] <- 1
annualWGT3.83 <- annualWGT3 [annualWGT3$year > 1982,]
annualWGT3.83$CFgear <- 1
annualWGT3b <- rbind (annualWGT3.70, annualWGT3.83)

# Calculate converted weight in tonnes
annualWGT3b$tonnes <- (annualWGT3b$strat.WGT * annualWGT3b$CFgear)/1000

# Merge strat zones to subset by assessment area 
annualWGT4 <- merge (annualWGT3b, stratzones)
#Subset out demersal species - list of codes came from Catalina Gomez did not match data provided to me so used SPEC<1001 + 1191
#annualWGT4 <- subset(annualWGT4, (SPEC %in% c(10,11,12,13,14,15,16,17,18,19,20,21,23,25,28,30,31,35,40,41,42,43,44,49,50,51,52,59,140,141,142,143,110,111,112,114,115,117,118,200,201,202,203,204,211,220,221,300,301,304,310,320,340,350,400,620,650,122,123,142,143,149,156,216,302,303,306,307,313,314,316,331,341,410,411,412,414,501,503,505,508,512,520,595,602,603,604,616,617,619,620,621,622,623,624,625,626,628,630,631,632,633,637,640,641,647,704,714,742,743,744,816,880)))
annualWGT5 <- subset(annualWGT4, SPEC<1001|SPEC=="1191")
# Remove pelagic species
annualWGT5<-annualWGT5[ which( ! annualWGT5$SPEC %in% c(seq (60, 89))), ]

# Calculate tonnes by year in each strata zone required
annualWGT.4X <- annualWGT5 [annualWGT5$s.4X == "4X",]
annualWGT.4VW <- annualWGT5 [annualWGT5$s.4VW == "4VW",]
# Creates summarized data frame for each strata zone needed
annualWGT4X <- ddply (annualWGT.4X,. (year, SPEC), summarize, sum.tonnes = sum (tonnes), ZONE = unique (s.4X))
annualWGT4VW <- ddply (annualWGT.4VW,. (year, SPEC), summarize, sum.tonnes = sum (tonnes), ZONE = unique (s.4VW))

#Select most recent year - 
CRNT.YR <- 2020 #2020 was selected because 2021 summer survey biomass estimates are not available until conversion factors are calculated between Needler and Cartier
WGTyear4X <- annualWGT4X [annualWGT4X$year < (CRNT.YR + 1),]
WGTyear4VW <- annualWGT4VW [annualWGT4VW$year < (CRNT.YR + 1),]
WGTyear4VW<-WGTyear4VW[!(WGTyear4VW$year=="2018"),] # Removes 2018 when there was no survey coverage of 4VW

# Write to csv to update master file
write.csv(
  WGTyear4X ,
  "C:/RV Surveys/R Files/demersalbiomass_4X.csv"
)
write.csv(
  WGTyear4VW ,
  "C:/RV Surveys/R Files/demersalbiomass_4VW.csv"
)


# Plot Scotian Shelf Demersal Fish

#Import data and gather species into one factor variable by Division
#Data is divided into 4VW and 4X 
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(scales)

MAR <- read_csv("MAR_Demersal_Jan18_2022.csv")

#4VW Data
MAR_4VW <- MAR %>% 
  select(YEAR, Cod_4VW, Haddock_4VW, SilverHake_4VW, Redfish_4VW, Halibut_4VW, Dogfish_4VW) %>% gather("species", "tonnes", 2:7)

#Select year and total survey values into another variable
MAR2_4VW <- MAR %>% select(YEAR, TotDemersal_4VW)

#4X East Data
MAR_4X <- MAR %>% select(YEAR, Cod_4X, Haddock_4X, SilverHake_4X, Redfish_4X, Halibut_4X, Dogfish_4X) %>% gather("species", "tonnes", 2:7)
MAR2_4X <- MAR %>% select(YEAR, TotDemersal_4X)

#ENGLISH VERSION

#4VW Plot

#Colours for species
col <- c("Cod_4VW" = "#00bcd4",
         "Redfish_4VW" = "#E91E63",
         "Haddock_4VW" = "#FDD835",
         "SilverHake_4VW" = "#80DEEA",
         "Dogfish_4VW" = "#000000",
         "Halibut_4VW" = "#F8BBD0")
col2 <- c("TotDemersal_4VW" = "grey90")

#Legend labels
txt <- c("Cod_4VW" = "Atlantic cod",
         "Redfish_4VW" = "Redfish",
         "Haddock_4VW" = "Haddock",
         "SilverHake_4VW" = "Silver hake",
         "Dogfish_4VW" = "Spiny dogfish",
         "Halibut_4VW" = "Atlantic halibut")
txt2 <- c("TotDemersal_4VW" = "4VW survey total demersal fish biomass")

P1 <- ggplot(NULL) +
  geom_col(data=MAR2_4VW, aes(x=YEAR, y=TotDemersal_4VW/1000, fill="TotDemersal_4VW")) +
  geom_line(data=MAR_4VW, aes(x=YEAR, y=tonnes/1000, colour=species), size=1) +
  geom_point(data=MAR_4VW, aes(x=YEAR, y=tonnes/1000, colour=species)) +
  scale_fill_manual(values=col2, labels=txt2) +
  scale_colour_manual(values = col, labels=txt) +
  scale_y_continuous(expand=c(0,0), limits = c(0, 800)) +
  scale_x_continuous(expand = c(0,0.5), limits = c(1969, 2022))

P1 + ggtitle("4VW Scotian Shelf Survey - Demersal Fish") +
  labs(y=("Survey Biomass (thousands of tonnes)")) +
  theme(plot.title = element_text(size=12),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "black", size = 1.5),
        legend.position = c(0.8,0.8),
        legend.text = element_text(size=8),
        legend.title=element_blank(),
        legend.spacing.y = unit(-0.2, "cm"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10))


#4X East Plot

#Colours for species
col3 <- c("Cod_4X" = "#00bcd4",
          "Redfish_4X" = "#E91E63",
          "Haddock_4X" = "#FDD835",
          "SilverHake_4X" = "#80DEEA",
          "Dogfish_4X" = "#000000",
          "Halibut_4X" = "#F8BBD0")
col4 <- c("TotDemersal_4X" = "grey90")

#Legend labels
txt3 <- c("Cod_4X" = "Atlantic cod",
          "Redfish_4X" = "redfish",
          "Haddock_4X" = "haddock",
          "SilverHake_4X" = "silver hake",
          "Dogfish_4X" = "spiny dogfish",
          "Halibut_4X" = "Atlantic halibut")
txt4 <- c("TotDemersal_4X" = "4X survey total demersal fish biomass")


P2 <- ggplot(NULL)+
  geom_col(data=MAR2_4X, aes(x=YEAR, y=TotDemersal_4X/1000, fill="TotDemersal_4X")) +
  geom_line(data=MAR_4X, aes(x=YEAR, y=tonnes/1000, colour=species), size=1) +
  geom_point(data=MAR_4X, aes(x=YEAR, y=tonnes/1000, colour=species)) +
  scale_colour_manual(values = col3, labels=txt3) +
  scale_fill_manual(values = col4, labels=txt4) +
  scale_y_continuous(expand = c(0,0.5), limits = c(0,800)) +
  scale_x_continuous(expand = c(0,0.5), limits = c(1969, 2022))

P2 + ggtitle("4X Scotian Shelf Survey - Demersal Fish") +
  labs(y=("Survey Biomass (thousands of tonnes)")) +
  theme(plot.title = element_text(size=12),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "black", size = 1.5),
        legend.position = c(0.12,0.87),
        legend.text = element_text(size=8),
        legend.title=element_blank(),
        legend.spacing.y = unit(-0.2, "cm"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10))



