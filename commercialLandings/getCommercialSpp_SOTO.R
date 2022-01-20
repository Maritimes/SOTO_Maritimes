#' getCommercialSpp_SOTO
#' 
#' This is an overly complex file used to grab the vectors of (SOTO) species codes making up
#' the categories of "Pelagic", "Demersal" and "Benthic Invertebrates"
#' 
#' I've created it this way so that the actual names of species can be listed within each function
getPelagicSpp_SOTO <- function(){
    # PELAGIC SPECIES
  ## Pelagics spp were pulled from a document called "LandingsPelagic_SOTO.r/LPelagic_nafo.csv", provcided by Catalina Gomez
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
  sppPelagic<-c(202, 204, 256, 260, 263, 264, 272, 278, 280, 282, 284, 289, 299, 302, 310, 312, 340, 462, 468, 469, 474)
  return(sppPelagic)
}
getDemersalSpp_SOTO <- function(){
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
sppDemersal <- c(101,102,103,104,105,106,112,114,116,118,120,122,124,129,132,144,154,168,169,174,182,186,188,220, 274,459,479)
return(sppDemersal)
}
getBenthicInvertsSpps_SOTO <- function(){
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
sppBenthicInverts <- c(529, 535, 536, 564, 602, 608, 610, 612, 614, 619, 620, 622, 639, 652)
return(sppBenthicInverts)
}

