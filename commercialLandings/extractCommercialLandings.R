library(RODBC)
library(SSdata)
# account performing extractions must be granted "CL_USER_ROLE"
channel <<- odbcConnect("PTRAN_64", uid = oracle.username, pwd = oracle.password)
# #extract the data up to and including e.year
SSdata::extractLAND(path = "C:/git/Maritimes/SOTO_Maritimes/data/data_priv/SSdata/data/landings/", e.year = 2021)