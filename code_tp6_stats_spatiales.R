# Chargement des packages
library(dplyr)
library(sf)
library(spdep)
library(RColorBrewer)

# Import des données
iris<-st_read("./fonds/iris_franceentiere_2021/iris_franceentiere_2021.shp")
data<-read.csv2("./data/BASE_TD_FILO_DISP_IRIS_2018.csv",sep=";",dec=".")

# Jointure
marseille<-iris %>% 
  filter(substr(depcom,1,3)=="132") %>% 
  left_join(
    data %>% select(code=IRIS,DISP_MED18),
    by="code"
  )

# Q2
# Convertir le système de projection en Lambert-93 (EPSG 2154)

