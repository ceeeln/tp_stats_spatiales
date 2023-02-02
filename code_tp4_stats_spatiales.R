rm(list=ls())
library(dplyr)
library(sf)
library(mapsf)
library(classInt)
library(leaflet)
##############Exercice 1 ##########
#data importation
pop19 = readxl::read_xlsx("fonds/Pop_legales_2019.xlsx")
# Q1
pop19 = pop19 %>% mutate(
  #on change le code commune que si 75
  COM = ifelse(substr(COM,1,2) == 75, "75056", COM)) %>% 
  group_by(COM) %>% 
  #ungroup the data
  summarise(PMUN19 = sum(PMUN19), .groups = 'drop')
str(pop19)

metro_sf = st_read("fonds/Fonds_carte/France_metro/commune_francemetro_2021.shp",
                   options = "ENCODING=WINDOWS-1252") %>% 
  left_join(pop19, by=c('code'='COM')) %>% 
  mutate(DENSITE= PMUN19/surf)
str(metro_sf)

# Q2 distribution de la densite
plot(metro_sf$DENSITE)

# Q3 carte choroplèthe
plot(metro_sf["DENSITE"], border=FALSE)

# Q4 Représenter le résultat de la discrétisation de la densité de population 
#selon les méthodes des quantiles, jenks et des écarts-types, 
#ainsi que la méthode pretty

#by default five classes
plot(metro_sf["DENSITE"], border=FALSE, breaks="quantile")
#jenks construit les classes les plu diff possibles
plot(metro_sf["DENSITE"], border=FALSE, breaks="jenks")
plot(metro_sf["DENSITE"], border=FALSE, breaks="sd")
plot(metro_sf["DENSITE"], border=FALSE, breaks="pretty")
#Plupart du territoire francais vide Q3:95 mais max 27000
quantile(metro_sf$DENSITE)

# Q5 comparer la distribution de la variable
#continue avec celle de la variable discrétisee

#construction intervalle a partir des donnees
n=5
quantile_decoupe <- classInt::classIntervals(
  metro_sf$DENSITE,
  style = "quantile", #method
  n) #number of class
summar(quantile_decoupe$var) #quantile initial
summary(quantile_decoupe$brks) #borne des intervalles/ quantile avec de nouvelles classes

#construction var categorielle
table(cut(metro_sf$DENSITE,
          breaks= quantile_decoupe$brks,
          include.lowest = TRUE,
          right=FALSE))

# b
pal1 <- RColorBrewer::brewer.pal(n = 5, name = "YlOrRd")

plot(quantile_decoupe, pal = pal1, main= "Découpage quantile")

# c

# d
metro_sf = metro_sf %>% mutate(
  DENSITE_cat = cut(
    DENSITE,
    breaks = c(0,40,162,1000,8000,27310), #borne chacun des intervalles
    include.lowest = TRUE,
    right = FALSE,
    ordered_result = TRUE #yes our class are ordered
  )
)

#useNA : pour verifier pas de NA dans nos donnees
table(metro_sf$DENSITE_cat, useNA = "always")
plot(metro_sf["DENSITE_cat"], border=FALSE, pal=pal1)


###########Exercice 2#############
# Q1
dep_sf = st_read("fonds/Fonds_carte/France_metro/dep_francemetro_2021.shp",
        options = "ENCODING=WINDOWS-1252")
tx_pauvrete = readxl::read_xlsx("fonds/Donnees/Taux_pauvrete_2018.xlsx")

mer = st_read("fonds/Fonds_carte/merf_2021/merf_2021.shp",
              options = "ENCODING=WINDOWS-1252")

#Jointure pour ajouter le taux de pauvreté
dep_sf = dep_sf %>% left_join(tx_pauvrete %>% select(-Dept), by=c('code'= 'Code'))
summary(dep_sf$Tx_pauvrete)

# Méthode de Fisher
mf_map(dep_sf,
       "Tx_pauvrete",
       type="choro",
       nbreaks = 4,
       breaks = "fisher")

# Méthode equals
mf_map(dep_sf,
       "Tx_pauvrete",
       type="choro",
       nbreaks = 4,
       breaks = "equal")

# Méthode quantile
mf_map(dep_sf,
       "Tx_pauvrete",
       type="choro",
       nbreaks = 4,
       breaks = "quantile")

# Q2 Découpage manuel

dep_sf = dep_sf %>% mutate(
  tx_pauvrete_cat = cut(
    Tx_pauvrete,
    breaks = c(0,13,17,25,max(Tx_pauvrete)), #borne chacun des intervalles
    include.lowest = TRUE,
    right = FALSE,
    ordered_result = TRUE #yes our class are ordered
  )
)

# Zoom sur paris
dep_idf = dep_sf %>% filter(code %in% c(75,92,93,94))

# exportation pdf
pdf(file= "macarte.pdf", width =9, height=11)

# Current map
mf_map(dep_sf,
       "Tx_pauvrete",
       type="choro",
       breaks=c(0,13,17,25,max(dep_sf$Tx_pauvrete)),
       leg_pos = NA)

# Ajout encadré / To add an inset map to the current map
mf_inset_on(dep_sf, pos="topright", cex=0.2)

# Initialization of the inset
mf_init(dep_idf)

mf_map(dep_idf,
       "Tx_pauvrete",
       type="choro",
       breaks=c(0,13,17,25,max(dep_sf$Tx_pauvrete)),
       leg_pos = NA, #enlever la legende
       add = T)

# Label
mf_label(
  dep_idf,
  var = "code",
  color = "black"
)

# To close the inset
mf_inset_off()

# Ajout de la mer
mf_map(mer, col="steelblue", add= TRUE)

# Legend
mf_legend(
  type = "choro",
  title = "Taux de pauvreté",
  val = c("", "Moins de 13", "De 13 à moins de 17", "De 17 à 25", "Supérieur à 25"),
  pal = "Mint",
  pos = "left"
)

mf_layout(title = "Taux de pauvreté par département en 2018",
          credits= "Source : Insee - © IGN - Insee")
# ferme le pdf
dev.off()

#exportation fond de carte
sf::st_write(dep_sf, "dept_tx_pauvrete_2018.gpkg")

##########Exercice 3############
reg_sf = st_read("fonds/Fonds_carte/France_metro/reg_francemetro_2021.shp",
                 options = "ENCODING=WINDOWS-1252")
#tx_pauvrete = readxl::read_xlsx("fonds/Donnees/Pop_region_2019.xlsx")

