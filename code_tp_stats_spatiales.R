library(sf)
library(dplyr)

commune_francemetro = st_read("fonds/commune_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252")
str(commune_francemetro)
summary(commune_francemetro)


View(commune_francemetro[1:10,])

st_crs(commune_francemetro)

communes_Bretagne = commune_francemetro %>% filter(reg == 53)%>% select(c(1,2,11, 14,16))
str(communes_Bretagne) #toujours un sf (et dataframe)
plot(communes_Bretagne, lwd= 0.1)
plot(st_geometry(communes_Bretagne), lwd= 0.1) #ne conserve que la géométrie de la region

communes_Bretagne$surf2 = st_area(st_geometry(communes_Bretagne))
#= geometry
#mutate
units(communes_Bretagne$surf2) <- "km^2"

#Q11
#C different parce que surf crée a partir d'un polygone plus précis "valeur réelle"

#Q12
dept_bretagne = communes_Bretagne %>% 
  group_by(dep) %>% 
  mutate(superficie_dep = sum(surf)) %>% 
  select(dep, superficie_dep) %>% 
  distinct(dep, .keep_all = TRUE)

plot(st_geometry(dept_bretagne))
