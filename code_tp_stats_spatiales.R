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

units(communes_Bretagne$surf2) <- "km^2"

# Q11
#C different parce que surf crée a partir d'un polygone plus précis "valeur réelle"

# Q12
# summarize écrase les données autant de ligne que de groupes
depts_bretagne <- communes_Bretagne %>% 
  group_by(dep) %>% 
  summarise(
    surf = sum(surf)
  )
str(depts_bretagne) # tjrs objet sf
plot(depts_bretagne %>% st_geometry())

# Q13
# st_geo pour garder que la geometry
communes_Bretagne %>% st_union() %>% st_geometry() %>% plot()
depts_bretagne_geo <- communes_Bretagne %>% 
  group_by(dep) %>% 
  summarise(
    geometry = st_union(geometry)
  )
plot(depts_bretagne_geo)
#on a gardé seulement dep et geometry

# Q14

centr_depts_bretagne = depts_bretagne_geo %>% 
  st_centroid()
str(centr_depts_bretagne)
st_crs(centr_depts_bretagne)

plot(depts_bretagne_geo %>% st_geometry())
plot(centr_depts_bretagne %>% st_geometry(), add = TRUE)

#Nouveau dataframe
lib_depts = data.frame(
  code = as.character(c(22,29,35,56)), #pour jointure ap
  lib = c("Cotes-d'Armor", "Finistère", "Ille-et-Vilaine", "Morbihan")
)

centr_depts_bretagne <- centr_depts_bretagne %>% 
  left_join(
    lib_depts,
    by = c("dep" = "code") #champ qui décrit la clef
  )

coords_centr <- st_coordinates(centr_depts_bretagne) %>% 
  bind_cols( #be sure de l'ordre des lignes
    centr_depts_bretagne %>%
      select(dep, lib) %>% 
      st_drop_geometry()
  )
str(coords_centr)

plot(depts_bretagne_geo %>% st_geometry())
plot(centr_depts_bretagne %>% st_geometry(), add = TRUE)
text(coords_centr, labels = coords_centr$lib, adj = c(0.5,-0.2))

library(ggplot2)
ggplot2::ggplot() + geom_sf(data= depts_bretagne_geo) +  #geom des dep
  geom_sf(data=centr_depts_bretagne) + #geom des centroides
  geom_sf_text(data=centr_depts_bretagne, aes(label=lib), size=4)+
  theme_void()

# Q15

#intersection de deux fonds st_intesect renvoie une liste

communes_centr_depts = st_intersects(
  centr_depts_bretagne, #liste longueur dictée par le 1er
  communes_Bretagne
)
str(communes_centr_depts)

communes_centr_depts = st_intersection( #renvoie un polygone
  centr_depts_bretagne, #liste longueur dictée par le 1er
  communes_Bretagne
)

#est-ce que mes centres sont dans mes communes
communes_centr_depts = st_within(
  centr_depts_bretagne, #liste longueur dictée par le 1er
  communes_Bretagne
)


# Q17

chefs_lieux = communes_Bretagne %>% filter(libelle %in% c("Rennes", "Saint-Brieuc", "Quimper", "Vannes")) %>% 
  st_centroid()

distances = st_distance(chefs_lieux %>%  arrange(code),
                        communes_centr_depts_sf %>% arrange(code),
                        by_element = TRUE) #pour avoir la distance uniquement entre les elmt de mm position
#distance entre x1 et y1 puis x2 et y2 etc.
#avec false on recupere une matrice

name(distances) = c(22,29,35,53)

# 18 - communes à moins de 20 ####
# a buffer 
buff_centr_sf <- st_buffer(communes_centr_depts_sf, 20000) #unite de fonds en m
buff_centr_sf %>%  str()
buff_centr_sf %>% st_geometry() %>% plot()
plot(communes_centr_depts_sf %>% st_geometry(), add = TRUE)

#b
# Attention: Deux résultats possibles
# Intersection
# Ne retenir que la partie des communes intersectantes qui est dans le buffer
com_buff_sf <- st_intersection(
  communes_bretagne,
  buff_centr_sf
)
str(com_buff_sf)
plot(com_buff_sf %>% st_geometry())

# OU st_intersects
# Permet de récupérer l'intégralité des polygones des communes intersectantes

com_buff2_index <- st_intersects(buff_centr_sf, communes_bretagne)
com_buff2_sf <- communes_bretagne[unlist(com_buff2_index),]

library(ggplot2)

ggplot() +
  geom_sf(data = com_buff2_sf, fill = NA, colour = "grey75") +
  geom_sf(data = com_buff_sf, fill = "steelblue") +
  theme_void()

com_buff2_sf %>% st_drop_geometry() %>%  count(dep)
com_buff_sf %>% st_drop_geometry() %>%  count(dep)
# même résultat en termes de nb de communes (attendu)


# 19 - st_transform -------------------------------------------------------

communes_bretagne_wgs84 <- communes_bretagne %>% 
  st_transform(crs = 4326) %>% 
  mutate(surf3 = st_area(geometry))
st_crs(communes_bretagne_wgs84)

par(mfcol = c(1,2))
plot(communes_bretagne_wgs84 %>% st_geometry(), col = "steelblue")
plot(communes_bretagne %>% st_geometry(), col = "steelblue")
dev.off()

ggplot() +
  geom_sf(data = communes_bretagne, fill = NA) +
  theme_void()

communes_bretagne_wgs84 %>% 
  mutate(surf3 = units::set_units(surf3, "km^2")) %>%
  select(starts_with("surf")) %>% 
  summary()
