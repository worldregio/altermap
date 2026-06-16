library(sf,quietly = T, verbose=F, warn.conflicts = F,)
library(dplyr,quietly = T, verbose=F,warn.conflicts = F)
library(mapsf)
library(leaflet)
library(stringr)

## Load maps

stu <- readRDS("data/Axe4_final/geom/student_maps_3857.RDS")
reg <- readRDS("data/Axe4_final/geom/Senegal_regions_3857.RDS")
gri <- readRDS("data/Axe4_final/geom/Senegal_grille_3857.RDS")
mask <-readRDS("data/Axe4_final/geom/Senegal_mask_3857.RDS")


## Select regions according to a name

stu$fouta <- str_detect(tolower(maps$nom_reg),"fout")
sel <-stu %>% filter(fouta==1)

## Check
mf_map(reg, col="lightyellow")
mf_map(sel, 
       col="blue",
       alpha=0.1, 
       border="red",
       add=T)
plot(mask$geometry,add=T, col="white", border="white")
mf_map(reg, col=NA, border="black", add=T)
mf_label(reg, var="NAME_1",halo=T)
mf_layout(title = "Noms de région comprenant Fouta* (16 étudiants)",
          credits="Altermap 2026 - Axe 4",
          frame=T,
          scale=T,
          arrow=T)

