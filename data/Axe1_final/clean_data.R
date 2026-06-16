## Programme de jointure entre le fichier kobo et les cartes ##

library(readxl)
library(dplyr)
library(tidyr)
library(sf)
library(writexl)


## (A) Extraction des données kobo
don <- read_xlsx("kobo/ALTERMAP_2026-06-11_libelle.xlsx")
xml <- read_xlsx("kobo/ALTERMAP_2026-06-11_xml.xlsx")

### (A.1) Ajout des colonnes "Autres" aux nom de régions

 don$reg01<-case_when(don$`Région 1`=="Autre" ~ don$Reg_01_autre,.default = don$`Région 1`)
 don$reg02<-case_when(don$`Région 2`=="Autre" ~ don$Reg_02_autre,.default = don$`Région 2`)
 don$reg03<-case_when(don$`Région 3`=="Autre" ~ don$Reg_03_autre,.default = don$`Région 3`)
 don$reg04<-case_when(don$`Région 4`=="Autre" ~ don$Reg_04_autre,.default = don$`Région 4`)
 don$reg05<-case_when(don$`Région 5`=="Autre" ~ don$Reg_05_autre,.default = don$`Région 5`)
 don$reg06<-case_when(don$`Région 6`=="Autre" ~ don$Reg_06_autre,.default = don$`Région 6`)
 don$reg07<-case_when(don$`Région 7`=="Autre" ~ don$Reg_07_autre,.default = don$`Région 7`)
 don$reg08<-case_when(don$`Région 8`=="Autre" ~ don$Reg_08_autre,.default = don$`Région 8`)
 don$reg09<-case_when(don$`Région 9`=="Autre" ~ don$Reg_09_autre,.default = don$`Région 9`)
 don$reg10<-case_when(don$`Région 10`=="Autre" ~ don$Reg_10_autre,.default = don$`Région 10`)
 don$reg11<-case_when(don$`Région 11`=="Autre" ~ don$Reg_11_autre,.default = don$`Région 11`)
 don$reg12<-case_when(don$`Région 12`=="Autre" ~ don$Reg_12_autre,.default = don$`Région 12`)
 don$reg13<-case_when(don$`Région 13`=="Autre" ~ don$Reg_13_autre,.default = don$`Région 13`)
 don$reg14<-case_when(don$`Région 14`=="Autre" ~ don$Reg_14_autre,.default = don$`Région 14`)
 don$reg15<-case_when(don$`Région 15`=="Autre" ~ don$Reg_15_autre,.default = don$`Région 15`)
 don$reg16<-case_when(don$`Région 16`=="Autre" ~ don$Reg_16_autre,.default = don$`Région 16`)
 don$reg17<-case_when(don$`Région 17`=="Autre" ~ don$Reg_17_autre,.default = don$`Région 17`)
 don$reg18<-case_when(don$`Région 18`=="Autre" ~ don$Reg_18_autre,.default = don$`Région 18`)
 don$reg19<-case_when(don$`Région 19`=="Autre" ~ don$Reg_19_autre,.default = don$`Région 19`)

## (A.2) ajustement de la variable code
don$num <- don$`N° de questionnaire`
don$num <- as.numeric(don$num)+1000
don$code <- substr(don$num,2,4)
don$code 

# (A.3) sélection et recodage des variable utiles
don$lieu <- as.factor(don$`G.2 : Lieu d'enquête`)
don$date <- as.Date(don$`G.1 : Date de l'enquête`)
don$niveau <- don$`G.3 : Niveau d'étude`
don$sexe <- don$`A.1 : Sexe`
don$age <- 2026-don$`A.2 : Année de naissance`
don$nais <- xml$A3_Born
don$nais_pere<-xml$A3b_Born_father
don$nais_mere<-xml$A3c_Born_mother
don$nat1 <-xml$A4a_Nat1
don$nat2 <-xml$A4b_Nat2

don$revenu<- don$`A.10 Revenu familial`
don$etu_mere <- don$`A.11 Niveau d'étude de votre mère`
don$etu_pere <- don$`A.12 Niveau d'étude de lvotre père`

don$lang_ori1<-don$Langue1...20
don$lang_ori2<-don$Langue2...21
don$lang_ori3<-don$Langue3...22
don$lang_ori4<-don$Langue4...23
don$lang_ori_nb <- 4-(is.na(don$lang_ori1) +is.na(don$lang_ori2) + is.na(don$lang_ori3) + is.na(don$lang_ori4))
#table(don$lang_ori_nb)

don$lang_act1<-don$Langue1...24
don$lang_act2<-don$Langue2...25
don$lang_act3<-don$Langue3...26
don$lang_act4<-don$Langue4...27
don$lang_act_nb <- 4-(is.na(don$lang_act1) +is.na(don$lang_act2) + is.na(don$lang_act3) + is.na(don$lang_act4))
#table(don$lang_act_nb)

don$mig1 <- xml$A8_mig1
don$mig2 <- xml$A8_mig2
don$mig3 <- xml$A8_mig3
don$mig4 <- xml$A8_mig4
don$mig_nb <- 4-(is.na(don$mig1) +is.na(don$mig2) + is.na(don$mig3) + is.na(don$mig4))

don$mob1 <- xml$A9_mob1
don$mob2 <- xml$A9_mob2
don$mob3 <- xml$A9_mob3
don$mob4 <- xml$A9_mob4
don$mob_nb <- 4-(is.na(don$mob1) +is.na(don$mob2) + is.na(don$mob3) + is.na(don$mob4))


don$ech1_local<- don$`B.1 : Sentiment d'appartenance : échelles  multiples/... Une ville, un quartier, un village`
don$ech2_regional <- don$`B.1 : Sentiment d'appartenance : échelles  multiples/... Un département, une région`
don$ech3_national <- don$`B.1 : Sentiment d'appartenance : échelles  multiples/... Un pays`
don$ech4_continental <- don$`B.1 : Sentiment d'appartenance : échelles  multiples/... Un continent, une partie du Monde`
don$ech5_mondial <-don$`B.1 : Sentiment d'appartenance : échelles  multiples/...  Le Monde, la Terre`
don$ech6_autre <- don$`B1 : Sentiment d'appartenance :  autre`
don$ech_principale <- don$`B.1 : Sentiment d'appartenance principal`
don$att1 <- xml$B2_STALIK1
don$att2 <- xml$B2_STALIK2
don$att3 <- xml$B2_STALIK3
don$att4 <- xml$B2_STALIK4_001
don$att5 <- xml$B2_STALIK5
don$rep1 <- xml$B2_STAUNL1
don$rep2 <- xml$B2_STAUNL2
don$rep3 <- xml$B2_STAUNL3
don$rep4 <- xml$B2_STAUNL4
don$rep5 <- xml$B2_STAUNL5
don$mots_eur <- paste(don$`Europe 1`, don$`Europe 2`, don$`Europe 3`, don$`Europe 4`, don$`Europe 5`, sep=", ")
don$mots_eur <- gsub("NA","", don$mots_eur)
don$mots_med <- paste(don$`Méditerranée 1`, don$`Méditerranée 2`, don$`Méditerranée 3`, don$`Méditerranée 4`, don$`Méditerranée 5`, sep=", ")
don$mots_med<- gsub("NA","", don$mots_med)
don$mots_afr <- paste(don$`Afrique 1`, don$`Afrique 2`, don$`Afrique 3`, don$`Afrique 4`, don$`Afrique 5`, sep=", ")
don$mots_afr<- gsub("NA","", don$mots_afr)


## (A.3) Préparation du tableau de jointure
tab <- don %>% select(code, lieu, date, niveau, sexe, age, nais, 
                      nais_mere, nais_pere, nat1, nat2,
                      revenu, etu_mere, etu_pere, 
                      lang_ori1,lang_ori2,lang_ori3,lang_ori4,lang_ori_nb,
                      lang_act1,lang_act2,lang_act3,lang_act4,lang_act_nb,
                      mig1,mig2,mig3,mig4,mig_nb,
                      mob1,mob2,mob3,mob4,mob_nb,
                      ech1_local, ech2_regional, ech3_national, ech4_continental, ech5_mondial, ech6_autre, ech_principale,
                      att1,att2,att3,att4,att5, rep1,rep2,rep3, rep4, rep5,
                      mots_eur, mots_med,mots_afr,
                      reg01,reg02, reg03,reg04, reg05,reg06,reg07,reg08, reg09, reg10,
                      reg11, reg12, reg13, reg14, reg15, reg16, reg17, reg18, reg19)

col <- pivot_longer(tab,cols = c( reg01,reg02, reg03,reg04, reg05,reg06,reg07,reg08, reg09, reg10,
                                  reg11, reg12, reg13, reg14, reg15, reg16, reg17, reg18, reg19)
                    )%>%
          filter(is.na(value)==F) %>%
          mutate(id = as.numeric(substr(name,4,7))) %>%
          select(code, lieu, id,reg_nom=value)



# (B) Prépration des fichiers géométriques

## (B.1) UPC

listmap <- list.files("map_FRA")
k <- length(listmap)
map <- st_read(paste0("map_FRA/", listmap[1]))
map$code<-substr(listmap[1],4,6) 
for (i in 2:k) {
  map2 <- st_read(paste0("map_FRA/", listmap[i]))
  map2$code<-substr(listmap[i],4,6)
  map<-rbind(map,map2)
}
map_FRA <- map

## (B.2) USSEIN

listmap <- list.files("map_SEN/")
k <- length(listmap)
map <- st_read(paste0("map_SEN/", listmap[1]))
map$code<-substr(listmap[1],4,6) 
for (i in 2:k) {
  map2 <- st_read(paste0("map_SEN/", listmap[i]))
  map2$code<-substr(listmap[i],4,6)
  map<-rbind(map,map2)
}
map_SEN <- map

## (B.3) Assemblage et agrégation

maps <- rbind(map_FRA, map_SEN) %>% group_by(id, code) 
plot(maps$geometry)

# (B.4) Jointure

mapfin <- left_join(maps, col)

# (B.4) Ajout de la colonne map_OK au fichier principal

etud <- unique(map_reg$code)
etudOK <- data.frame(code=etud, map_OK=1)

tabfin<-left_join(tab, etudOK)
tabfin$map_OK[is.na(tabfin$map_OK)]<-0
table(tabfin$map_OK)

# (C) Sauvegarde des deux fichiers
saveRDS(mapfin,"data/map_reg.RDS")
st_write(mapfin, "data/map_reg.geojson")
saveRDS(tabfin,"data/survey.RDS")
write_xlsx(tabfin, "data/survey.xlsx")
