## Programme de jointure entre le fichier kobo et les cartes ##

library(readxl)
library(dplyr)
library(tidyr)
library(sf)
library(writexl)


## (A) Extraction des données kobo
don <- read_xlsx("kobo/ALTERMAP_2026_gouvernance_V6_libelle.xlsx")
xml <- read_xlsx("kobo/ALTERMAP_2026_gouvernance_V6_xml.xlsx")

## (A.1) Chargement des données de numérisation
map <- readRDS("data/maps_regions_senegal_VF.RDS")
code_numer<-unique(names(table(map$code)))


## (A.2) ajustement de la variable code
don$code <- don$`G.0 : Code questionnaire`
don$code <- gsub("-","",don$code)

don$numerisation <- as.factor(don$code %in% code_numer)
levels(don$numerisation) <- c("Non","Oui")

# (A.3) sélection et recodage des variable utiles
don$niveau <- as.factor(substr(don$code,1,2))
don$lieu <- don$niveau
levels(don$lieu) <- c("UCAD", "UGTD","UGB")
don$date <- as.Date(don$start)
don$sexe <- don$`A.1 : Sexe`
don$age <- 2026-don$`A.2 : Année de naissance`
don$age[don$age <0]<-NA
don$nais_pays <- xml$A3_Pays
don$nais_dept <- xml$A31_dept
don$nais_pere<-xml$A32_Pays_Pere
don$nais_mere<-xml$A32_Pays_Mere

don$mobil<- as.factor(don$`A.4 : Avez vous toujours vécu dans la même commune / localité au Sénégal`)
don$mobil_nat <- don$`A.5.1 : Indiquez le liste des autres communes du Sénégal où vous avez vécu plus de six mois`
don$mobil_int <- don$`A.5.2 : Indiquez la listes des autres pays où vous avez vécu plus de six mois`

don$ech1 <- don$`1st choice`
don$ech2 <- don$`2nd choice`
don$ech3 <- don$`3rd choice`
don$ech_autre <- don$`B.1.2 : Si vous avez indiquez "Autre", précisez`

don$app_cayor <-don$`B.2 Cochez les espaces auxquels vous vous sentez appartenir, même en dehors des frontières officielles.  /Cayor`
don$app_baol <- don$`B.2 Cochez les espaces auxquels vous vous sentez appartenir, même en dehors des frontières officielles.  /Baol`
don$app_boundou <- don$`B.2 Cochez les espaces auxquels vous vous sentez appartenir, même en dehors des frontières officielles.  /Boundou`
don$app_sinesaloun <- don$`B.2 Cochez les espaces auxquels vous vous sentez appartenir, même en dehors des frontières officielles.  /Sine Saloum`
don$app_gabu <- don$`B.2 Cochez les espaces auxquels vous vous sentez appartenir, même en dehors des frontières officielles.  /Gabu`
don$app_casamance <- don$`B.2 Cochez les espaces auxquels vous vous sentez appartenir, même en dehors des frontières officielles.  /Casamance historiques`
don$app_autre <- don$`B.2.2Autres espaces d'appartenance`


don$att1 <- don$`Aimerait vivre : 1er choix`
don$att2 <- don$`Aimerait vivre : 2e choix`
don$att3 <- don$`Aimerait vivre : 3e choix`
don$rep1 <- don$`N'aimerait pas vivre : 1er choix`
don$rep2 <- don$`N'aimerait pas vivre : 2e choix`
don$rep3 <- don$`N'aimerait pas vivre : 3e choix`

don$royaume_connait <- don$`B.4  Connaissez-vous le royaume ou l'entité historique précoloniale dans lequel se situait votre lieu de niassance`
don$royaume_nom <- don$`B.4.1 Précisez son nom`
don$decoupage_royaume <- don$`B.6) Les frontières des anciens royaumes devraient-elles influencer les découpages administratifs actuels ?`

don$decoupage_avis<-don$`C.1) Le découpage en 14 régions reflète-t-il les réalités culturelles, économiques et géographiques du Sénégal ?`
don$decoupage_modif <- don$`C.2) Si non, comment souhaiteriez vous modifier ce découpage ?`
don$decoupage_dakar <-don$`C.3)   La région de Dakar vous semble-t-elle trop dominante par rapport aux autres régions du Sénégal ?`

don$reg01<-don$`C.4.1 : Nom de la région 1`
don$reg02<-don$`C.4.2 : Nom de la région 2`
don$reg03<-don$`C.4.3 : Nom de la région 3`
don$reg04<-don$`C.4.4 : Nom de la région 4`
don$reg05<-don$`C.4.5 : Nom de la région 5`
don$reg06<-don$`C.4.6 : Nom de la région 6`
don$reg07<-don$`C.4.7 : Nom de la région 7`
don$reg08<-don$`C.4.8 : Nom de la région 8`
don$reg09<-don$`C.4.9 : Nom de la région 9`
don$reg10<-don$`C.4.10 : Nom de la région 10`
x <-don %>% select(reg01,reg02, reg03, reg04, reg05, reg06, reg07, reg08, reg09, reg10)
mat<-as.matrix(x)
mat2 <- is.na(mat)==F
don$nbreg<- apply(mat2, 1, sum)


don$reg_officielle<-don$`C.5  Quels sont les critères que vous avez utilisé pour ce découpage/Découpages administratifs actuels`
don$reg_langue<- don$`C.5  Quels sont les critères que vous avez utilisé pour ce découpage/Langue / ethnie`
don$reg_religion <- don$`C.5  Quels sont les critères que vous avez utilisé pour ce découpage/Religion / confrérie`
don$reg_geophys <- don$`C.5  Quels sont les critères que vous avez utilisé pour ce découpage/Géographie physique (fleuves, forêts, relief, climat, ...)`
don$reg_histoire <- don$`C.5  Quels sont les critères que vous avez utilisé pour ce découpage/Hisoire / royaimes précoloniaux`
don$reg_economie <- don$`C.5  Quels sont les critères que vous avez utilisé pour ce découpage/Economie / bassins d'emploi`
don$reg_famille <- don$`C.5  Quels sont les critères que vous avez utilisé pour ce découpage/Liens familiaux / réseaux de migration`
don$reg_perso <- don$`C.5  Quels sont les critères que vous avez utilisé pour ce découpage/Ressenti personnel / vécu`
don$reg_autre <- don$`C.5  Quels sont les critères que vous avez utilisé pour ce découpage/Autre`
don$reg_autre_precisez <- don$`C.6 Précisez`
don$reg_commentaire <- don$`C7 Commentaire libre sur le découpage proposé`



don$mots_baol <- paste(xml$group_va79n77_bao_1_mot1, xml$group_va79n77_bao_1_mot2, xml$group_va79n77_bao_1_mot3, sep=", ")
don$mots_baol <- gsub("NA","", don$mots_baol)

don$mots_casamance <- paste(xml$group_va79n77_cas_1_mot1, xml$group_va79n77_cas_1_mot2, xml$group_va79n77_cas_1_mot3, sep=", ")
don$mots_casamance <- gsub("NA","", don$mots_casamance)

don$mots_cayol <- paste(xml$group_va79n77_cay_1_mot1, xml$group_va79n77_cay_1_mot2, xml$group_va79n77_cay_1_mot3, sep=", ")
don$mots_cayol <- gsub("NA","", don$mots_cayol)

don$mots_foutatoro <- paste(xml$group_va79n77_fou_1_mot1, xml$group_va79n77_fou_1_mot2, xml$group_va79n77_fou_1_mot3, sep=", ")
don$mots_foutatoro <- gsub("NA","", don$mots_foutatoro)


don$mots_gabuboundou <- paste(xml$group_va79n77_gab_1_mot1, xml$group_va79n77_gab_1_mot2, xml$group_va79n77_gab_1_mot3, sep=", ")
don$mots_gabuboundou <- gsub("NA","", don$mots_gabuboundou)

don$mots_sinesaloun <- paste(xml$group_va79n77_sin_1_mot1, xml$group_va79n77_sin_1_mot2, xml$group_va79n77_sin_1_mot3, sep=", ")
don$mots_sinesaloun <- gsub("NA","", don$mots_sinesaloun)

## (A.3) Préparation du tableau de jointure
tab <- don %>% select(code, numerisation, lieu, date, niveau, sexe, age, 
                      nais_pays,nais_dept, nais_mere, nais_pere,
                      mobil, mobil_nat, mobil_int,
                      ech1, ech2, ech3, ech_autre,
                      app_cayor, app_baol, app_boundou, app_gabu, app_sinesaloun, app_casamance, app_autre,
           
                                att1,att2,att3, rep1,rep2,rep3,
                      royaume_connait,
                      royaume_nom,
                      decoupage_royaume,
                      decoupage_avis,
                      decoupage_dakar,
                      nbreg,
                      reg01,reg02, reg03,reg04, reg05,reg06,reg07,reg08, reg09, reg10,

                      reg_perso, reg_famille, reg_economie, reg_histoire, reg_geophys, 
                      reg_religion, reg_langue, reg_officielle, reg_autre, reg_autre_precisez, reg_commentaire,
                      
                      mots_baol, mots_casamance, mots_cayol, mots_foutatoro, mots_gabuboundou, mots_sinesaloun,
                      )

saveRDS(tab, "data/survey_regions_senegal_VF.RDS")
write_xlsx(tab, "data/survey_regions_senegal_VF.xlsx")
