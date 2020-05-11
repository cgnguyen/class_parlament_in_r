####Setup#### 

  #Neue Packete Installieren 
  install.packages("tidyverse")

  #Packete Laden
  library(tidyverse)

  #Check Working Directory
  getwd()
  
####Daten Einlesen und anschauen####
  
  data_fed<-read_csv("fd-cross.csv")
  
  data_fed
  
  View(data_fed)
  
  #Aus dem Internet einlesen - von meiner github Seite 
  data_fed_internet<-read_csv("https://raw.githubusercontent.com/cgnguyen/parlament_in_r/master/fd-cross.csv")
  
####Daten Säubern 1 - Numerisch zu Faktor ####
  
  summary(data_fed$feddummy)
  
  data_fed<-
    data_fed %>% 
    mutate(feddummy = factor(recode(feddummy,
                                    `0`="Nein",
                                    `1`="Ja")))

  summary(data_fed$feddummy)
  

####Deskritpive Daten 1: BIP pro Kopf####
  data_fed$gdpppp94
  
  summary(data_fed$gdpppp94)
  
  mean(data_fed$gdpppp94)
  
  mean(data_fed$gdpppp94, na.rm=T)
  
####Deskriptive Daten 2: Gruppen und Gruppenunterschiede####
  
  data_fed %>% 
    group_by(feddummy) %>%
    summarize(bip_kopf= mean(gdpppp94))
  
  data_fed %>% 
    group_by(feddummy) %>%
    summarize(bip_kopf= mean(gdpppp94, na.rm=T))
  

  data_fed %>% 
    group_by(feddummy) %>%
    summarize(bip_kopf= median(gdpppp94, na.rm=T))
  
  data_fed %>%
    group_by(feddummy) %>%
    summarize(bip_kopf= median(gdpppp94, na.rm=T),
              count= n()) 
  
  ####Deskriptive Daten 3: Mehr als eine Variable (BIP Pro Kopf und BIP wachstum)####
  
  #Summary statistics 
  summary(data_fed[,c("gdpppp94","growth10")]) 
  
  data_fed %>%
    group_by(feddummy)%>%
    summarize(bip_kopf= mean(gdpppp94, na.rm=T),
              bip_wachs = mean(growth10, na.rm=T),
              count= n()) 
  
  data_fed %>%
    group_by(feddummy)%>%
    summarize(bip_kopf= mean(gdpppp94, na.rm=T),
              bip_kopf_mittel =  median(gdpppp94, na.rm=T),
              count= n()) 
  
  
####Data Säubern 2: Numerische Variablen zu Kategorische Variablen####
  summary(data_fed$dezrev1)
  
  
  data_fed<-
    data_fed %>% 
    mutate(dezentralsierung = factor(case_when(dezrev1 < 0.06 ~ "Niedrig",
                                               dezrev1 >= 0.06 & dezrev1 < 0.24 ~ "Mittel",
                                               dezrev1 >= 0.24 ~"Hoch")))

  summary(data_fed$dezentralsierung)    
  
####Deskritpive Daten 4:  Mehr als eine Gruppenvariable ####
  
  data_fed %>% 
    group_by(feddummy,dezentralsierung) %>%
    summarize(bip_kopf= mean(gdpppp94, na.rm=T),
              bip_wachstum = mean(growth10, na.rm=T))
  
  ####Daten Säubern 3: Datensatz filtern, anordnen etc.#### 
  
  
  data_fed %>% 
    group_by(feddummy,dezentralsierung) %>%
    summarize(bip_kopf= mean(gdpppp94, na.rm=T),
              bip_wachstum = mean(growth10, na.rm=T))%>%
    filter(!is.na(dezentralsierung))
  
  data_fed %>% 
    group_by(feddummy,dezentralsierung) %>%
    summarize(bip_kopf= mean(gdpppp94, na.rm=T),
              bip_wachstum = mean(growth10, na.rm=T))%>%
    filter(!is.na(dezentralsierung))%>%
    select(feddummy,dezentralsierung,bip_kopf)
  
  
  
####Export der Ergebnisse####
  
  table_export<-
    data_fed %>% 
    group_by(feddummy,dezentralsierung) %>%
    summarize(bip_kopf= mean(gdpppp94, na.rm=T),
              bip_wachstum = mean(growth10, na.rm=T))%>%
    filter(!is.na(dezentralsierung))%>%
    select(feddummy,dezentralsierung,bip_kopf)

  table_export
  
  #Export als CSV 
  write_csv(table_export,path="bip_kopf.csv")
  
  #Export als csv(deutsch)
  write_csv2(table_export,path="bip_kopf_de.csv")
   
  
