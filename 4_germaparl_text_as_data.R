####Setup####
  ####*Installing Germaparl- currently not on Cran####
  install.packages("drat")
  drat::addRepo("polmine")
  install.packages("polmineR")
  install.packages("cwbtools")
  
  devtools::install_github("PolMine/GermaParl", ref = "dev")
  
  
  ####*Lade Bibliotheken####
  library(polmineR)
  library(GermaParl)
  library(tidyverse)  

  library(scales)  # Disable scientific notation

  
  
  
  ####* Download voller Corpu für GermaParl####
  germaparl_download_corpus()
  
  use("GermaParl")
    
  
####Die Struktur der Daten####
  #Attribute 1
  p_attributes("GERMAPARL")
  
  p_attributes("GERMAPARL", p_attribute ="pos")
  
  #Meta-Attribute 
  s_attributes("GERMAPARL")
  
  s_attributes("GERMAPARL", s_attribute = "year")
  
  s_attributes("GERMAPARL", s_attribute = "party")
  
  s_attributes("GERMAPARL", s_attribute = "date")
  
  

####Partionen / Zoomen####
  	#Alle Ansprachen von Angela Merkel 
    merkel_alle <- partition("GERMAPARL", speaker = "Angela Merkel")
    
    #Bundestag 2008
    bt2008<-  partition("GERMAPARL" , year = 2008) 
    
    #Bundestag 2008 ohne Zwischenrufe
    bt2008<-  "GERMAPARL" %>%
      partition(year = 2008) %>%
      partition(interjection = FALSE) 
    
    #Bundestag 2008 ohne Zwischenrufe + Nur SprecherInnen der Regierung 
    bt2008min <- "GERMAPARL" %>% 
        partition(year = 2008) %>%
        partition(interjection = FALSE) %>%
        partition(role = c("mp", "government"))
    
    #Partition Bundles
    bt_all<-partition_bundle("GERMAPARL", s_attribute  = "party")
    bt_all_year<-partition_bundle(bt_all, s_attribute  = "year")
  
####Anfang - Text suchen/ Lesen/Keywords in Context####
    ####*Datum und Sprecher####
  	merkel <- partition("GERMAPARL", speaker = "Angela Merkel", date = "2013-09-03")
    read(merkel)
    
    
    ####*Keywords in Context####
    kwic("GERMAPARL", query = "Migrationshintergrund")
    
      #Mehr Worte
      kwic("GERMAPARL", query = "Migrationshintergrund", left = 15, right = 15)
      
      #Mit Kontext
      kwic("GERMAPARL", query = "Migrationshintergrund", s_attributes = c("party", "date"), verbose = FALSE)
      
      #Zum Volltext
      text_temp<-kwic("GERMAPARL", query = "Migrationshintergrund",  verbose = FALSE)
      
      read(text_temp, i=1)  
      
    ####*Keywords + Partition####
    text_temp<-kwic(merkel_alle, query = "Pandemie",  verbose = FALSE)
     
    text_temp<-kwic(merkel_alle, query = '"wir" "schaffen"',  verbose = FALSE)
    
    ####*CQP Syntax/ Regular Expressions etc.####
    kwic(merkel_alle, query = '[pos = "NN"] "mit" "Migrationshintergrund"', cqp = TRUE)
    
    #Regular Expressions - nur ein Beispiel 
    kwic("GERMAPARL", query = '"Multikult.*"', cqp = TRUE)
    
    
####Deskritpive Analyse####
  ####*Zahl der Redeanteile/Size####
  #Redeanteil/ Grösse 
  size("GERMAPARL")
  
  #Mit Einem S-Attribut - Legislaturperiode
  size("GERMAPARL", s_attribute = "lp")
  
  #Graphische Darstellung
  size("GERMAPARL", s_attribute = "lp")%>%
    ggplot()+
      aes(x=lp, y=size)+
      geom_col()+
      scale_y_continuous(labels=comma)+
      theme_minimal()+
      labs(x="Legislaturperiode", y="Redeanteil") 
  
  #Zwei Attribute - Fraktion und Jahr 
  tab_redeanteil<-size("GERMAPARL", s_attribute = c("year","party")) 
  
  tab_redeanteil
  #Graphische Darstellung 1 
  tab_redeanteil%>%
    ggplot()+
    aes(x=year,y=size,group=party, color=party)+
    geom_line()
  
  #Graphische Darstellung 2 - etwas schöner
  tab_redeanteil %>%
    mutate(party = dplyr::recode(temp$party,
        "PDS" = "LINKE",
        "parteilos" = NA_character_,
        "fraktionslos" = NA_character_,))%>%
    filter(!is.na(party))%>%
    filter(party!="")%>%
    group_by(year,party)%>%
    summarize(size=sum(size)) %>%
    
    ggplot()+
      aes(x=year,y=size,group=party, color=party)+
      geom_line()+
      theme_minimal()+
      labs(x="Jahr",y="Redeanteil")+
      scale_colour_manual(values = c("Black","Yellow","Dark Green","Purple","Red"))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
    
####*Spezifische Worte Zählen und Darstellen####
  #Einfaches Zählen
  count("GERMAPARL", query = c("Migrationshintergrund"))
  
  "GERMAPARL"%>%
    count(query = c("Migrationshintergrund"))
  
  #Mehre Worte + 
  queries <- c(
    "Migrationshintergrund","Ausländer","Migration","Integration")
    
  dt <- count("GERMAPARL", query = queries)
  
  dt
  
  #Graphische Darstellung
  dt %>%
    mutate(freq=freq*100)%>%
    ggplot()+
      aes(x=query, y=freq)+
      geom_col()+
      scale_y_continuous(labels=comma)+
      labs(x="Wort",y="Frequenz in Prozent")+
      theme_minimal()
  
  
  #Zählen mit Attributen 
  temp<-dispersion("GERMAPARL", "Integration", s_attribute = "party", freq=T)

  temp %>% show()
  
  
  temp %>% 
     mutate(party = dplyr::recode(temp$party,
        "PDS" = "LINKE",
        "parteilos" = NA_character_,
        "fraktionslos" = NA_character_,))%>%
    filter(!is.na(party))%>%
    filter(party!="")%>%
    group_by(party)%>%
    summarize(freq=sum(freq))%>%
    ggplot()+
      aes(x=party, y=freq, fill=party)+
      geom_col()+
      theme_minimal()+
      scale_fill_manual(values = c("Black", "Blue","Yellow","Dark Green","Purple","Red"))+
      labs(x="Partei",y="Frequenz", title="Wer spricht über Integration?")
      

  temp<-dispersion("GERMAPARL", "Integration", s_attribute = c("year","party"), freq=T)
  
  temp %>% show()
  
  
