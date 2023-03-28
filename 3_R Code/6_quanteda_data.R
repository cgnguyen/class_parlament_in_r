#Setup------------------
  library(quanteda)
  library(tidyverse)


#Getting Data using API - Example from the CMP--------------------
  
  library(seededlda) 
  library(manifestoR)
  library(car)


##Daten des Manifesto Projekts Herunterladen ------------------------
  #Ihr braucht ein API
  mp_setapikey("manifesto_apikey.txt")
  
  #Sind wir verbunden? - Ja
   mp_availability(TRUE)
  
  #Welche Dokumente gibt es aus Deutschland? 
  available_docs <- mp_availability(countryname == "Germany")
  
    available_docs <- mp_availability(countryname == "Germany")


  
  available_docs
  
  #Parteien (hier als Codes - siehe Manifesto Webseite)
  available_docs$party
  
  #Zeitraum 
  available_docs$date

  #Spezifischer Zeitraum  - 19 Parteiprogramme
  available_ger <- mp_availability(
    countryname == "Germany" & 
      date > 201701 )
  
  
  #Daten Herunterladen 
  tm_corpus <- mp_corpus(available_ger)
  
  #Für Quanteda vorbereiten 

    
    tm_corpus %>%
    as.data.frame(with.meta = TRUE)%>%
    group_by(manifesto_id)%>%
    summarize(text=paste(text, collapse = " "))
  
  
  
  %>%
    corpus(docid_field = "manifesto_id", unique_docnames = FALSE) ## quanteda's corpus function

 
  quanteda_corpus
  
  
  
  
  
  
  
  
  
  
#Daten Manipulieren und reinigen -----------------------------------------------
  ##Text Daten reinigen  -----------------------------
  quanteda_corpus%>%
    tokens(remove_punct = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(quanteda::stopwords("german"))%>%
    tokens_select(min_nchar=3L)%>%
    head(5)
    


  
  unique(quanteda_corpus$party_name)
  
  #Informationen im Datensatz verändern - Datum 
  quanteda_corpus$date
  
  quanteda_corpus$datum<-ym(quanteda_corpus$date)
  quanteda_corpus$year<-year(quanteda_corpus$datum)
  
  #Lesbarer ID Code 
  quanteda_corpus$id_neu<-
    paste(quanteda_corpus$party_name,quanteda_corpus$year)

  
  ##Eigene DocVars  Hinzufügen -----------------------------------------------------
  #z.B. Politische Positionen aus dem Chapel Hill Expert Survey 2019
  D_ches<-read.csv("CHES2019V3.csv") 
  
  D_ches<-
    D_ches%>%
      filter(country==3)%>%
      select(party,lrecon,galtan)
  
  #Parteiprogramme sind für CDU/CSU - > wir nehmen den wert der CDU und setzten das Jahr auf 2017
  D_ches<-
    D_ches  %>%
    filter(party !="CSU")%>%
    filter(party!= "DieTier")%>%
    filter(party!= "DieTier")%>%
    mutate(party_name=car::recode(party, "'CDU'='CDU/CSU'"))%>%
    select(!party)%>%
    mutate(year=2017)
  
  
  
  #Mit dem Corpus verbinden 
  
    #Alte Docvars nehmen
    docvar_ger<- docvars(quanteda_corpus)
    
    #CHES Hinzufügen
    docvar_ger<-
      docvar_ger%>%
        left_join(D_ches, by=c("party_name","year"))
    
    #Wieder in den Corpus packen
    docvars(quanteda_corpus) <- docvar_ger
    