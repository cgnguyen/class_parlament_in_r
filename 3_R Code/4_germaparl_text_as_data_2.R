####Setup####
  ####*Neue Packete - nur einmal ####
  install.packages("polmineR")
  install.packages("cwbtools")
  install.packages("GermaParl")
  install.packages("xts")
  install.packages("stopwords")
  install.packages("stringr")
  install.packages("patchwork")
  install.packages("lubridate")
  

  ####*Lade Bibliotheken####
  library(cwbtools)
  library(polmineR)
  library(GermaParl)
  library(data.table)
  library(xts)
  library(tidyverse)
  library(stopwords)
  library(stringr)
  library(patchwork) #For easier multiple graphs
  library(lubridate) #Für Datum 
  library(scales)  # Disable scientific notation
  
  ####* Download voller Corpus für GermaParl - nur einmal notwendig####
  #Wichtig - Ablage der Daten
    germaparl_download_corpus(
    registry_dir = system.file(package = "GermaParl", "extdata", "cwb", "registry"),
    corpus_dir = system.file(package = "GermaParl", "extdata", "cwb", "indexed_corpora")
    )
  
  
  Sys.getenv("CORPUS_REGISTRY") # as a check, presumably not available yet
  Sys.setenv(CORPUS_REGISTRY = "C:/Users/cgngu/cwb/registry") # insert location of your registry!
  


  #Delete Corpus
  corpus()
  library(RcppCWB)
  cl_delete_corpus("GERMAPARL", registry = "C:/Users/cgngu/AppData/Local/Temp/RtmpAX5YYB/polmineR_registry")



  ####*Corpus Aktiveren#####
  use("GermaParl")
  
  library(RcppCWB)
  cl_delete_corpus("GERMAPARL", registry = "C:/Users/cgngu/AppData/Local/Temp/RtmpAX5YYB/polmineR_registry")

    
  size("GERMAPARL")
  
  corpus()
  
s=####Die Struktur der Daten####
  #Attribute 1
  p_attributes("GERMAPARL")
  
  p_attributes("GERMAPARL", p_attribute ="pos")
  
  #Meta-Attribute 
  s_attributes("GERMAPARL")
  
  s_attributes("GERMAPARL", s_attribute = "year")
  
  s_attributes("GERMAPARL", s_attribute = "party")
  
  s_attributes("GERMAPARL", s_attribute = "date")
  
  

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
    bt_all<-partition_bundle("GERMAPARL", s_attribute  = "parliamentary_group")
    bt_all_year<-partition_bundle(bt_all, s_attribute  = "year")
    
      
  ####*Keywords + Partition####
  merkel_alle <- partition("GERMAPARL", speaker = "Angela Merkel")
  
  text_temp<-kwic(merkel_alle, query = "Pandemie",  verbose = FALSE)
  read(text_temp, i=1)  
     
  text_temp<-kwic(merkel_alle, query = '"wir" "schaffen"',  verbose = FALSE)
  read(text_temp, i=1)  
  
    
####CQP Syntax/ Regular Expressions etc.####
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
    mutate(party = dplyr::recode(party,
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
      scale_colour_manual(values = c("Black","Blue","Yellow","Dark Green","Purple","Red"))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
    
####*Spezifische Worte Zählen und Darstellen####
  #Einfaches Zählen
  polmineR::count("GERMAPARL", query = c("Migrationshintergrund"))
  
  "GERMAPARL"%>%
    polmineR::count(query = c("Migrationshintergrund"))
  
  #Mehre Worte + 
  word_vec <- c(
    "Migrationshintergrund","Ausl?nder","Migration","Integration")
    
  count_germaparl <- polmineR::count("GERMAPARL", query = word_vec)
  

  #Graphische Darstellung
  count_germaparl %>%
    mutate(freq=freq*100)%>%
    ggplot()+
      aes(x=query, y=freq)+
      geom_col()+
      scale_y_continuous(labels=comma)+
      labs(x="Wort",y="Frequenz in Prozent")+
      theme_minimal()
  
  
  #Zählen mit Attributen 
  temp<-dispersion("GERMAPARL", "Migrationshintergrund", s_attribute = "party", freq=T)

  temp %>% show()
  
  #Graphische Darstellung
  temp %>% 
     mutate(party = dplyr::recode(party,
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
      labs(x="Partei",y="Frequenz", title="Wer spricht ?ber Migrationshintergrund?")
      

  temp<-dispersion("GERMAPARL", "Migrationshintergrund", s_attribute = c("year","party"), freq=T)
  
  temp %>% show() 
  
  temp %>%
    select(year,CDU,CSU,FDP,GRUENE,LINKE,PDS,SPD)%>%
    pivot_longer(-year)%>%
    mutate(party = dplyr::recode(name,
        "PDS" = "PDS/LINKE",
        "LINKE"= "PDS/LINKE"))%>%
    group_by(party,year)%>%
    summarize(freq=sum(value))%>%
    print(n=30)
  
  temp %>%
    select(year,CDU,CSU,FDP,GRUENE,LINKE,PDS,SPD)%>%
    pivot_longer(-year)%>%
    mutate(party = dplyr::recode(name,
        "PDS" = "PDS/LINKE",
        "LINKE"= "PDS/LINKE"))%>%
    group_by(party,year)%>%
    summarize(freq=sum(value))%>%
    ggplot()+
      aes(x=year,y=freq,group=party,color=party)+
      geom_line(size=1)+
      theme_minimal()+
      labs(x="Jahr",y="Frequenz der Wortnutzung", title="Wer spricht ?ber Migrationshintergrund?")+
      scale_colour_manual(values = c("Black","Blue","Yellow","Dark Green","Purple","Red"))+
      scale_y_continuous(labels=comma)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  #Regular Expressions + graphische Darstellung 
  dispersion("GERMAPARL", 
                   query = '"Migration.*"', 
                   cqp = TRUE, 
                   s_attribute = c("year","party"), freq=T)%>%
    select(year,CDU,CSU,FDP,GRUENE,LINKE,PDS,SPD)%>%
    pivot_longer(-year)%>%
    mutate(party = dplyr::recode(name,
        "PDS" = "PDS/LINKE",
        "LINKE"= "PDS/LINKE"))%>%
    group_by(party,year)%>%
    summarize(freq=sum(value))%>%
    ggplot()+
      aes(x=year,y=freq,group=party,color=party)+
      geom_line(size=1)+
      theme_minimal()+
      labs(x="Jahr",y="Frequenz der Wortnutzung", title="Wer spricht ?ber Migration?")+
      scale_colour_manual(values = c("Black","Blue","Yellow","Dark Green","Purple","Red"))+
      scale_y_continuous(labels=comma)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  

####Fortgeschrittene Analysen####
  ####*Co-Occurance####

  co_mig<-cooccurrences("GERMAPARL", query = 'Migrationshintergrund', left = 10, right = 10)
  show(co_mig)
  
  #Filter 
  co_mig %>% 
    subset(ll >= 11.83) %>%
    subset(count_coi >= 5) %>% 
    subset(!tolower(word) %in% tm::stopwords("de")) %>%
    subset(!word %in% c("''", ",", "``"))

  #Visualsierungen 
   co_mig %>% 
    subset(ll >= 11.83) %>%
    subset(count_coi >= 5) %>% 
    subset(!tolower(word) %in% tm::stopwords("de")) %>%
    subset(!word %in% c("''", ",", "``"))%>%
    polmineR::dotplot()
   
   #Visualierung - ggplot Version 
    co_mig_df<-
      co_mig@stat %>%
      subset(ll >= 11.83) %>%
      subset(count_coi >= 5) %>% 
      subset(!tolower(word) %in% tm::stopwords("de")) %>%
      subset(!word %in% c("''", ",", "``"))%>%
      slice_max(n=20, order_by=ll)%>%
      ggplot()+
        aes(x=reorder(word,ll), y=ll)+
        geom_point()+
        coord_flip()+
        theme_bw()+
        labs(x="Wort","LL")
    co_mig_df

   #Vergleich/Unterschiedliche Fraktionen  
   cdu_mig <- partition("GERMAPARL", parliamentary_group = "CDU/CSU") %>%
      cooccurrences(query = 'Migrationshintergrund', left = 10, right = 10)
    
   green_mig <- partition("GERMAPARL", parliamentary_group = "GRUENE") %>%
      cooccurrences(query = 'Migrationshintergrund', left = 10, right = 10) 
     
   
  fig_cdu<-
      cdu_mig@stat %>% 
        subset(ll >= 11.83) %>%
        subset(count_coi >= 5) %>% 
        subset(!tolower(word) %in% tm::stopwords("de")) %>%
        subset(!word %in% c("''", ",", "``"))%>%
        slice_max(n=20, order_by=ll)%>%
        ggplot()+
          aes(x=reorder(word,ll), y=ll)+
          geom_point()+
          coord_flip()+
          theme_bw()+
          labs(x="Wort","LL", title="CDU")

    fig_green<-
      green_mig@stat %>% 
        subset(ll >= 11.83) %>%
        subset(count_coi >= 5) %>% 
        subset(!tolower(word) %in% tm::stopwords("de")) %>%
        subset(!word %in% c("''", ",", "``"))%>%
        slice_max(n=20, order_by=ll)%>%
        ggplot()+
          aes(x=reorder(word,ll), y=ll)+
          geom_point()+
          coord_flip()+
          theme_bw()+
          labs(x="Wort","LL", title="Gr?ne")

    #Beide Abbildungen - braucht patchwork library
    fig_cdu+fig_green
    

####*Sentiment Analyse####
  #Sentiment Dictionary der Uni Leizpig 
  gist_url <- "https://gist.githubusercontent.com/PolMine/70eeb095328070c18bd00ee087272adf/raw/c2eee2f48b11e6d893c19089b444f25b452d2adb/sentiws.R"
  devtools::source_url(gist_url) # danach ist Funktion verfügbar
  SentiWS <- get_sentiws()  #Funktion für das Herunterladen des Dictionary
  
  
  #Wie sieht das aus? 
  head(SentiWS, 10)
  
  #Fix Encoding
  Encoding(SentiWS$word)<-"UTF-8"
  Encoding(SentiWS$lemma)<-"UTF-8"
  
  head(SentiWS, 10)
  
  #Positive/Negative Worte
  SentiWS%>%
    arrange(-weight)%>%
    print(10)
  
  
  #Wie wird über Migrationshintergrund gesprochen? - nicht viel vor 2001 
  bt2001 <- "GERMAPARL" %>% 
     partition(year = 2001:2016)

  sentiment_df <- context("GERMAPARL", query = "Migration", p_attribute = c("word", "pos"), verbose = FALSE) %>%
    partition_bundle(node = FALSE) %>%
    magrittr::set_names(s_attributes(., s_attribute = "date")) %>%
    polmineR::weigh(with = SentiWS) %>% summary()
  
  #Nach Jahr aggregieren 
  sentiment_year<-
    sentiment_df %>% 
    mutate(date = as_date(name),
           year = year(date))%>%
    group_by(year)%>%
    summarize(negative_n=sum(negative_n),
              positive_n=sum(positive_n),
              size= sum(size))%>%
    mutate(negative_share=negative_n/size,
           positive_share=positive_n/size)
  
  
  
  #Visualiserung 1 
  sentiment_year%>%
    select(year,positive_share,negative_share)%>%
    mutate(negative_share=-1*negative_share)%>%
    pivot_longer(col=-year)%>%
    ggplot()+
    aes(x=year,y=value, group=name)+
    geom_line()+
    theme_bw()+
    ylim(-0.075,0.075)
  
  
  #Visualsierung 2
  sentiment_year%>%
    select(year,positive_share,negative_share)%>%
    mutate(sentiment=positive_share-negative_share)%>%
      ggplot()+
    aes(x=year,y=sentiment)+
    geom_line()+
    theme_bw()+
    ylim(-0.075,0.075)+
    geom_hline(yintercept=0, linetype="dashed")+
    labs(x="Jahr",y="Sentiment",title="Sentiment zur Migration")
  
  
  ####*Sentiment nach Partei
  #CDU
   sentiment_cdu <- 
      partition("GERMAPARL", parliamentary_group = "CDU/CSU") %>%
      polmineR::context(., query = "Migration", p_attribute = c("word", "pos"))%>%
      partition_bundle(node = F)%>%
      magrittr::set_names(s_attributes(., s_attribute = "date")) %>%
      polmineR::weigh(with = SentiWS) %>%
      summary()%>%
      mutate(date = as_date(name),
             year = year(date))%>%
      group_by(year)%>%
      summarize(negative_n=sum(negative_n),
                positive_n=sum(positive_n),
                size= sum(size))%>%
      filter(!is.na(year))%>%
      mutate(negative_share=negative_n/size,
      positive_share=positive_n/size) %>% 
      select(year,positive_share,negative_share)%>%
      mutate(sentiment=positive_share-negative_share)
   
   
   
  #Grüne
   sentiment_green <- 
      partition("GERMAPARL", parliamentary_group = "GRUENE") %>%
      polmineR::context(., query = "Migration", p_attribute = c("word", "pos"))%>%
      partition_bundle(node = F)%>%
      magrittr::set_names(s_attributes(., s_attribute = "date")) %>%
      polmineR::weigh(with = SentiWS) %>%
      summary()%>%
      mutate(date = as_date(name),
             year = year(date))%>%
      group_by(year)%>%
      summarize(negative_n=sum(negative_n),
                positive_n=sum(positive_n),
                size= sum(size))%>%
      filter(!is.na(year))%>%
      mutate(negative_share=negative_n/size,
      positive_share=positive_n/size) %>% 
      select(year,positive_share,negative_share)%>%
      mutate(sentiment=positive_share-negative_share)
      
   
      #Daten Kombineren 
      sentiment_total<-
        inner_join(sentiment_cdu,sentiment_green, by="year")%>%
        select(year,sentiment.x,sentiment.y)%>%
        pivot_longer(col=-year)%>%
        mutate(name= dplyr::recode(name,
          "sentiment.x"= "CDU/CSU",
          "sentiment.y"= "Grüne"))%>%
      ggplot()+
          aes(x=year,y=value, group=name, color=name)+
          geom_line()+
          theme_bw()+
          geom_hline(yintercept=0, linetype="dashed")+
          labs(x="Jahr",y="Sentiment",title="Sentiment zur Migration")+
          scale_color_manual(values=c("Black","Dark Green"))
  
      sentiment_total
      
