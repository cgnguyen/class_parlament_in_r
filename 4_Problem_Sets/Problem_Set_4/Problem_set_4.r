####Problem Set 4 Antworten####

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
  library(lubridate) #FÃ¼r Datum 
  library(scales)  # Disable scientific notation
  library(car)
  library(ggthemes)
  
  
  ####*Corpus Aktiveren#####
  use("GermaParl")
    

####*Spezifische Worte ZÃ¤hlen und Darstellen####

  #ZÃ¤hlen mit Attributen 
  temp<-dispersion("GERMAPARL", "Föderalismus", s_attribute = "party", freq=T)

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
    summarize(count=sum(count),
              size=sum(size),
      freq_1=sum(freq),
      freq_2=count/size)%>%
    ggplot()+
      aes(x=party, y=freq_2, fill=party)+
      geom_col()+
      scale_fill_manual(values = c("Black", "Blue","Yellow","Dark Green","Purple","Red"))+
      labs(x="Partei",y="Frequenz in %", title="Wer spricht Über Föderalismus?")+
      theme_solarized()
      

  temp<-dispersion("GERMAPARL", "Föderalismus", s_attribute = c("year","party"), freq=T)
  
  temp %>% show() 
  
  
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
      scale_colour_manual(values = c("Black","Blue","Yellow","Dark Green","Purple","Red"))+
      scale_y_continuous(labels=comma)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
  
  
  #Regular Expressions + graphische Darstellung 
  dispersion("GERMAPARL", 
                   query = '"Föderalis.*"', 
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
      labs(x="Jahr",y="Frequenz der Wortnutzung", title="Wer spricht über Föderalismus?")+
      scale_colour_manual(values = c("Black","Blue","Yellow","Dark Green","Purple","Red"))+
      scale_y_continuous(labels=comma)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  

####Fortgeschrittene Analysen####
  ####*Co-Occurance####
  ####*

  co_fed<-cooccurrences("GERMAPARL", query = 'Föderalismusreform', left = 10, right = 10)
  show(co_fed)
  
  #Filter 
  co_fed %>% 
    subset(ll >= 11.83) %>%
    subset(count_coi >= 5) %>% 
    subset(!tolower(word) %in% tm::stopwords("de")) %>%
    subset(!word %in% c("''", ",", "``"))


   #Visualierung - ggplot Version 
    co_fed_df<-
      co_fed@stat %>%
      subset(ll >= 11.83) %>%
      subset(count_coi >= 5) %>% 
      subset(!tolower(word) %in% tm::stopwords("de")) %>%
      subset(!word %in% c("''", ",", "``"))%>%
      slice_max(n=30, order_by=ll)%>%
      ggplot()+
        aes(x=reorder(word,ll), y=ll)+
        geom_point()+
        coord_flip()+
        theme_bw()+
        labs(x="Wort","LL")
    co_fed_df

   #Vergleich/Unterschiedliche Fraktionen  
   cdu_fed <- partition("GERMAPARL", parliamentary_group = "CDU/CSU") %>%
      cooccurrences(query = 'Föderalismus', left = 10, right = 10)
    
   spd_fdp <- partition("GERMAPARL", parliamentary_group = "SPD") %>%
      cooccurrences(query = 'Föderalismus', left = 10, right = 10) 
     
   
  fig_cdu<-
      cdu_fed@stat %>% 
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

  fig_cdu
  
    fig_spd<-
      spd_fdp@stat %>% 
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
          labs(x="Wort","LL", title="SPD")

    #Beide Abbildungen - braucht patchwork library
    fig_cdu+fig_spd
    

####*Sentiment Analyse####
  #Sentiment Dictionary der Uni Leizpig 
  gist_url <- "https://gist.githubusercontent.com/PolMine/70eeb095328070c18bd00ee087272adf/raw/c2eee2f48b11e6d893c19089b444f25b452d2adb/sentiws.R"
  devtools::source_url(gist_url) # danach ist Funktion verfÃ¼gbar
  SentiWS <- get_sentiws()  #Funktion fÃ¼r das Herunterladen des Dictionary
  
  
  #Wie sieht das aus? 
  head(SentiWS, 10)
  
  #Fix Encoding
  Encoding(SentiWS$word)<-"UTF-8"
  Encoding(SentiWS$lemma)<-"UTF-8"
  
  head(SentiWS, 10)
  
  #Positive/Negative Worte
  SentiWS%>%
    arrange(weight)%>%
    print(10)
  
  
  #Wie wird Föderalismus gesprochen? 

  sentiment_df <- context("GERMAPARL", query = "Föderalismus", 
                          p_attribute = c("word", "pos"), verbose = FALSE) %>%
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
    labs(x="Jahr",y="Sentiment",title="Sentiment zum Föderalismus")
  
  
  ####*Sentiment nach Partei
  #CDU
   sentiment_cdu <- 
      partition("GERMAPARL", parliamentary_group = "CDU/CSU") %>%
      polmineR::context(., query = "Föderalismus", p_attribute = c("word"))%>%
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
   
   
   
  #FDP
   sentiment_FDP <- 
      partition("GERMAPARL", parliamentary_group = "FDP") %>%
      polmineR::context(., query = "Föderalismus", p_attribute = c("word", "pos"))%>%
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
        inner_join(sentiment_cdu,sentiment_FDP, by="year")%>%
        select(year,sentiment.x,sentiment.y)%>%
        pivot_longer(col=-year)%>%
        mutate(name= dplyr::recode(name,
          "sentiment.x"= "CDU/CSU",
          "sentiment.y"= "FDP"))%>%
      ggplot()+
          aes(x=year,y=value, group=name, color=name)+
          geom_line()+
          theme_bw()+
          geom_hline(yintercept=0, linetype="dashed")+
          labs(x="Jahr",y="Sentiment",title="Sentiment zum Föderalismus")+
          scale_color_manual(values=c("Black","Yellow"))
  
      sentiment_total
      
