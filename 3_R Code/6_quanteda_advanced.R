# Setup -------------------------------------------------------------------
  library(quanteda)
  library(tidyverse)
  
  quanteda_corpus<-read_rds("quanteda_corpus.rds")

  ##Daten Säubern----------------------------------------
  ##Data Cleaning: Partei-Namen ---------------
  # Codebook https://manifesto-project.wzb.eu/down/data/2020b/codebooks/codebook_MPDataset_MPDS2020b.pdf
  #Parteien: https://manifesto-project.wzb.eu/down/data/2022a/codebooks/parties_MPDataset_MPDS2022a.csv
  
  
  name_vec<-docnames(quanteda_corpus)
  
  name_vec_new<-
    name_vec%>%
    str_remove(.,"_201709")%>%
    str_replace(.,"41113","Greens")%>%
    str_replace(.,"41223","Left")%>%
    str_replace(.,"41320","SPD")%>%
    str_replace(.,"41420","FDP")%>%
    str_replace(.,"41912","SSW")%>%
    str_replace(.,"41953","Afd")%>%
    str_replace(.,"41521","CDU/CSU")
  
  docnames(quanteda_corpus)<-name_vec_new
  
  docnames(quanteda_corpus)
  
  quanteda_corpus
  
  #Informationen im Datensatz verändern - Parteinamen
  quanteda_corpus$party_name<-car::recode(quanteda_corpus$party,"
                                          41113='GRUNEN';
                                          41223= 'LINKE';
                                          41320= 'SPD';
                                          41420='FDP';
                                          41521='CDU/CSU';
                                          41953='AfD';
                                          41912='SSW'")
  
  
  
    
#Dictionaries---------------------------------------------------------
  ##Einfache Wortliste - Populismus in Wahlprogrammen -----------
  #- Inspiriert von 
  #https://www.tandfonline.com/doi/abs/10.1080/01402382.2011.616665 
  #https://github.com/jogrue/popdictR
  
  
  ###Dictionary Definieren --------------
    populism_lexicon<-dictionary(
      list(populism= c("elit*","skandal*","volk*","bürger*")))
  
  
  ###Absolute Tokenzahl (für Gewichtung)-------------
  dfm_party_tokens<-
      quanteda_corpus%>%
        tokens()%>%
        tokens_select(pattern = stopwords("de"),selection="remove")%>%
        tokens_select(pattern ="innen", selection="remove")%>%
        tokens_select(pattern ="dass", selection="remove")%>%
        tokens_select(pattern=phrase("freie demokraten"), selection="remove")%>%
        tokens(remove_punct = T)%>%
        dfm()%>%
        dfm_group(groups=party_name)%>%
        ntoken()
  
  dfm_party_tokens
  ###DFM Matrix mit populismusworten ertellen--------------------
  
  dfm_party_populism<-
      quanteda_corpus%>%
        tokens()%>%
        tokens_select(pattern = stopwords("de"),selection="remove")%>%
        tokens_select(pattern ="innen", selection="remove")%>%
        tokens_select(pattern ="dass", selection="remove")%>%
        tokens_select(pattern=phrase("freie demokraten"), selection="remove")%>%
        tokens(remove_punct = T)%>%
        tokens_lookup(dictionary=populism_lexicon)%>% 
        dfm()%>% 
        dfm_group(groups=party_name)
        
  dfm_party_populism%>%
    convert(to="data.frame")%>%
    # mutate(total=dfm_party_tokens)%>%
    # mutate(pop_percent=populism/total)%>%
    ggplot()+
      aes(x=doc_id,y=populism)+
      geom_col()
  

  
  ##Die Auswahl der Wörter ist sehr wichtig! - Bürger auch eine Anrede-------------
    populism_lexicon_neu<-dictionary(
      list(populism= c("elit*","skandal*","volk*")))
  
    dfm_party_populism_neu<-
        quanteda_corpus%>%
          tokens()%>%
          tokens_select(pattern = stopwords("de"),selection="remove")%>%
          tokens_select(pattern ="innen", selection="remove")%>%
          tokens_select(pattern ="dass", selection="remove")%>%
          tokens_select(pattern=phrase("freie demokraten"), selection="remove")%>%
          tokens(remove_punct = T)%>%
          tokens_lookup(dictionary=populism_lexicon_neu)%>% 
          dfm()%>% 
          dfm_group(groups=party_name)
          
  
    dfm_party_populism_neu%>%
      convert(to="data.frame")%>%
      ggplot()+
        aes(x=doc_id,y=populism)+
        geom_col()
    
  
  ###Vielleicht auch nach Länge gewichten-----------------------------------
  dfm_party_populism_neu%>%
    convert(to="data.frame")%>%
    mutate(total=dfm_party_tokens)%>%
    mutate(pop_percent=populism/total)%>%
    ggplot()+
      aes(x=doc_id,y=pop_percent)+
      geom_col()
  
    
    
  ##Sentiment Analyse -----------------------
  library(quanteda.sentiment) #Noch nicht auf Cran
  
  remotes::install_github("quanteda/quanteda.sentiment") #Installation 
  
  
  #Wie sieht das aus? 
  data_dictionary_sentiws 
  

  
  ###Polarity---------------------------
  
  ##DFM gewichten
  dfm_party_sentiment<-
        quanteda_corpus%>%
          tokens()%>%
          tokens_select(pattern = stopwords("de"),selection="remove")%>%
          tokens_select(pattern ="innen", selection="remove")%>%
          tokens_select(pattern ="dass", selection="remove")%>%
          tokens_select(pattern=phrase("freie demokraten"), selection="remove")%>%
          tokens(remove_punct = T)%>%
          dfm()%>%
          dfm_lookup(dictionary=data_dictionary_sentiws)%>%
          dfm_group(groups=party_name)

  
  dfm_party_sentiment%>%
    convert(to="data.frame")%>%
    mutate(sentiment= positive-negative)%>%
    ggplot()+
      aes(x=doc_id, y=sentiment)+
      geom_col()
    
  
  
  ###Valence---------------------------
  valence(data_dictionary_sentiws )
  
  ##DFM gewichten
  dfm_party_sentiment_valence<-
        quanteda_corpus%>%
          tokens()%>%
          tokens_select(pattern = stopwords("de"),selection="remove")%>%
          tokens_select(pattern ="innen", selection="remove")%>%
          tokens_select(pattern ="dass", selection="remove")%>%
          tokens_select(pattern=phrase("freie demokraten"), selection="remove")%>%
          tokens(remove_punct = T)%>%
          dfm()%>%
          dfm_group(groups=party_name)%>%
          textstat_valence(., dictionary=data_dictionary_sentiws, 
                           normalization = "all")


  
  dfm_party_sentiment_valence%>%
      ggplot()+
      aes(x=doc_id, y=sentiment)+
      geom_col()
    
  
  
#Wordscores----------------------------------------------------------
  #Predict the left-right position of 2021 based on 2017 program  
  library(quanteda.textmodels)
  library(quanteda.textplots)
  
  ##Daten Vorbereiten-----------------------

  ##Get LR position from CHES 
  D_ches<-read.csv("CHES2019V3.csv") 
  
  D_ches<-
    D_ches%>%
      filter(country==3)%>%
      select(party,lrecon,galtan)
  
  ##Parteiprogramme sind für CDU/CSU - > wir nehmen den wert der CDU und setzten das Jahr auf 2017
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
    
    docvar_ger$year<-as.numeric(substring(docvar_ger$date, first= 1, last=4))
    
    #CHES Hinzufügen
    docvar_ger<-
      docvar_ger%>%
        left_join(D_ches, by=c("party_name","year"))
    
    #Wieder in den Corpus einfügen
    docvars(quanteda_corpus) <- docvar_ger
  
  ##Aggregiere auf Jahr und Parteiebene 
  dfm_party_year<-
          quanteda_corpus%>%
          tokens()%>%
          tokens_select(pattern = stopwords("de"),selection="remove")%>%
          tokens_select(pattern ="innen", selection="remove")%>%
          tokens_select(pattern ="dass", selection="remove")%>%
          tokens_select(pattern=phrase("freie demokraten"), selection="remove")%>%
          tokens(remove_punct = T)%>%
          dfm()%>%
          dfm_group(groups= interaction(party_name,year))
    
  dfm_party_year$lrecon
  
  ##Wordscores Berechnen--------------------------
  ws_germany <- textmodel_wordscores(dfm_party_year, y = dfm_party_year$lrecon, smooth = 1)
  
  summary(ws_germany)
  
  #"Linke" Worte
  ws_germany$wordscores%>%
    broom::tidy()%>%
    arrange(x)
  
  #"Rechte" Worte
  ws_germany$wordscores%>%
    broom::tidy()%>%
    arrange(-x)
  
  ##Wordscore Prediction/ Extrapolation --------------------------
  pred_ws_germany <- predict(ws_germany, se.fit = TRUE, newdata = dfm_party_year)
  
  pred_ws_germany
  
  #Textstat Version
  textplot_scale1d(pred_ws_germany)
  
  #GGPlot Version 
  pred_ws_germany%>%
    as.data.frame()%>%
    mutate(party_name=rownames(.))%>%
    ggplot()+
      aes(x=party_name, y=fit, ymin=fit-se.fit*1.96,ymax=fit+se.fit*1.96)+
      geom_point()+
      geom_errorbar(width=0.2)+
      coord_flip()
  
#Wordfish ------------------------------
  
  
  ##Apply wordfish Model : Dir means that the value of the dimension of document 1 < document 5
  #Rechenintensiv - daher reduzierung der Matrix auf Wörter die mindestens 10 mal auftauchen
  wf_germany<-
    dfm_party_year %>%
    dfm_trim(min_termfreq=10)%>%
     textmodel_wordfish(., dir = c(5, 1))
  
  
  ##Ergebnisse Zeigen & Visualsieren ------------------
  summary(wf_germany)
  
  textplot_scale1d(wf_germany)
  
  textplot_scale1d(wf_germany, groups=dfm_party_year$party_name)
  

  # ##Diagnostik & Worte Anschauen-----------------
  # #Dauert etwas
  # fig_diagnostic<-
  #   textplot_scale1d(wf_germany, margin = "features", 
  #                highlighted = c("kriminalität","volk","elite"))
  # 
  # fig_diagnostic

#Topic Modeling------------------------------------------------------
  library(seededlda)
  library("stm")
  
  ##DFM Vorbereiten-------------------
  dfm_germ<-
      quanteda_corpus%>%
        tokens()%>%
        tokens_select(pattern = stopwords("de"),selection="remove")%>%
        tokens_select(pattern ="innen", selection="remove")%>%
        tokens_select(pattern ="dass", selection="remove")%>%
        tokens_select(pattern=phrase("freie demokraten"), selection="remove")%>%
        tokens(remove_punct = T)%>%
        dfm()%>%
        dfm_trim(min_termfreq=10)

  #Conver to STM mode
  dfm_stm <- convert(dfm_germ, to = "stm")
  
  
  
  ##Finde 5 Themen-------------------
  #Dauert etwas
  tm_germany <-stm(documents = dfm_stm$documents,
         vocab = dfm_stm$vocab, 
         K = 5,
         verbose = TRUE)
    
    
  
  
  plot(tm_germany)

  ###Worte assoziert mit den Themen---------------------
 
  labelTopics(model,topics = c(1:5), n=5)

  ###Zuweisen von Themen zu dokumenten------------------
  dfm_germ$topic <- topics(tm_germany)

  

  