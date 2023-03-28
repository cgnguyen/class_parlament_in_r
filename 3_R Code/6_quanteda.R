
# Setup -------------------------------------------------------------------
  library(quanteda)
  library(tidyverse)

  quanteda_corpus<-read_rds("quanteda_corpus.rds")

#Basics ------------------------------------------------------



  #Wie sieht eine Quanteda Dateiaus? - Jeder Satz hier eine eigenes "Dokument"  
  quanteda_corpus
  

  #Jedes Dokument hat eine ID
  docnames(quanteda_corpus)

  #Jedes Dokument hat Extra Variablen 
  docvars(quanteda_corpus)
  
  docvars(quanteda_corpus, field="party")
  unique(docvars(quanteda_corpus, field="party"))
  
  
  
#Document-Feature Matrix + Datenreinigen ------------
  
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
  
  
  #Informationen im Datensatz verändern - Parteinamen
  quanteda_corpus$party_name<-car::recode(quanteda_corpus$party,"
                                          41113='GRUNEN';
                                          41223= 'LINKE';
                                          41320= 'SPD';
                                          41420='FDP';
                                          41521='CDU/CSU';
                                          41953='AfD';
                                          41912='SSW'")
  
  #Tokens als Analyse-Einheit 
  quanteda_corpus%>%
    tokens(.)
  
  
  ##Date Cleaning
  #Keine Zeichen oder stopwörter
    quanteda_corpus%>%
      tokens(remove_punct = T)%>%
      tokens_select(pattern = stopwords("de"),selection="remove")
    
    #Nur wortstamm 
    quanteda_corpus%>%
      tokens(remove_punct = T)%>%
      tokens_select(pattern = stopwords("de"),selection="remove")%>%
      tokens_wordstem(., language = ("de"))
  
  
  ##Tidyverse Logic und Auswahl
  quanteda_corpus%>%
    corpus_subset(party == 41420) 
  
  quanteda_corpus%>%
    corpus_subset(party_name =="GRUNEN") 
  




#Kwic- Keywords in Context ---------------------------
  #Texte Lesen/keywords in Context 
  quanteda_corpus%>%
    tokens(.)%>% 
    kwic(., pattern="Migrant", window= 5)
  
  #Text Lesen/Kewyord in Context mit Wildcards
  quanteda_corpus%>%
    tokens(.)%>% 
    kwic(., pattern="Migrant*", window= 5)
  
  quanteda_corpus%>%
    tokens(.)%>% 
    kwic(., pattern="*Migrant*", window= 5)
  

  #Längere Begriffe
  quanteda_corpus%>%
    tokens(.)%>% 
    kwic(., pattern=phrase("illegale Migration"))
  
  #Etwas schöner zu Lesen
  quanteda_corpus %>%
    tokens() %>%
    kwic(phrase("Migrant"), window = 10)%>%
    DT::datatable(caption = "Keywords in context", 
                rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10, 
                                                 lengthMenu = c(5, 10, 15, 20)))

  
    quanteda_corpus%>%
      corpus_subset(party_name=="AfD")%>%
      corpus_subset(pos==475)%>%
      as.character()

  
   
#Einfache Statistiken mit Document Feature Matrix-------------
  library(quanteda.textstats) 
  
  corpus_dfm<-
    quanteda_corpus%>%
      tokens(remove_punct = T)%>%
      tokens_select(pattern = stopwords("de"),selection="remove")%>%

      dfm()
    
  corpus_dfm
  
  #Gruppieren nach Features 
    corpus_dfm%>%
      dfm_group(party_name)
  
#Word Frequencies ---------------
  
  corpus_dfm%>%
    textstat_frequency(n=10)
    
  #Nach Partei  
  corpus_dfm%>%
    textstat_frequency(n=10, group=party_name)

  #Visualsieren 
  corpus_dfm%>%
    textstat_frequency(n = 10, group = party_name)%>%
    filter(group!="SSW")%>%
    ggplot()+
      aes(x=reorder(feature,frequency),y=frequency)+
      geom_col()+
      facet_wrap(.~group,ncol = 2, scales = "free")+
      coord_flip()
  
  
  #Probleme "lösen"

    quanteda_corpus%>%
      tokens()%>%
      tokens_select(pattern = stopwords("de"),selection="remove")%>%
      tokens_select(pattern ="*innen", selection="remove")%>%
      tokens_select(pattern ="dass", selection="remove")%>%
      tokens_select(pattern=phrase("freie demokraten"), selection="remove")%>%
      tokens(remove_punct = T)%>%
      dfm()%>%
      textstat_frequency(n = 10, group = party_name)%>%
      filter(group!="SSW")%>%
      ggplot()+
        aes(x=reorder(feature,frequency),y=frequency)+
        geom_col()+
        facet_wrap(.~group,ncol = 2, scales = "free")+
        coord_flip()
  
  
  
  
  
#(word clouds)-------------------
    library("quanteda.textplots")
    set.seed(123)
    
    textplot_wordcloud(corpus_dfm)
    
    
     corpus_dfm%>%
        dfm_trim(min_termfreq = 20, verbose = FALSE)%>%
        textplot_wordcloud()
  
    quanteda_corpus%>%
      corpus_subset(party_name!="SSW") %>%
      tokens()%>%
      tokens_select(pattern = stopwords("de"),selection="remove")%>%
      tokens_select(pattern ="*innen", selection="remove")%>%
      tokens_select(pattern ="dass", selection="remove")%>%
      tokens_select(pattern=phrase("freie demokraten"), selection="remove")%>%
      tokens(remove_punct = T)%>%
      dfm()%>%
      dfm_group(groups = party_name) %>%
      dfm_trim(min_termfreq = 5, verbose = FALSE) %>%
      textplot_wordcloud(comparison = TRUE)
  
#Co-Occurrences  --------------------
    ##Einfache Co-Occurance---------
    corpus_co<-fcm(corpus_dfm)
    
    topfeatures(corpus_co)
    
    feat <- names(topfeatures(corpus_co, 20))
    corpus_co_select <- fcm_select(corpus_co, pattern = feat, selection = "keep")
 
    

    
    textplot_network(corpus_co_select, min_freq = 0.8, vertex_size =5)
    
    
     ##Mehr Data Cleaning & Co-Occurance---------
    
    co_simple<-
      quanteda_corpus%>%
      tokens()%>%
      tokens_tolower()%>%
      tokens_select(pattern = stopwords("de"),selection="remove")%>%
      tokens_select(pattern ="*innen", selection="remove")%>%
      tokens_select(pattern ="dass", selection="remove")%>%
      tokens_select(pattern=phrase("freie demokraten"), selection="remove")%>%
      tokens(remove_punct = T, remove_numbers = T)%>%
      fcm(context="window",window=20)
  
    
    feat <- names(topfeatures(co_simple, 25))
    corpus_co_select <- fcm_select(co_simple, pattern = feat, selection = "keep")
 
    

    
    textplot_network(corpus_co_select, min_freq = 0.8, vertex_size =5)
    
    ##Spezifische Worte
    
    
    co_features<-names(topfeatures(co_simple["integration", ], n=40))
    corpus_co_select <- fcm_select(co_simple, pattern = co_features, selection = "keep")
 
    corpus_co_select

    
    textplot_network(corpus_co_select, min_freq = 0.8, vertex_size =5)
    

