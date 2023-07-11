 Setup -------------------------------------------------------------------
  library(quanteda)
  library(tidyverse)

  quanteda_corpus<-read_rds("quanteda_corpus.rds")

#Basics ------------------------------------------------------

##Data Cleaning--------------

  quanteda_corpus$party_name<-car::recode(quanteda_corpus$party,"
                                          41113='GRUNEN';
                                          41223= 'LINKE';
                                          41320= 'SPD';
                                          41420='FDP';
                                          41521='CDU/CSU';
                                          41953='AfD';
                                          41912='SSW'")
  
  
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
  
  
  quanteda_corpus$id_code<-name_vec_new
  

quanteda_corpus


#Frage 1: Kwic- Keywords in Context ---------------------------
  #1a)
  #Texte Lesen/keywords in Context 
  quanteda_corpus%>%
    tokens(.)%>% 
    kwic(., pattern="Ausländer", window= 10)
  
  
  #1b)
  
  #Text Lesen/Kewyord in Context mit Wildcards
  quanteda_corpus%>%
    tokens(.)%>% 
    kwic(., pattern="Ausländer*", window= 10)
  
  
  #1c)
  #Etwas schöner zu Lesen
  # quanteda_corpus %>%
  #   tokens() %>%
  #   kwic(phrase("Migrant"), window = 10)%>%
  #   DT::datatable(caption = "Keywords in context", 
  #               rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10, 
  #                                                lengthMenu = c(5, 10, 15, 20)))

  
    quanteda_corpus%>%
      corpus_subset(party_name=="AfD")%>%
      corpus_subset(pos==475)%>%
      as.character()

  
   
#Einfache Statistiken mit Document Feature Matrix-------------
  library(quanteda.textstats) 
  

  
#Word Frequencies ---------------
  #Frage 2
    
    
    quanteda_corpus%>%
      tokens()%>%
      tokens_select(pattern = stopwords("de"),selection="remove")%>%
      tokens_select(pattern ="innen", selection="remove")%>%
      tokens_select(pattern ="dass", selection="remove")%>%
      tokens_select(pattern=phrase("freie demokraten"), selection="remove")%>%
      tokens(remove_punct = T)%>%
      dfm()%>%
      dfm_wordstem(language = "german")%>%
      dfm_tolower()%>%
      textstat_frequency(n = 20, group = party_name)%>%
      filter(group=="FDP")%>%
      ggplot()+
        aes(x=reorder(feature,frequency),y=frequency)+
        geom_col()+
        facet_wrap(.~group,ncol = 2, scales = "free")+
        coord_flip()
    
    quanteda_corpus%>%
      tokens()%>%
      tokens_select(pattern = stopwords("de"),selection="remove")%>%
      tokens_select(pattern ="innen", selection="remove")%>%
      tokens_select(pattern ="dass", selection="remove")%>%
      tokens_select(pattern=phrase("freie demokraten"), selection="remove")%>%
      tokens(remove_punct = T)%>%
      dfm()%>%
      dfm_wordstem(language = "german")%>%
      dfm_tolower()%>%
      textstat_frequency(n = 20, group = party_name)%>%
      filter(group=="SPD")%>%
      ggplot()+
        aes(x=reorder(feature,frequency),y=frequency)+
        geom_col()+
        facet_wrap(.~group,ncol = 2, scales = "free")+
        coord_flip()
    
    
    
#Frage 3: Co-Occurrences  --------------------
    ##Einfache Co-Occurance---------
     quanteda_corpus%>%
      corpus_subset(party_name=="AfD")%>%
      tokens()%>%
      tokens_select(pattern = stopwords("de"),selection="remove")%>%
      tokens_select(pattern ="innen", selection="remove")%>%
      tokens_select(pattern ="dass", selection="remove")%>%
      tokens_select(pattern=phrase("freie demokraten"), selection="remove")%>%
      tokens(remove_punct = T)%>%
      dfm()%>%
      dfm_wordstem(language = "german")%>%
      dfm_tolower()%>%
      fcm()%>%
      broom::tidy()%>%
      arrange(desc(value))
    
    
#Frage 4: co-occurance+ textstats frequency
  #Identify sentences in which word is mentioned

   temp<-quanteda_corpus%>%
    tokens(.)%>% 
    kwic(., pattern="Ausländer*", window= 10)
   
   temp
   
   select_vec<-temp$docname
   
   
    quanteda_corpus%>%
      corpus_subset(id_code %in% select_vec)%>%
      tokens()%>%
      tokens_select(pattern = stopwords("de"),selection="remove")%>%
      tokens_select(pattern ="innen", selection="remove")%>%
      tokens_select(pattern ="dass", selection="remove")%>%
      tokens_select(pattern=phrase("freie demokraten"), selection="remove")%>%
      tokens(remove_punct = T)%>%
      dfm()%>%
      dfm_tolower()%>%
      textstat_frequency(n = 10)
      
   
   
   
