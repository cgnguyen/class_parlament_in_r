####Setup###
  #Load Packages
  library(tidyverse)
  library(ggthemes)
  library(haven)

  #Read in Data###
  DATA_vote<-read_dta("vote_characteristics.dta")
  
####Data Cleaning####
  DATA_vote$feld<-as_factor(DATA_vote$policy1)
  DATA_vote$fraktion<-as_factor(DATA_vote$sponsor1)
  DATA_vote$art<-as_factor(DATA_vote$vote_type)
  
####Frage 1: Zuordnung des Bereichs ####
  
  DATA_vote%>%
    group_by(feld)%>%
    summarize(n=n())%>%
    arrange(-n)%>%
    ggplot(aes(x=reorder(feld,-n),y=n))+
      geom_col()+
      theme_bw()+
      theme(axis.text.x = element_text(size=8, angle=90, vjust=0.5))
  
  
####Frage 2 relative Häufigkeit####
  DATA_vote%>%
    group_by(feld)%>%
    summarize(n=n())%>%
    mutate(freq = n / sum(n))%>%
    arrange(-n)%>%
    ggplot(aes(x=reorder(feld,-freq), y=freq))+
    geom_col()+ 
    theme_bw()+
    theme(axis.text.x = element_text(size=8, angle=90, vjust=0.5)) 
  
  
####Frage 3 relative Häufigkeit nach Partei####
  DATA_vote%>%
    group_by(fraktion,feld)%>%
    summarize(n=n())%>%
    group_by(fraktion)%>%
    mutate(freq = n / sum(n))%>%
    filter(fraktion %in% c("CDU/CSU","SPD","Greens","FDP","Left/PDS"))%>%
    ggplot(aes(x=reorder(fraktion,-freq), y=freq, fill=feld, color=feld))+
      geom_col()+
      # facet_grid(.~fraktion)+
      theme_bw()+
      theme(axis.text.x = element_text(size=8, angle=90, vjust=0.5)) 
  
  
  
  
  
  
  
####Extra - zusammenfassung der Policyfelder in stacked bar chart und zusammenfassung####
  DATA_vote%>%
    group_by(feld)%>%
    summarize(n=n())%>%
    mutate(freq = n / sum(n))%>%
    arrange(-n)%>%
      ggplot(aes(x=1, y=freq, fill=feld))+
      geom_bar(position="fill",stat="identity", color="black")+ 
      theme_bw()+
    theme(axis.text.x = element_text(size=8, angle=90, vjust=0.5)) 
  
  
    
####Frage 4: Sponsor behavior####
  
  
  
  DATA_vote%>%
    group_by(elecper,fraktion)%>%
    summarize(n=n()) %>% 
    filter(fraktion %in% c("CDU/CSU","SPD","Greens","FDP","Left/PDS"))%>%
    mutate(farbe= case_when(fraktion == "CDU/CSU" ~ 'black', 
                            fraktion == "SPD" ~ 'red', 
                            fraktion == "FDP" ~ 'yellow', 
                            fraktion == "Greens" ~ 'dark green',
                            fraktion == "Left/PDS" ~ 'purple'))%>%
    ggplot(aes(x=elecper,y=n, group=fraktion, color=fraktion))+
      geom_line()+
      theme_bw()
      # scale_color_manual(values=farbe)
  
  