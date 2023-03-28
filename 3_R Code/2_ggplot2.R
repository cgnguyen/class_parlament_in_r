####Setup####
  library(haven)
  library(tidyverse)
  library(ggthemes)  

  



####Daten Einlesen####
  
  #Parlamentarier
  DATA_mp<-read_dta("mp_characteristics.dta")

  View(DATA_mp)
  
####Daten Reinigen####
  DATA_mp$geschlecht<-as_factor(DATA_mp$gender)
  DATA_mp$partei<-as_factor(DATA_mp$party_elec)
  
  
####Einfache Deskriptive Daten-Männder oder Frauen####

  ####*Einfache Balkendiagramme####
  data_gender<-
    DATA_mp %>%
      group_by(geschlecht)%>%
      summarize(n=n())
  
  #Einfache Darstellung 
  graph_gender_einfach<-
    ggplot(data=data_gender,
         aes(x=geschlecht, y=n))+
      geom_col(stat="count")
  
  
  #Darstellung Anpassen
  graph_gender_einfach+
    theme_bw()+
    xlab("Geschlecht")+
    ylab("Anzahl")+
    ggtitle("Bundestag 1949-2013 - Geschlechter")+
    scale_x_discrete(labels=c("Frauen","Männer"))
  
  
    
  
  ####*Komplizierte Balkendiagramme####
  #Frauen und Männerparteien
  
  DATA_mp %>%
    group_by(partei,geschlecht) %>%
    summarize(n=n())%>%
    mutate(freq = n / sum(n))
  
  #Version 1-  in einer Darstellung 
  DATA_mp %>%
    group_by(partei,geschlecht) %>%
    summarize(n=n())%>%
    mutate(freq = n / sum(n))%>%
    filter(partei!="unaffiliated")%>%
    ggplot()+
      aes(x=partei,y=freq, fill=geschlecht)+
      geom_col(position="dodge")+
    theme_bw()
  
  
  #Version 2 - Facet Grid
  DATA_mp %>%
    group_by(partei,geschlecht) %>%
    summarize(n=n())%>%
    mutate(freq = n / sum(n))%>%
    filter(partei!="unaffiliated")%>%
    ggplot()+
    aes(x=geschlecht,y=freq)+
    geom_col()+
    facet_grid(.~partei)+
    scale_x_discrete(labels=c("Frauen","Männer"))+
    theme_bw()
  
  #Achtung! Hier werden Menschen doppelt gezählt, da Datenbasis Legislaturperiode ist. 
  #Zusammenfassung durch unique - auch hier Achtung: Es gibt wenige Menschen die wechseln 
  
  DATA_mp %>%
    group_by(mp_id)%>%
    summarize(geschlecht=unique(geschlecht),
              partei=unique(partei[1]))%>%
    group_by(partei,geschlecht) %>%
    summarize(n=n())%>%
    mutate(freq = n / sum(n))%>%
    filter(partei!="unaffiliated")%>%
    ggplot()+
    aes(x=geschlecht,y=freq)+
    geom_col()+
    facet_grid(.~partei)+
    scale_x_discrete(labels=c("Frauen","Männer"))+
    theme_bw()

####Lieniendiagramme####    
  
  
####*Einfaches Diagram- Männer oder Frauen? Über die Jahre####
  DATA_mp %>%
    group_by(elecper,geschlecht)%>%
    summarize(n=n()) %>%
    mutate(freq = n / sum(n))%>%
    ggplot()+
      aes(x=elecper, y=freq, color=geschlecht)+
      geom_line()+
      theme_bw()
  
  
####*Facet Diagram- Männer oder Frauen? Über die Jahre + Partei####
 DATA_mp %>%
    group_by(elecper,geschlecht,partei)%>%
    summarize(n=n())%>%
    group_by(partei,elecper)%>%
    mutate(freq = n / sum(n))%>%
    filter(partei!="unaffiliated")%>%
    filter(partei!="other party")%>%
    ggplot()+
      aes(x=elecper, y=freq, color=geschlecht)+
      geom_line()+
      facet_grid(.~partei)+
      theme_bw()
  

  

    
  
  
  
  
  
  
  
  
  
  

  
  
  
    
####Bonus- Seats in Parliament####
  library(ggparliament)
  
  germany<-
    DATA_mp %>%
    group_by(elecper,partei,geschlecht) %>%
    summarize(seats=n())%>%
    filter(partei!="unaffiliated")%>%
    filter(elecper==13)
  
    germany <- parliament_data(election_data = germany, 
                             parl_rows = 10,
                             type = 'semicircle',
                             party_seats = germany$seats)
  
    bundestag <- ggplot(germany, aes(x, y, colour=partei)) +
      geom_parliament_seats(size = 2) +
      geom_emphasize_parliamentarians(geschlecht == "female") +
      labs(colour="Party") +
      theme_ggparliament(legend = TRUE)+
      scale_colour_manual(values = c("Red","Black","Blue","Yellow","Dark Green","Purple"),
                          limits = germany$party_short)  +
      labs(title="Parteien - und Geschlechterverteilung im Bundestag",
           caption="13 Legislaturperiode. Weibliche Abgeordnete wurden farblich hervorgehoben")
      
  
    bundestag    
  
  
    
    
    
    
    
####Bonus-Animated Graphs####
    library(plotly)
    
  
    plot_basic<-
      DATA_mp %>%
      group_by(elecper,geschlecht)%>%
      summarize(n=n()) %>%
      mutate(freq = n / sum(n))%>%
      ggplot()+
      aes(x=elecper, y=freq, color=geschlecht)+
      geom_line()+
      theme_bw()

    plot_animated<-ggplotly(plot_basic)  
    
    plot_animated 
    
    
    
    
  
  