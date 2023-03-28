####Setup####
  ####*Packages
  #Activate Library 
    library(tidyverse)
    library(haven)
    library(stargazer)
    library(texreg)
    library(sjPlot)
    library(ggrepel)

 
####Read in Data####
    DATA<-read_dta("voting_behavior.dta")
    
    
    
####Clean Data####
    DATA$vote_deviate<-as_factor(DATA$vote_deviate)
    
    #Simplify deviation variable
    
    
    ## Recoding DATA$vote_deviate into DATA$vote_deviate_rec
      DATA$vote_deviate_rec <- dplyr::recode_factor(DATA$vote_deviate,
        "excused absence" = "absence",
        "strong deviation" = "deviation",
        "weak deviation" = "deviation",
        "unexcused absent" = "absence",
        "invalid vote" = NA_character_,
        "voting behavior not/wrongly protocolled" = NA_character_,
        "no party line due to tie within the party group" = NA_character_,
        "no party line because no absolute majority within the party group" = NA_character_
      )
          
    
####Generate variables by Legislaturperiode, party####
    table.analysis<-
        DATA%>%
          filter(party_text %in% c("CDU","SPD","GRÜNE","FDP","Linke","Linke/PDS","B90/GR","CSU"))%>%
          group_by(vote_deviate_rec,elecper,party_text)%>%
          summarize(n=n())%>%
          mutate(freq = n / sum(n))%>%
        arrange(elecper,party_text,vote_deviate_rec)
      
      
      table.analysis%>%
        ggplot+
        aes(x=elecper,y=freq, color=party_text)+
        geom_smooth()
      
      
      
      
      
      
    
    
    
    
    
    

    
    