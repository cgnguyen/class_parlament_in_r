####Setup####
    library(tidyverse)
    library(haven)
####Read in Data####
    DATA<-read_dta("voting_behavior.dta")
  
####Clean Data- simplify deviation variable####
    DATA$vote_deviate<-as_factor(DATA$vote_deviate)

    DATA$vote_deviate_rec <- dplyr::recode_factor(DATA$vote_deviate,
        "excused absence" = "absence",
        "strong deviation" = "deviation",
        "weak deviation" = "deviation",
        "unexcused absent" = "absence",
        "invalid vote" = NA_character_,
        "no deviation" ="no_deviation",
        "voting behavior not/wrongly protocolled" = NA_character_,
        "no party line due to tie within the party group" = NA_character_,
        "no party line because no absolute majority within the party group" = NA_character_
      )
          
####Generate variables by mp, party, for 17 electoral period####
    table.analysis<-
        DATA%>%
          filter(elecper==17)%>%
          group_by(mp_id,vote_deviate_rec)%>%
          summarize(n=n())%>%
          group_by(mp_id)%>%
          mutate(freq = n / sum(n))%>%
          pivot_wider( id_cols = mp_id,
                       names_from=vote_deviate_rec,
                       values_from=freq)%>%
          mutate(deviation= case_when(is.na(deviation) ~ 0,
                                      TRUE ~ deviation))
    
    
####Merge with mp characteristics####
      DATA.mp<-read_dta("mp_characteristics.dta")
      
      DATA.mp<-
        DATA.mp %>%
        filter(elecper==17)
    
      DATA<-left_join(
        table.analysis,DATA.mp)
      
####Write output as haven-dta fil####
      #Fix variable names
      write_dta(DATA, path="./Problem_set_3/problem_set_3.dta")

      