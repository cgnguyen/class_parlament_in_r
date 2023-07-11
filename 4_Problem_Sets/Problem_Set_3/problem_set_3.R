####Setup####
  #####*Activate Library#### 
    library(tidyverse)
    library(haven)

  ####*Get data####
  DATA<-read_dta("https://github.com/cgnguyen/parlament_in_r/blob/master/Problem_Set_3/problem_set_3.dta?raw=true")
      
  ####*Data cleaning####
  DATA$mandate<-as_factor(DATA$mandate)
    DATA$party_elec<-as_factor(DATA$party_elec)


####Exercise 1: Deviation and electoral safety####
          
      #Graph
      DATA %>%
        ggplot()+
        aes(x=elecsafe_overall, y=deviation)+
        geom_point()+
        theme_bw()+
        geom_smooth(method="lm")
      
      #Model
        mod_1 <- lm (deviation ~ elecsafe_overall, data=DATA)
        summary(mod_1)
        
      
 ####*Exercise 2: Deviation+ electoral safety+ fraktion####
        mod_2 <- lm (deviation ~ elecsafe_overall+party_elec, data=DATA)
        summary(mod_2)    

 ####*Exercise 3: Deviation+ electoral safety+ mandate####
        mod_3 <- lm (deviation ~ elecsafe_district+mandate+listpos, data=DATA)  
        
        summary(mod_3)    
      
    
    

    
    