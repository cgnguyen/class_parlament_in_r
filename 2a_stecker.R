####Setup####
  library(tidyverse)
  library(lubridate)

####Daten Laden und Anschauen####

load("extract_christoph.Rda")

DATA<-extract


View(DATA)

####Daten Reinigen####
DATA$year<-lubridate::year(DATA$verk_date)



####Abbildung herstellen####

temp<-DATA%>%
  group_by(year,bill_type) %>%
  summarize(n=n())%>%
  filter(!is.na(bill_type))%>%
  group_by(year)%>%
  mutate(freq = n / sum(n))%>%
  filter(bill_type=="Zustimmungsgesetz")%>%
  filter(year>1977)%>%
  ggplot()+
  aes(x=year,y=freq)+
  geom_line()+
  ylim(0,0.75)+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, angle=45, vjust=0.5))+
  scale_x_continuous(breaks = seq(1978, 2014, by = 2))+
  geom_vline(xintercept=2006, color="grey")+
  geom_label(aes(x=2006,y=0.7, label= "Reform"))

temp

ggsave(plot=temp, file="temp.png" , width = 20 , height =10 , unit= "cm")


  
  
