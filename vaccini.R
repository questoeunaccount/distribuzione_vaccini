# pacchetti ----

rm(list=ls())
library(tidyverse)
library(magrittr)
library(curl)
library(extrafont)
library(ggtext)
library(scales)
library(readxl)
library(readr)

`%notin%` <- Negate(`%in%`)
# DFs----

## ITALIA TOTALE----
vac_tot_ita <- read_csv("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/anagrafica-vaccini-summary-latest.csv")
pop_per_fascia <- read_delim("pop_per_fascia.csv",";", escape_double = FALSE, trim_ws = TRUE)
## REGIONI----
vac_reg_ita <- read_csv("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/somministrazioni-vaccini-latest.csv")
pop_reg_per_fascia <- read_delim("pop_reg_per_fascia_2.csv", ";", escape_double = FALSE, trim_ws = TRUE)
pop_reg_per_fascia %<>% rename(nome_area=Territorio)
## PAESI EU ----
vac_tot_eu <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations-by-age-group.csv")

# manipolazione ----

## ITALIA TOTALE ----
vac_tot_ita %<>% left_join(pop_per_fascia, by = 'fascia_anagrafica')
vac_tot_ita %<>% mutate(
"% prima dose"= prima_dose/`popolazione_tot per fascia`,
"% seconda dose"= seconda_dose/`popolazione_tot per fascia`
)

## REGIONI ----
# latest <- max(vac_reg_ita$data_somministrazione)
vac_reg_ita %<>% group_by(nome_area, fascia_anagrafica, fornitore) %<>% mutate(prima_dose_sum=cumsum(prima_dose), seconda_dose_sum=cumsum(seconda_dose))
latest<-as.Date("2021-08-23")
vecchi <- unique(vac_reg_ita$fascia_anagrafica)[c(8,9)]
vac_reg_ita %<>% filter(data_somministrazione==latest)
vac_reg_ita %<>% group_by(nome_area, fascia_anagrafica) %<>% summarise(prima_dose=sum(prima_dose_sum), seconda_dose=sum(seconda_dose_sum))

vecchioni<-vac_reg_ita %>% filter(fascia_anagrafica %in% vecchi) %>% group_by(nome_area) %>% 
  summarise(prima_dose=sum(prima_dose), seconda_dose=sum(seconda_dose)) %>% 
  mutate(fascia_anagrafica="80+")

pop_reg_per_fascia %<>% filter(Età>11)
pop_reg_per_fascia %<>% group_by(nome_area) %>% mutate(fascia_anagrafica=case_when(
  between(Età, 12,19)~"12-19",
  between(Età, 20,29)~"20-29",
  between(Età, 30,39)~"30-39",
  between(Età, 40,49)~"40-49",
  between(Età, 50,59)~"50-59",
  between(Età, 60,69)~"60-69",
  between(Età, 70,79)~"70-79",
  between(Età, 80,89)~"80-89",
  Età>89 ~"90+",
)) %>% ungroup() %>% group_by(nome_area,fascia_anagrafica) %>% summarise(totale_popolazione=sum(Value))

# pop_reg_per_fascia$nome_area<-recode(pop_reg_per_fascia$nome_area, "P.A. Bolzano"="Provincia Autonoma Bolzano / Bozen")
# pop_reg_per_fascia$nome_area<-recode(pop_reg_per_fascia$nome_area, "P.A. Trento"="Provincia Autonoma Trento")
# pop_reg_per_fascia$nome_area<-recode(pop_reg_per_fascia$nome_area, "Valle d'Aosta"="Valle d'Aosta / Vallée d'Aoste")


vac_reg_ita<-rbind(vac_reg_ita,vecchioni)
vac_reg_ita %<>% filter(fascia_anagrafica != "80-89")
vac_reg_ita %<>% filter(fascia_anagrafica != "90+")
vac_reg_ita %<>% left_join(pop_reg_per_fascia)

vac_reg_ita %<>% mutate(
  "% prima dose"= prima_dose/totale_popolazione,
  "% seconda dose"= seconda_dose/totale_popolazione
)
## PAESI EU ----
latest_eu<-max(vac_tot_eu$date)
vac_tot_eu %<>% filter(location %in% c("France", "Spain", "Italy"), date==latest_eu)

# aggregate(data = vac_tot_eu,
#         location~age_group,
#         function(location) unique(location))

#grafici----
##piramide età----
ggplot(vac_tot_ita)+
  geom_col(aes(y=`% prima dose`, x=fascia_anagrafica), fill="tomato", width=1)+
  geom_col(aes(y=`% seconda dose`, x=fascia_anagrafica), fill="darkred", width=1)+
  ylab("% sul totale popolazione per fascia anagrafica")+
  xlab("Fascia anagrafica")+
  coord_flip()+
  labs(
    title = "Distribuzione delle prime e seconde dosi delle vaccinazioni per fascia d'età",
    subtitle = "In rosso chiaro le prime dosi, in rosso scuro le seconde",
    caption = "Dati provenienti dal GitHub della Protezione Civile \nElaborazioni di Daniele De Rocchi e Giacomo Panzeri (IRPPS - CNR) ")
  
ggplot(vac_reg_ita)+
  geom_col(aes(y=`% prima dose`, x=fascia_anagrafica), fill="tomato", width=1)+
  geom_col(aes(y=`% seconda dose`, x=fascia_anagrafica), fill="darkred", width=1)+
  facet_wrap(~nome_area)+
  ylab("% sul totale popolazione per fascia anagrafica")+
  xlab("Fascia anagrafica")+
  coord_flip()+
  labs(
    title = "Distribuzione delle prime e seconde dosi delle vaccinazioni per fascia d'età su base regionale",
    subtitle = "In rosso chiaro le prime dosi, in rosso scuro le seconde",
    caption = "Dati provenienti dal GitHub della Protezione Civile \nElaborazioni di Daniele De Rocchi e Giacomo Panzeri (IRPPS - CNR) ")



vac_tot_eu %>% ggplot()+
  geom_col(data = subset(vac_tot_eu, location == "Spain"), aes(x=-people_vaccinated_per_hundred, y=age_group), fill="darkslategray2", width=1)+
  geom_col(data = subset(vac_tot_eu, location == "Italy"), aes(x=people_vaccinated_per_hundred, y=age_group), fill="tomato", width=1)+
  geom_col(data = subset(vac_tot_eu, location == "Spain"), aes(x=-people_fully_vaccinated_per_hundred, y=age_group), fill="cadetblue4", width=1)+
  geom_col(data = subset(vac_tot_eu, location == "Italy"), aes(x=people_fully_vaccinated_per_hundred, y=age_group), fill="darkred", width=1)+
  ylab("Fascia anagrafica")+
  scale_x_continuous(name="% sul totale popolazione per fascia anagrafica", breaks=c(-100, -50, 0, 50, 100), 
                     labels=c("100%", "50%", "0%", "50%", "100%"))+
  labs(
    title = "Distribuzione delle prime e seconde dosi delle vaccinazioni per Italia (in rosso) e Spagna (in azzurino/grigino)",
    subtitle = "Ai più accorti non sfuggirà che i dati della Spagna sulla fascia 80+ eccedono il 100%. Il dato è così fornito dalle fonti ufficiali spagnole.",
    caption = "Dati provenienti dal GitHub di OurWorldInData (a loro volta provenienti dai db di ECDC) \nElaborazioni di Daniele De Rocchi e Giacomo Panzeri (IRPPS - CNR) ")



vac_tot_eu %>% ggplot()+
  geom_col(data = subset(vac_tot_eu, location == "France"), aes(x=-people_vaccinated_per_hundred, y=age_group), fill="seagreen1", width=1)+
  geom_col(data = subset(vac_tot_eu, location == "Italy"), aes(x=people_vaccinated_per_hundred, y=age_group), fill="tomato", width=1)+
  geom_col(data = subset(vac_tot_eu, location == "France"), aes(x=-people_fully_vaccinated_per_hundred, y=age_group), fill="seagreen", width=1)+
  geom_col(data = subset(vac_tot_eu, location == "Italy"), aes(x=people_fully_vaccinated_per_hundred, y=age_group), fill="darkred", width=1)+
  ylab("Fascia anagrafica")+
  scale_x_continuous(name="% sul totale popolazione per fascia anagrafica", breaks=c(-100, -50, 0, 50, 100), 
                     labels=c("100%", "50%", "0%", "50%", "100%"))+
  labs(
    title = "Distribuzione delle prime e seconde dosi delle vaccinazioni per Italia (in rosso) e Francia (in verde)",
    # subtitle = "Ai più accorti non sfuggirà che i dati della Spagna sulla fascia 80+ eccedono il 100%. Il dato è così fornito dalle fonti ufficiali spagnole.",
    caption = "Dati provenienti dal GitHub di OurWorldInData (a loro volta provenienti dai db di ECDC) \nElaborazioni di Daniele De Rocchi e Giacomo Panzeri (IRPPS - CNR) ")
