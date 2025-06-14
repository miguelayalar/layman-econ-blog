
remove(list = ls())

library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(purrr)

#data on remittance
in_remit <- read_excel('Remittance inflows April 2020.xlsx') %>% 
  dplyr::rename(Countries = `Migrant remittance inflows (US$ million)`)

out_remit <- read_excel('Outward remittance flows April 2020.xlsx') %>% 
  dplyr::rename(countries = 'Migrant remittance outflows (US$ million)')

summary(in_remit)
summary(out_remit)

#regions
selected_countries <- c('Ecuador', 'Colombia', 'Peru', 'Bolivia', 
                      'Argentina', 'Mexico')

# % gdp2019
remitin_prop <- in_remit %>% 
  dplyr::select(1,42) %>% drop_na() %>% 
  #na.omit() %>% 
  dplyr::rename(share_gdp2019 = `Remittances as a share of GDP in 2019 (%)`)

remitin_prop$share_gdp2019 <- as.numeric(remitin_prop$share_gdp2019)
remitin_prop$Countries <- as.factor(remitin_prop$Countries)

remitin_prop %>% 
  dplyr::filter(Countries %in% selected_countries) %>% 
  dplyr::mutate(Countries = fct_reorder(Countries, desc(share_gdp2019))) %>% 
  ggplot(aes(x=Countries, y=share_gdp2019)) + geom_col() + 
  #theme(axis.title.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()
  

#make it longer
longer_inremit <- in_remit %>% select(-42) %>% 
  pivot_longer(c(-1), names_to = 'Years', values_to = 'US$ million')

longer_inremit$Years <- as.numeric(longer_inremit$Years)

c <- longer_inremit %>% filter(Countries %in% selected_countries) #%>% na.omit()

longer_inremit %>% filter(Countries == 'Ecuador') %>% 
  ggplot(aes(x=Years, y=`US$ million`)) +
  geom_col()


str(longer_inremit)

#outflow of money

longer_outremit <- out_remit %>% select(-45) %>% 
  pivot_longer(c(-1), names_to = 'Years', values_to = 'US$ million')

longer_outremit$Years <- as.numeric(longer_outremit$Years)

c <- longer_inremit %>% filter(Countries %in% andean_countries) #%>% na.omit()

longer_outremit %>% filter(countries == 'Colombia') %>% 
  ggplot(aes(x=Years, y=`US$ million`)) +
  geom_col()


#different data
remittance_glob <- read_excel('rpw_dataset_2011_2020_q2_v1.xlsx', sheet = 6)

local_remit <- remittance_glob %>% 
  dplyr::select(2,3,9,15,16,17)
