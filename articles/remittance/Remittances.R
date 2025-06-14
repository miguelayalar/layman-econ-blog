## R code ##
#packages
library(tidyverse)
library(janitor)
library(wbstats)
library(fpp3)
library(leplot)

remit_data <- wb_data('BX.TRF.PWKR.CD.DT', country = 'regions_only', date_as_class_date = T) %>%
  dplyr::select(2:5)
#BX.TRF.MGR.CD GFDD.OI.13


remit_gdp21 <- wb_data('GFDD.OI.13', country = 'all', 
                     date_as_class_date = F, 
                     start_date = 2004, 
                     end_date = 2021) %>% 
  dplyr::select(2:5)

colours <- c(le_palette(),"#2a9d8f","#fdb462")#, "#0f4c5c","#003049","#6d6875","#232ED1","#D7263D")

#plot remittances by regions
remit_data %>% drop_na() %>% 
  #filter(iso3c %in% regions) %>% 
  dplyr::mutate(region = as.factor(country),
                value = BX.TRF.PWKR.CD.DT/1e+9,
                year = year(date)) %>% 
  ggplot(aes(x= year, y = value, colour = region)) + 
  geom_line(size = 1.1) + 
  scale_color_manual("Region",values = colours) +
  ggtitle(label = 'Remittances by regions', subtitle = "Personal remittances, received (current US$)") +
  labs(y = 'US$ billion') + 
  le_theme(scale = 1.2, rm_x_leg = TRUE)

chart_save("Remittances-regions")


#plot of high-dependant countries
remit_gdp21 %>% 
  group_by(country) %>% summarise(avg = mean(GFDD.OI.13, na.rm = T)) %>% 
  arrange(desc(avg)) %>% head(10) %>% 
  ggplot(aes(avg, fct_reorder(country, avg))) +
  geom_col(fill = colours[1]) +
  scale_x_continuous(expand = expansion(mult = c(0, .1))) +
  xlab("% of GDP") +
  ggtitle(label = 'Reliance on remittances', 
          subtitle = "10 largest recipients of remittances") +
  le_theme(scale = 1.1, rm_y_leg = TRUE)

chart_save("Remittances-reliance")


# Ecuador Remittances
#https://contenido.bce.fin.ec/documentos/Estadisticas/SectorExterno/BalanzaPagos/Remesas/indice.htm
#get the total amount
remit_ecu <- readxl::read_xlsx('RemesasPub.xlsx', sheet = 1) %>% 
  slice(-1:-4) %>% drop_na()

remitecu_tidy <- remit_ecu %>% 
  pivot_longer(cols = 2:57, names_to = 'Date', values_to = 'value_thsnd') %>% 
  mutate(year = paste(substr(Date,1,4)),
         quarter = rep(c(3,6,9,12), 14),
         year_quarter = as.Date(paste0(substr(year,1,4),'-',substr(quarter,1,2),'-01')),
         region = REGIÓN) %>% 
  dplyr::select(year_quarter, region, value_thsnd) %>% 
  janitor::clean_names()

remitecu_ts <- remitecu_tidy %>% 
  dplyr::mutate(quarter = yearquarter(year_quarter)) %>% 
  dplyr::select(-year_quarter) %>% 
  as_tsibble(key = region,
             index = quarter)

#plot
remitecu_ts %>% 
  mutate(value = value_thsnd/1e+3,
         year = year(quarter)) %>% 
  autoplot(value, colour = colours[1], size = 1.2) +
  scale_x_yearquarter(date_breaks = '2 years') +
  scale_y_continuous(label = scales::comma)+
  labs(title = "Ecuador's remittances intake",
       subtitle = "Remittances in Ecuador grew during pandemic recession",
       y = "US$ (Millions)") +
  le_theme(scale = 1.1, rm_x_leg = TRUE)


chart_save("Remittances-Ecuador")
