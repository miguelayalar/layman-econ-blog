#packages
library(tidyverse)
library(foreign)
library(WDI)
library(wbstats)
library(ggiraph)
library(leplot)
cols <- le_palette()

#data
baci_data <- read_csv('BACI_HS12_Y2017_V202001.csv')
country_cod <- read_csv('country_codes_V202001.csv')
gravdata <- foreign::read.dta('gravdata.dta')
gdp_search <- wb_search("gdp")
gdp_imp <- wb_data("NY.GDP.MKTP.CD", start_date = 2017, end_date = 2017) %>% 
  select(1:5) %>% drop_na()

gdp_imp %>% summary()

baci_data %>% summary()
#variables
#k	Product category (HS 6-digit code)
#i	Exporter (ISO 3-digit country code)
#j	Importer (ISO 3-digit country code)
#v	Value of the trade flow (in thousands current USD)
#q	Quantity (in metric tons)

#--------- tidy and transformation -----------
uk_xp <- baci_data %>% filter(i == 826) %>% 
  select(-q) %>% 
  dplyr::mutate(j = as.factor(j)) %>% 
  dplyr::group_by(j) %>% 
  dplyr::summarise(total = sum(v))

uk_xp <- uk_xp %>% dplyr::rename(country_code = j,
                          totbilateral_exp = total) %>% 
  mutate(country_code = as.character(country_code))

cout_cod <- country_cod %>% 
  select(1,4) %>% 
  dplyr::mutate(country_code = as.character(country_code)) %>% 
  dplyr::rename(iso2c = iso_2digit_alpha)

uk_gravity <- inner_join(x = uk_xp, y = cout_cod, by = "country_code")

#left_join(x = uk_xp, y = cout_cod, by = "country_code")

uk_fulldata <- inner_join(x = uk_gravity, y = gdp_imp, by = "iso2c")
uk_fulldata <- uk_fulldata %>% 
  mutate(log_exp = log(totbilateral_exp),
         lgdp = log(NY.GDP.MKTP.CD))


#plot 1
p1 <- uk_fulldata %>%
  ggplot(aes(x = lgdp, y = log_exp)) + 
  geom_point(colour=cols[1], size = 3) +
  ylab('UK Bilateral Exports (log 000 US$)') +
  xlab('Importer GDP (log 000 US$)') +
  geom_smooth(method="lm",colour="#0f4c5c") +
  ggtitle('UK Bilateral Exports and Importer GDP, 2017',
          subtitle = "") +
  le_theme()

p1
chart_save("UK billateral trade")

eq <- lm(log_exp ~ lgdp, data = uk_fulldata)
summary(eq)

#the ratio of UK exports to importer GDP.
#note: iso2_o code for exporting country, iso2_d code for importing country
gravdist <- gravdata %>% 
  filter(year == 2015, 
         iso2_o == 'GB') %>% 
  select('iso2_d','distw', 'eu_d', 'fta_wto') %>% 
  dplyr::rename(iso2c = iso2_d) %>% 
  as_tibble()

ukfulldata_grav <- inner_join(x = uk_fulldata, y = gravdist, by = "iso2c")

uk_data <- ukfulldata_grav %>% 
  dplyr::mutate(loratio_xm = log(totbilateral_exp/NY.GDP.MKTP.CD),
                lodistw = log(distw)) %>% 
  select(-4,-6,-10)


#plot 2
p2 <- uk_data %>%
  ggplot(aes(x = lodistw, y = loratio_xm)) + 
  geom_point(colour=cols[1], size = 3) +
  ylab('UK Bilateral Exports/Importer GDP (log)') +
  xlab('Distance (log km)') +
  geom_smooth(method="lm",colour="#0f4c5c") +
  ggtitle('UK Bilateral Exports/Importer GDP and Distance, 2017',
          subtitle = "") +
  le_theme()

p2
chart_save("UK billateral trade distance")


eq2 <- lm(loratio_xm ~ lodistw, data = uk_data)
summary(eq2)

#plot3
p3 <- uk_data %>%
  dplyr::mutate(eu_member = factor(eu_d, levels = c(1,0),
                            labels = c("EU member","No EU member")),
         `Value of trade in billions` = totbilateral_exp/1000000) %>% 
  ggplot(aes(x = lodistw, y = loratio_xm)) + 
  geom_point(aes(x = lodistw, y = loratio_xm, size = `Value of trade in billions`,
                 colour = eu_member), alpha=1) +
  scale_color_manual("EU Member",values = cols) +
  scale_size(range = c(2, 10)) +
  geom_smooth(method="lm", colour="#0f4c5c") +
  le_theme(scale = 1) +
  ylab('UK Bilateral Exports/Importer GDP (log)') +
  xlab('Distance (log km)') +
  ggtitle('UK Bilateral Exports/Importer GDP and Distance, Scaled by Exports, 2017',
          subtitle = "Size is value of trade in billions")

p3
chart_save("UK billateral trade distance scaled")

# replicating same for australia

#--------- tidy and transformation -----------
aus_xport <- baci_data %>% filter(i == 36) %>% 
  dplyr::select(-q) %>% 
  dplyr::mutate(j = as.factor(j)) %>% 
  dplyr::group_by(j) %>% 
  dplyr::summarise(totbilateral_exp = sum(v))

aus_xport <- aus_xport %>% 
  dplyr::rename(country_code = j) %>% 
  dplyr::mutate(country_code = as.character(country_code))

countrycode <- country_cod %>% 
  select(1,4) %>% 
  dplyr::mutate(country_code = as.character(country_code)) %>% 
  dplyr::rename(iso2c = iso_2digit_alpha)

aus_gravity <- inner_join(x = aus_xport, y = countrycode, by = "country_code")

aus_fulldata <- inner_join(x = aus_gravity, y = gdp_imp, by = "iso2c")

aus_fulldata <- aus_fulldata %>% 
  dplyr::mutate(log_exp = log(totbilateral_exp),
         lgdp = log(NY.GDP.MKTP.CD))


#plot 1
p4 <- aus_fulldata %>%
  ggplot(aes(x = lgdp, y = log_exp)) + 
  geom_point(colour=cols[1], size = 3) +
  ylab('AUS Bilateral Exports (log 000 US$)') +
  xlab('Importer GDP (log 000 US$)') +
  geom_smooth(method="lm", colour="#0f4c5c") +
  ggtitle('AUS Bilateral Exports and Importer GDP, 2017',
          subtitle = "") +
  le_theme(scale = 1)

p4
chart_save("AUS billateral trade")

auseq <- lm(log_exp ~ lgdp, data = aus_fulldata)
summary(auseq)

#the ratio of AUS exports to importer GDP.
#note: iso2_o code for exporting country, iso2_d code for importing country
gravdist <- gravdata %>% 
  filter(year == 2015, 
         iso2_o == 'AU') %>% 
  dplyr::select('iso2_d','distw', 'eu_d', 'colony', 'fta_wto') %>% 
  dplyr::rename(iso2c = iso2_d) %>% as_tibble()

ausfulldata_grav <- inner_join(x = aus_fulldata, y = gravdist, by = "iso2c")

aus_data <- ausfulldata_grav %>% 
  dplyr::mutate(loratio_xm = log(totbilateral_exp/NY.GDP.MKTP.CD),
         lodistw = log(distw)) %>% 
  select(-4,-6,-10)


#plot 2
p5 <- aus_data %>%
  dplyr::mutate(fta_wto = factor(fta_wto, levels = c(1,0),
                            labels = c("FTA","No FTA"))) %>% 
  ggplot(aes(x = lodistw, y = loratio_xm)) + 
  geom_point(aes(x = lodistw, y = loratio_xm, colour = fta_wto), size = 3, alpha=1) +
  scale_color_manual("Free Trade Agreement",values = cols) + 
  ylab('AUS Bilateral Exports/Importer GDP (log)') +
  xlab('Distance (log km)') +
  geom_smooth(method="lm", colour="#0f4c5c") +
  ggtitle('AUS Bilateral Exports/Importer GDP and Distance, 2017',
          subtitle = "") +
  le_theme(scale = 1.1)

p5
chart_save("AUS billateral trade distance")

auseq2 <- lm(loratio_xm ~ lodistw, data = aus_data)
summary(auseq2)

#plot3
p6 <- aus_data %>%
  mutate(Countries = factor(fta_wto, levels = c(1,0),
                          labels = c("FTA","No FTA")),
         `Value of trade in billions` = totbilateral_exp/1000000) %>% #Value of the trade flow(in millions current USD)) 
  ggplot(aes(x = lodistw, y = loratio_xm)) + 
  geom_point(aes(x = lodistw, y = loratio_xm, size = `Value of trade in billions`,
                 colour = Countries), alpha=1) +
  scale_color_manual("Free Trade Agreement",values = cols) +
  scale_size(range = c(1, 10)) +
  ylab('AUS Bilateral Exports/Importer GDP (log)') +
  xlab('Distance (log km)') +
  geom_smooth(method="lm", colour="#0f4c5c") +
  ggtitle('AUS Bilateral Exports/Importer GDP and Distance, Scaled by Exports, 2017',
          subtitle = "Size is value of trade in billions") +
  le_theme(scale = 1.1)


p6
chart_save("AUS billateral trade distance scaled")
