library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(Quandl)
library(mFilter)
library(leplot)


##-------- econ article----------

ecb_variables <- read_excel('CTASTRIM110.xlsx', sheet = 3, skip = 5) %>% 
  na.omit()
ecb1_variables <- read_excel('CTASTRIM110.xlsx', sheet = 8, skip = 5) %>% 
  na.omit()

n <- 1:20
ecb1 <- ecb_variables[-5:-5*(n),]
ecb2 <- ecb1_variables[-5:-5*(n),]
ecb1 <- ecb1 %>% rename(c(Quarter = 'Variables', pib_nominal = `P.I.B.`))
ecb2 <- ecb2 %>% rename(Quarter = 'Variables', pib_deflator = `P.I.B.`) %>% 
  dplyr::select(pib_deflator)

full_ecb <- bind_cols(ecb1, ecb2)

need_vars <- full_ecb %>% 
  mutate(real_gdp = pib_nominal/pib_deflator,
         prepared_qtr = paste(substr(Quarter,1,4),'-',rep(1:4, times=20))) %>% 
  dplyr::select(real_gdp, prepared_qtr)

need_vars <- need_vars %>% mutate(Qtr = as.yearqtr(prepared_qtr, format = "%Y - %q"),
                                  log_gdp = log(real_gdp))# %>% 
  #dplyr::select(3,2,4)


need_vars %>% ggplot() + geom_line(aes(x=Qtr, y=log_gdp)) + le_theme()


need_vars <- need_vars %>%
  mutate(trend = 1:n())

# Estimate the model with a constant and a trend
time_detrend <- fitted(lm(log_gdp ~ trend, data = need_vars))
names(time_detrend) <- NULL

# Add series to main data frame
need_vars <- need_vars %>%
  mutate(lin_trend = time_detrend)

# Create data frame for the plot
temp <- need_vars %>%
  select(Qtr, log_gdp, lin_trend) %>%
  gather(key = "Variable", value = "value", -Qtr)

cols <- le_palette()

# Plot
ggplot(temp, aes(x = Qtr, y = value, colour = Variable)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = cols, labels = c("Linear trend", "GDP (logs)")) +
  le_theme(rm_y_leg = TRUE, rm_x_leg = TRUE) +
  labs(title = "Residuals of a linear trend estimation")





# Run HP filter
hp_gdp <- hpfilter(need_vars$log_gdp, freq = 1600) #quarter data

# Add the cyclical component of the HP filter and
# the linearly detrended sereis to the main data frame
need_vars <- need_vars %>%
  mutate(hp = hp_gdp$cycle,
         lin_cycle = log_gdp - lin_trend)

# Create data frame for the plot
temp <- need_vars %>%
  select(Qtr, hp, lin_cycle) %>%
  gather(key = "Variable", value = "value", -Qtr) %>%
  filter(!is.na(value)) %>%
  mutate(Variable = factor(Variable, levels = c("hp", "lin_cycle"),
                           labels = c("HP filter", "Residuals of\na linear trend\nestimation")))

# Plot
ggplot(temp, aes(x = Qtr, y = value, colour = Variable)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = cols) +
  le_theme(rm_y_leg = TRUE, rm_x_leg = TRUE) +
  labs(title = "Output Gap")



library(neverhpfilter)

# Get the series
y <- as.xts(ts(need_vars$log_gdp, start = 2000, frequency = 4))

# Dimnames must be specified, otherwise the function won't accept the input
dimnames(y) <- list(NULL, "log_gdp")

# Estimate
hamilton_temp <- yth_filter(y, h = 8, p = 4)

# Add residuals to the main data frame
need_vars <- need_vars %>%
  mutate(hamilton = as.numeric(hamilton_temp$log_gdp.cycle))

# Prepare dataset for plot
temp <- need_vars %>%
  select(Qtr, hamilton, hp, lin_cycle) %>%
  gather(key = "Variable", value = "value", -Qtr) %>%
  filter(!is.na(value)) %>%
  mutate(Variable = factor(Variable,
                           levels = c("hp", "hamilton", 'lin_cycle'),
                           labels = c("HP filter", 
                                      "Hamilton's\nalternative\nto the HP-filter", 
                                      "Linear trend")))

# Plot
ggplot(temp, aes(x = Qtr, y = value, colour = Variable)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = cols) +
  le_theme(rm_y_leg = TRUE, rm_x_leg = TRUE) +
  labs(title = "Hamilton's alternative to the HP filter",
       caption = "Source: Central Bank of Ecuador")



# use penn data for EC 1950-2017
ec_penn <- read_excel("raw_data_ECU.xlsx", sheet = 3) %>% janitor::clean_names()


ec_penn_rgdp <- ec_penn %>%
  mutate(trend = 1:n(),
         log_gdp = log(rgdp_penn)) %>% 
  filter(year<=2017)

# Estimate the model with a constant and a trend
time_detrend <- fitted(lm(log_gdp ~ trend, data = ec_penn_rgdp))
names(time_detrend) <- NULL

# Add series to main data frame
ec_penn_rgdp <- ec_penn_rgdp %>%
  mutate(lin_trend = time_detrend,
         lin_cycle = log_gdp - lin_trend)

# Create data frame for the plot
temp <- ec_penn_rgdp %>%
  select(year, log_gdp, lin_trend, lin_cycle) %>%
  gather(key = "Variable", value = "value", -year) %>% 
  mutate(
    type = if_else(Variable == "lin_cycle", "Gap", "Levels")
  )

# Plot
ggplot(temp, aes(x = year, y = value, colour = Variable)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~factor(type, c("Levels", "Gap")), scales = "free_y", nrow = 2) +
  geom_hline(data = data.frame(type = "Gap", yint = 0),
             aes(yintercept = yint)) +
  scale_colour_manual(values = cols, labels = c("Gap","Linear trend", "GDP (logs)")) +
  le_theme(rm_y_leg = TRUE, rm_x_leg = TRUE, scale = 1) +
  labs(title = "Detrending using linear regression")


