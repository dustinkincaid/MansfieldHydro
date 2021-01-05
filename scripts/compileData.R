# Compile Mansfield data from Beverley Wemple and Jamie Shanley
# Written by D. Kincaid 2021-01-04

# LOAD PACKAGES ----
library("groundhog")                            # To assure the same version of package is always loaded       
groundhog.day = "2021-01-01"
groundhog.library("tidyverse", groundhog.day)   # Loads multiple useful packages
groundhog.library("lubridate", groundhog.day)   # For working with dates
groundhog.library("zoo", groundhog.day)         # Data gap filling functions
groundhog.library("fs", groundhog.day)          # For reading in and binding multiple CSV files at once

# READ & TIDY DATA ----
# Daily runoff values (mm) for WY01-19; should be able to get this updated to WY20 too
# Note: the dates that fail to parse are 29-Feb from years that don't have a leap day
runoff <- read_csv("data/MM daily mm master WY01-19_v5.csv") %>% 
  # select(-(`RB average 01-16`:`WB cum mm`)) %>% 
  # slice(1:365) %>% 
  pivot_longer(cols = `RB WY01`:ncol(.), names_to = "id", values_to = "q_mm") %>% 
  mutate(site = str_sub(id, 1, 2),
         wyear = parse_number(id),
         wyear = 2000 + wyear) %>% 
  rename_all(tolower) %>% 
  # Format date
  mutate(mday = parse_number(date),
         month = str_sub(date, -3, -1),
         year = ifelse(month %in% c("Oct", "Nov", "Dec"), wyear - 1, wyear),
         date = ymd(paste(year, month, mday, sep = "-"))) %>% 
  select(site, wyear, date, doy_wyear = jday, q_mm) %>% 
  arrange(site, date)

# Daily snow data from snow stake on eastern side of Mt. Mansfield from National Climate Data Center
# Original depths are in inches, but I convert to meters here
snow <- read_csv("data/NCDC_MtMansfield Snow Stake.csv") %>% 
  rename_all(tolower) %>% 
  select(date, snow_dep = snwd) %>% 
  # Convert snow depth from inches to meters
  mutate(snow_dep_m = snow_dep * 0.0254) %>% 
  select(-snow_dep) %>% 
  # Format date
  mutate(date = mdy(date)) %>% 
  filter(!is.na(date))

# Daily MET summary data from Morrisville-Stowe Airport from GHCN on 2020-01-04 (see READ ME_GHCN.txt for details)
# No snowfall or snow depth data
# Daily mean temps end on 2005-07-31
met <- read_csv("data/GHCN_DailyMet_MorrisvilleAirport_2021-01-04.csv", col_types = cols(SNWD = "d", SNOW = "d")) %>% 
  rename_all(tolower) %>% 
  select(date, precip_mm = prcp, temp_avg = tavg, temp_max = tmax, temp_min = tmin, wind_spd_avg = awnd) %>% 
  # We'll fill missing temps using last observation carried forward
  # Recall daily mean temps end on 2005-07-31
  # Max data gap set to 2
  mutate_at(vars(c(temp_avg, temp_max, temp_min)),
            list(~ na.locf(., maxgap = 2, na.rm = FALSE)))

# VWC MET data
data_dir <- "data/VMC Mansfield met data"
met_vwc <- 
  data_dir %>% 
  # List only the CSV files in the folder
  fs::dir_ls(regexp = "\\.csv$") %>% 
  # Read in all CSV files and compile into one df via row-binding
  map_dfr(read_csv) %>% 
  # map_dfr(read_csv, .id = "source") %>% 
  # Drop precip column (unreliable; unsure of units; and they don't collect in winter)
  select(-precip) %>% 
  # Format date
  rename(datetime = date) %>% 
  mutate(datetime = mdy_hm(datetime),
         date = date(datetime)) %>% 
  select(datetime, date, everything()) %>% 
  arrange(datetime)

# Stowe Mountain Resort water withdrawals
withdraw <- read_csv("data/SMR withdrawal summary.csv") %>% 
  mutate(year = ifelse(month >= 10, wyear - 1, wyear))
  

# CALCULATE ADDITIONAL VARIABLES
# Cumulative runoff
runoff <- 
  runoff %>% 
  group_by(site, wyear) %>% 
  mutate(q_mm_cum = cumsum(q_mm)) %>% 
  ungroup()

# Let's look at these curves to make sure they make sense
runoff %>% 
  ggplot(aes(x = doy_wyear, y = q_mm_cum, group = site, color = site)) +
  facet_wrap(~wyear, ncol = 4) +
  geom_path()

# Calculate daily means for VWC data
met_vwc_daily <-
  met_vwc %>% 
  select(datetime, date, airtemp_c, pyranometer_watt_m2) %>% 
  group_by(date) %>% 
  summarize(across(c(airtemp_c, pyranometer_watt_m2), ~ round(mean(.x, na.rm = F), 1))) %>% 
  rename(temp_avg_vwc = airtemp_c)

# Compare daily temp. means from GHCN and VWC
compareTemp <- 
  full_join(met, met_vwc_daily)

compareTemp %>% 
  # Focus on March-May
  filter(month(date) >= 3 & month(date) <= 5) %>% 
  ggplot(aes(x = temp_avg, y = temp_avg_vwc)) +
  geom_point() +
  geom_abline(slope = 1) +
  geom_smooth(method = "lm")

lm.temp <- lm(temp_avg_vwc ~ temp_avg, data = compareTemp %>% filter(month(date) >= 3 & month(date) <= 5))
summary(lm.temp)

# Total SMR withdrawals
withdraw <- 
  withdraw %>% 
  rowwise() %>% 
  mutate(use_total_mm = sum(use_snowmaking_mm, use_diverted_mm, use_golf_mm, na.rm = T)) %>% 
  select(wyear, month, year, everything())

# JOIN DFs INTO ONE DF
# Still need to join the WITHDRAW df
alldat <- full_join(runoff, snow) %>% 
  full_join(met) %>% 
  full_join(met_vwc_daily)
