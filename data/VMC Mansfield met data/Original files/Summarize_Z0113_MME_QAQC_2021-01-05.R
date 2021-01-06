# Trim full dataset from FEMC and summarize by hourly means

dat <- read_csv("data/VMC Mansfield met data/Original files/Z0113_MME_QAQC_2021-01-05.csv", col_types = cols(PRECIP = "d")) %>% 
  mutate(RECORDTIME = mdy_hm(RECORDTIME))

sub <-
  dat %>% 
  filter(RECORDTIME >= ymd_hms("2012-08-27 10:00:00") & RECORDTIME < ymd_hms("2020-09-30 23:59:59")) %>% 
  rename_all(tolower) %>% 
  select(timestamp = recordtime, airtemp_c = airtemp, windspeed_m_s = mean_horizontal_windspeed, rh_per = relhumid, pyranometer_watt_m2 = pyranom) %>% 
  mutate(time_hourly = floor_date(timestamp, unit = "hour")) %>% 
  group_by(time_hourly) %>% 
  summarize(across(c(airtemp_c:pyranometer_watt_m2), ~mean(.x, na.rm = T))) %>% 
  rename(date = time_hourly)

sub %>% 
  mutate(date = as.character(date)) %>% 
  write_csv("data/VMC Mansfield met data/mansfield-13-20.csv")


