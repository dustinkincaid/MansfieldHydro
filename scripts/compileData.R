# Compile Mansfield data from Beverley Wemple and Jamie Shanley
# Written by D. Kincaid 2021-01-04
# Updated by D. Kincaid 2021-06-22


# LOAD PACKAGES ----
# library("groundhog")                            # To assure the same version of package is always loaded       
# groundhog.day = "2021-06-18"
# groundhog.library("tidyverse", groundhog.day)   # Loads multiple useful packages
# groundhog.library("lubridate", groundhog.day)   # For working with dates
# groundhog.library("zoo", groundhog.day)         # Data gap filling functions
# groundhog.library("fs", groundhog.day)          # For reading in and binding multiple CSV files at once
# groundhog.library("data.table", groundhog.day)

library("tidyverse")
library("lubridate")
library("zoo")
library("fs")
library("data.table")


# READ & TIDY DATA ----
# Daily runoff values (mm) for WY01-19; should be able to get this updated to WY20 too
# Note: the dates that fail to parse are 29-Feb from years that don't have a leap day
  runoff <- read_csv("data/MM daily mm master WY01-20.csv") %>% 
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
    # Feb 29 on leap years are missing doy_wyear, so let's redo these
    arrange(site, date) %>% 
    group_by(site, wyear) %>% 
    mutate(doy_wyear = row_number()) %>% 
    ungroup() %>% 
    filter(!is.na(date))
    
# Daily snow data from snow stake on eastern side of Mt. Mansfield from National Climate Data Center (GHCND:USC00435416)
# Original depths are in inches, but I convert to meters here
  snow <- read_csv("data/NCDC_MtMansfield Snow Stake_plus2020.csv") %>% 
    rename_all(tolower) %>% 
    select(date, snow_dep = snwd) %>% 
    # Convert snow depth from inches to meters
    mutate(snow_dep_m = snow_dep * 0.0254) %>% 
    select(-snow_dep) %>% 
    # Format date
    mutate(date = ymd(date)) %>% 
    filter(!is.na(date)) %>% 
    arrange(date) %>% 
    # Weirdly missing these dates; add them
    bind_rows(data.frame(date = c(ymd("2011-04-10"), ymd("2013-05-30")), snow_dep_m = c(NA, NA))) %>%
    # Linear interpolate missing values
    mutate(snow_dep_m = na.approx(snow_dep_m, x = date, xout = date, maxgap = 10, na.rm = FALSE))
  
  # Of the spring dates (DOY 170 to 245), we are missing snow depth data from 3/29/20 to 4/9/20
  # Let's use linear interpolation to fill this shortish gap
  snow <-
    snow %>% 
    arrange(date) %>% 
    mutate(snow_dep_m = na.approx(snow_dep_m, x = date, xout = date, maxgap = 15, na.rm = FALSE))

# Daily MET summary data from Morrisville-Stowe Airport from GHCN on 2020-01-04 (see READ ME_GHCN.txt for details)
# No snowfall or snow depth data
# Daily mean temps end on 2005-07-31
  met <- read_csv("data/GHCN_DailyMet_MorrisvilleAirport_2021-01-04.csv", col_types = cols(SNWD = "d", SNOW = "d")) %>% 
    rename_all(tolower) %>% 
    select(date, precip_mm = prcp, temp_avg = tavg, temp_max = tmax, temp_min = tmin) %>% 
    # This data set is mysteriously missing data for 2012-02-28
    # I got the precip value for this day from the Stowe CoCoRaHS station (VT-LM-1): https://www.cocorahs.org/ViewData/StationPrecipSummary.aspx
    bind_rows(data.frame(date = ymd("2012-02-28"), precip_mm = 3.81)) %>% 
    arrange(date) %>% 
    # We'll fill missing temps using last observation carried forward
    # Recall daily mean temps end on 2005-07-31
    # Max data gap set to 2
    mutate_at(vars(c(temp_avg, temp_max, temp_min)),
              list(~ na.locf(., maxgap = 2, na.rm = FALSE))) %>% 
    # Calculate temp_avg for after 2005-07-31 by taking the average of temp_max and temp_min
    mutate(temp_avg = ifelse(is.na(temp_avg), (temp_min + temp_max)/2, temp_avg))
  
  # Let's gap fill precip data here with VWC/FEMC data
  # I'm reading in the full dataset downloaded from their website on 2021-01-05
  # Below I read in VWC/FEMC data, but I use Beverley's gap-filled timeseries from 2000-2012 and then use the data
  # downloaded on 2021-01-05 for 2012-2020 (I only read in a trimmed file below)
  met_vwc_precip <- read_csv("data/VMC Mansfield met data/Original files/Z0113_MME_QAQC_2021-01-05.csv", col_types = cols(PRECIP = "d")) %>% 
    rename_all(tolower) %>% 
    mutate(recordtime = mdy_hm(recordtime)) %>% 
    mutate(date = date(recordtime)) %>% 
    select(date, recordtime, precip_mm = precip) %>% 
    # Calculate daily precip totals
    group_by(date) %>% 
    summarize(precip_mm_vwc = sum(precip_mm))
  
  # Join the dfs and fill missing airport precip values with VWC value
  # Still ~25 missing precip values, the only continuous gap is 2009-03-08 to 2009-03-21
  # I checked the Stowe station (VT-LM-1) on CoCoRaHS and it is missing precip data for the 2009-03 period as well
  # Going to replace remaining NAs with 0s b/c there are relatively few
  met <-
    left_join(met, met_vwc_precip) %>% 
    mutate(precip_mm = ifelse(is.na(precip_mm), precip_mm_vwc, precip_mm)) %>% 
    mutate(precip_mm = replace_na(precip_mm, 0)) %>% 
    select(-precip_mm_vwc)
  
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
  
  # Look at pyran data for March-May to make sure there is a daily cycle
  met_vwc %>% 
    filter(month(datetime) == 3 & mday(datetime) > 15) %>% 
    ggplot(aes(x = datetime, y = pyranometer_watt_m2)) +
    facet_wrap(~year(datetime), scales = "free") +
    geom_line()
  # They look reasonable to me (e.g., no obvious blockage by snow), except that
  # we are missing VWC MET data from 3/11/20 01:00 (first missing reading) to 3/25/20 14:00 (last missing reading)
  # I will deal with these missing data below when we calculate daily averages
  
  # met_vwc %>% 
  #   filter(year(date) == 2020 & month(datetime) == 3) %>% 
  #   ggplot(aes(x = datetime, y = pyranometer_watt_m2)) +
  #   geom_line() 
  

# Stowe Mountain Resort water withdrawals
# MISSING 2020 data, but won't be using it in SOM  
# Let's assume an NA means 0 here, so replace NAs with 0  
  withdraw <- read_csv("data/SMR withdrawal summary.csv") %>% 
    mutate(year = ifelse(month >= 10, wyear - 1, wyear)) %>% 
    mutate_at(vars(starts_with("use")),
              list(~ replace_na(., 0)))
  

# CALCULATE ADDITIONAL VARIABLES ----
# Cumulative runoff
  runoff <- 
    runoff %>% 
    arrange(site, date) %>% 
    group_by(site, wyear) %>% 
    mutate(q_mm_cum = cumsum(q_mm)) %>% 
    ungroup()

  # Let's look at these curves to make sure they make sense
  runoff %>% 
    ggplot(aes(x = doy_wyear, y = q_mm_cum, group = site, color = site)) +
    facet_wrap(~wyear, ncol = 4) +
    geom_path()
  
# Cumulative precip
  met <-
    met %>% 
    mutate(wyear = ifelse(month(date) >= 10, year(date) + 1, year(date))) %>% 
    arrange(date) %>% 
    group_by(wyear) %>% 
    mutate(precip_mm_cum = cumsum(precip_mm)) %>% 
    ungroup()
  
  # Let's look at these curves to make sure they make sense
  met %>% 
    group_by(wyear) %>% 
    mutate(doy_wyear = row_number()) %>% 
    ungroup() %>% 
    ggplot(aes(x = doy_wyear, y = precip_mm_cum)) +
    facet_wrap(~wyear, ncol = 4) +
    geom_path()
  
  # How do cum. precip curves map onto cum. runoff curves
  # Normalize between 0 and 100%
  water <-
    full_join(runoff, met)
  
  # Calc annual q_mm and precip_mm totals
  water_ann_totals <- 
    water %>% 
    # Remove NA values on Feb 29 in non-leap years
    filter(!is.na(q_mm)) %>% 
    group_by(site, wyear) %>% 
    summarize(q_mm_tot = sum(q_mm),
              precip_mm_tot = sum(precip_mm)) %>% 
    ungroup()
  
  # Calc percent contribution to annual total
  water <-
    left_join(water, water_ann_totals) %>% 
    mutate(q_mm_per = q_mm_cum/q_mm_tot*100,
           precip_mm_per = precip_mm_cum/precip_mm_tot*100) %>% 
    select(-c(q_mm_tot, precip_mm_tot))
  
  # Graph
  water %>% 
    # pivot_longer(cols = c(q_mm_per, precip_mm_per), names_to = "var", values_to = "percent") %>% 
    pivot_wider(names_from = site, values_from = c(q_mm_per, precip_mm_per)) %>% 
    pivot_longer(cols = c(q_mm_per_RB, q_mm_per_WB, precip_mm_per_WB), names_to = "var", values_to = "value") %>% 
    ggplot(aes(x = doy_wyear, y = value, group = var, color = var)) +
    facet_wrap(~wyear) +
    geom_path()
  
# Calculate daily temp means for VWC data
  met_vwc_daily_temp <-
    met_vwc %>% 
    select(datetime, date, airtemp_c) %>% 
    group_by(date) %>% 
    summarize(airtemp_c = round(mean(airtemp_c, na.rm = F), 1)) %>% 
    rename(temp_avg_vwc = airtemp_c) %>% 
    ungroup()
  
  # Fill 3/11/20 to 3/25/20 using prediction from model with GHCN data
  # Note: This isn't perfect because daily temp avgs for GHCN ended in 2005, so I had to calculate the daily temp avg
  # by taking the mean of the recorded minimum and maximum temps each day
  # Compare daily temp. means from GHCN and VWC
    compareTemp <- 
      full_join(met, met_vwc_daily_temp)
    
    compareTemp %>% 
      # Focus on March-May
      filter(month(date) >= 3 & month(date) <= 5) %>% 
      ggplot(aes(x = temp_avg, y = temp_avg_vwc)) +
      geom_point() +
      geom_abline(slope = 1) +
      geom_smooth(method = "lm")
    
    lm.temp <- lm(temp_avg_vwc ~ temp_avg, data = compareTemp %>% filter(month(date) >= 3 & month(date) <= 5))
    # Extract intercept and slope estimates
    yint <- coef(lm.temp)["(Intercept)"]
    slope <- coef(lm.temp)["temp_avg"]
    
    # Replace missing values using the coefficients from model
    met_vwc_daily_temp <-
      met_vwc_daily_temp %>% 
      full_join(met %>% select(date, temp_avg), by = "date") %>% 
      arrange(date) %>% 
      mutate(temp_avg_vwc = ifelse(date >= ymd("2020-03-11") & date <= ymd("2020-03-25"), slope*temp_avg+yint, temp_avg_vwc))

# Calculate cumulative daily pyran (watt/m2) values
  met_vwc_daily_pyr <- 
    met_vwc %>% 
    select(datetime, date, pyranometer_watt_m2) %>% 
    group_by(date) %>% 
    summarize(pyra_wattm2_dailyTot = sum(pyranometer_watt_m2))  
  
  # Fill 3/11/20 to 3/25/20 with mean from 5 days prior to 3/11 and 5 days after 3/25
  pyran_3_20_mean <-
    met_vwc_daily_pyr %>% 
    filter(date >= ymd("2020-03-11") - days(5) & date <= ymd("2020-03-25") + days(5)) %>% 
    filter(date != ymd("2020-03-11") & date != ymd("2020-03-25")) %>% 
    summarize(pyra_mean = mean(pyra_wattm2_dailyTot)) %>% 
    pull()
  
  met_vwc_daily_pyr <-
    met_vwc_daily_pyr %>% 
    # Removing existing values for these daytes
    filter(date != ymd("2020-03-11") & date != ymd("2020-03-25")) %>% 
    # Add the missing rows
    bind_rows(tibble(date = seq(as.Date('2020-03-11'), as.Date('2020-03-25'), by = "1 days"),
                     pyra_wattm2_dailyTot = NA)) %>% 
    # Fill the missing values with pyran_3_20_mean
    mutate(pyra_wattm2_dailyTot = ifelse(date >= ymd("2020-03-11") & date <= ymd("2020-03-25"), pyran_3_20_mean, pyra_wattm2_dailyTot)) %>% 
    arrange(date)
  
# Total SMR withdrawals & cumulative total withdrawals
  # MISSING 2020 data, but won't be using it in SOM
  withdraw <- 
    withdraw %>% 
    rowwise() %>% 
    mutate(use_total_mm = sum(use_snowmaking_mm, use_diverted_mm, use_golf_mm, na.rm = T)) %>% 
    group_by(wyear) %>% 
    mutate(use_total_mm_cum = cumsum(use_total_mm)) %>% 
    select(wyear, month, year, everything())
  

# CALCULATE ANTECEDENT VARIABLES ----
  # 3-day precip totals (includes the day of observation and 2 days before)
  water <- setDT(water)[, precip_mm_3d := frollsum(precip_mm, n = 3, align = "right", fill = NA, na.rm = F)]
  # 3-day temp average
  met_vwc_daily_temp <- setDT(met_vwc_daily_temp)[, temp_avg_vwc_3d := frollmean(temp_avg_vwc, n = 3, align = "right", fill = NA, na.rm = F)]
  
 
  
# JOIN DFs INTO ONE DF
alldat <- full_join(water, snow) %>% 
  # full_join(met) %>% 
  full_join(met_vwc_daily_temp) %>% 
  full_join(met_vwc_daily_pyr) %>% 
  mutate(year = year(date),
        month = month(date)) %>% 
  full_join(withdraw) %>% 
  # Divide monthly use volumes by number of days each month
  # mutate(mday = mday(date)) %>% 
  group_by(year, month) %>% 
  mutate(use_snow_mm_daily = use_snowmaking_mm/max(mday(date)),
         use_total_mm_daily = use_total_mm/max(mday(date))) %>% 
  ungroup() %>% 
  select(-c(year, month, use_snowmaking_mm, use_diverted_mm, use_golf_mm, use_total_mm)) %>% 
  # Remove any data after 2020-09-30
  filter(date <= ymd("2020-09-30"))

# Look at missing data for spring months
# I we're missing 2020 withdrawal data
missing <-
  alldat %>%
  filter(doy_wyear >= 170 & doy_wyear <= 245) %>%
  select(-c(wyear, doy_wyear, q_mm, q_mm_cum, q_mm_per, precip_mm_per, temp_max, temp_min, temp_avg)) %>%
  filter_all(any_vars(is.na(.)))

# Write to CSV
write_csv(alldat, "data/alldata_compiled.csv")
