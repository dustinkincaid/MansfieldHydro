# Create plots and results from SOM analysis of Mt Mansfield paired watersheds for Shanley et al. Hydro. Proc. MS

# Load packages
  library("tidyverse")
  library("lubridate")
  library("zoo")
  library("ggpmisc")
  
  
# Read in data
  # Calculated event metrics for each Mansfield watershed as calculated in scripts/compileData.R
  mets <- read_csv("data/alldata_compiled.csv")
  
  # SOM results from 2021-01-12 (see SOManalysis.Rmd)
  ResultsDF <- read_csv("data/somResults/2021-01-12/Results_bothSites.csv")
  clustAssignmDF <- read_csv("data/somResults/2021-01-12/ClustAssign_bothSites.csv")
  
# Convert the mets df to the df myData, which was used in the SOM analysis 
  # First we're going to filter the data to focus on March-May dynamics
    # We want to avoid incorporating too much of the climate pattern, which would wash out this spring signal
    # We'll want to play with the size of this window
    # For reference water year days-of-year: Mar 15 = 166; Apr 1 = 183; May 1 = 213; May 15 = 227; May 31 = 243
    mets_sub <- mets %>%
      filter(doy_wyear >= 170 & doy_wyear <= 245)  
  
  # For the analysis we only need one watershed/site of data, because the drivers we use are the same for both sites
    mets_sub_1site <- mets_sub %>% 
      filter(site == "WB")
  
  # Select vars to keep Step 1
    # List the ID and response variables that will NOT be used in the analysis
    drop.info <- names(mets_sub_1site %>% 
                         select(# Remove the non-numerical ID/INFO
                                site, wyear, date, doy_wyear,
                                # Remove response variables
                                q_mm, q_mm_cum, q_mm_per,
                                # Remove other variables we know we won't want for SOM, but will want for graphing results later
                                precip_mm_cum, precip_mm_per))
    
    # List the other variables that will not be used in this analysis
    drop.vars <- names(mets_sub_1site %>% 
                         select(temp_max, temp_min,
                                # We'll keep the average temps measured at VWC/FEMC station, so drop those from Morrisville/Stowe airport
                                temp_avg,
                                # Let's just use the cumulative total volume of water diverted from the stream by the resort
                                use_snow_mm_daily, use_total_mm_daily,
                                # After looking at an analysis, I'm going to drop the use categories altogether,
                                use_total_mm_cum))  
    
  # Create myData
    myData <- mets_sub_1site %>% 
      # Remove the vars listed above
      select(-one_of(drop.info, drop.vars)) %>% 
      # Keep only complete observations/rows (no NAs in any of the columns)
      na.omit()

  
# Choose the best SOM run based on non-parametric F-stat and quantization error
  # Set coefficient for second axis on plot below
  # Note: this might not be perfect and may need to adjust
  coeff <- median(ResultsDF$npF)/max(ResultsDF$QE)
  
  ResultsDF %>% 
    # Sort by npF (max to min)
    arrange(desc(npF)) %>% 
    # Only show top 50% of choices
    slice(1:(nrow(.)/2)) %>%
    # Create lattice config ID (rows x cols)
    mutate(latID = paste(paste(rows, "x", cols, sep = ""), "_", Clusters, "cl", "_", Run, sep = "")) %>% 
    # Plot
    ggplot(aes(x = reorder(latID, -npF))) +
    geom_bar(aes(y = npF, fill = "npF"), stat= "identity") +
    geom_line(aes(y = QE * coeff, group = 1, color = "QE")) +
    geom_point(aes(y = QE * coeff, group = 1, color = "QE"), size = 2) +
    scale_y_continuous(
      name = "npF",
      sec.axis = sec_axis(~. / coeff, name = "QE")
    ) +
    scale_fill_manual(values = "gray70") +
    scale_color_manual(values = "blue") +
    xlab("Lattice config. & clusters (rows x cols _ clusters_run)") +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.spacing = unit(-0.5, "lines"),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))
  
# Choose the best run from each high performing (top 50%) cluster #  
  # Here I manually chose the best run for each cluster of interest (3, 4, 5)
  best_summ_top_man <- 
    ResultsDF %>% 
    select(Run, rows, cols, Nodes, Clusters, npF, QE) %>% 
    # Choose run numbers here
    filter(Run %in% c(9, 17, 22)) %>% 
    # Sort by npF (max to min)
    arrange(desc(npF)) %>% 
    ungroup() %>% 
    mutate_at(vars(c(npF, QE)),
              .funs = ~round(., 3))
  best_summ_top_man
  
# We're going to focus on the SOM run w/ 4 clusters, because it makes the most sense
  # Set up dataframe for 4 clusters
  best_summ_top_4cl <-
    best_summ_top_man %>% 
    filter(Clusters == 4) 
  
  best_run_4cl = best_summ_top_4cl$Run
  
  datWithCluster_4cl <- clustAssignmDF %>% 
    # Parse columns into correct type
    type_convert() %>% 
    filter(Run == best_run_4cl) %>% 
    # Pivot to longer format
    pivot_longer(cols = obs1:ncol(.), names_to = "obs", values_to = "cluster") %>% 
    select(cluster) %>% 
    # Bind cluster ID to myData
    bind_cols(myData) %>% 
    # Join to original data to get event IDs
    left_join(mets_sub) %>% 
    # Drop all the independent variables you didn't use in the SOM (copy from myData above); 
    # but KEEP the INFO vars
    select(-one_of(drop.vars)) %>%
    mutate(SOMversion = "4cl") %>% 
    # Arrange columns
    select(SOMversion, site, wyear, date, doy_wyear, cluster, q_mm, q_mm_cum, everything())
  
# Set themes for plots
  theme1 <- theme_bw() +
            theme(panel.background = element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank())

  theme2 <- theme_classic() +
            theme(axis.text = element_text(size = 11),
                  axis.title = element_text(size = 12),
                  axis.title.x = element_text(margin=margin(5,0,0,0)),
                  axis.title.y = element_text(margin=margin(0,5,0,0)),
                  legend.title = element_text(size = 9),
                  legend.text = element_text(size = 9))
  
# Plot z-scores of predictor variables by cluster #
  datWithCluster_4cl %>%     
    # Remove the drop.info vars from above
    select(-one_of(drop.info)) %>% 
    # Rename clusters
    mutate(cluster = paste("Cluster", cluster, sep = " ")) %>% 
    # Calculate z-scores for each independent variable
    mutate_at(vars(c(precip_mm:pyra_wattm2_dailyTot)),
            .funs = list(~ (. - mean(., na.rm = T)) / sd(., na.rm = T))) %>%
    pivot_longer(cols = precip_mm:pyra_wattm2_dailyTot, names_to = "var", values_to = "value") %>% 
    group_by(cluster, var) %>% 
    summarize(median = median(value, na.rm = T),
              q2 = quantile(value, 1/4, na.rm = T),
              q3 = quantile(value, 3/4, na.rm = T)) %>%   
    # Order the vars n var
    # mutate(var = factor(var, levels = c("rain_int_mmPERmin_mean", "rain_int_mmPERmin_max", "rain_event_hrs", "rain_event_total_mm",
    #                                     "q_4d", "time_sinceLastEvent", "API_4d", "VWC_pre_wet_15cm", "VWC_pre_dry_30cm", "VWC_pre_dry_15cm", "SoilTemp_pre_wet_15cm",
    #                                     "solarRad_4d", "PET_mmHR", "diff_airT_soilT", "DOY"))) %>%
    ggplot(aes(x = var, y = median)) +
      # Must use scales = "free" when using the reordering functions
      facet_wrap(~cluster, ncol = 2) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = q2, ymax = q3), width = 0.2, position = position_dodge(0.9)) +
      # scale_x_reordered() +
      ylab("Median z-scores") +
      coord_flip() +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.title.y = element_blank())
  
  ggsave("plots/somResults_clusterZscores.pdf", width = 6, height = 4, units = "in", dpi = 150)
  
# Plot how the clusters map on to the cumulative runoff curves
  datWithCluster_4cl %>% 
    mutate(cluster = factor(cluster)) %>% 
    ggplot(aes(x = date)) +
    facet_wrap(~wyear, ncol = 4, scales = "free_x") +
    geom_rect(aes(xmin = date, xmax = date,
                  ymin = -Inf, ymax = Inf,
              color = cluster, fill = cluster)) +  
    geom_path(aes(y = q_mm_cum, group = site, linetype = site)) +
    scale_linetype_manual(name = "Watershed",
                          values = c("solid", "dashed")) +
    ylab("Runoff (mm)") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_color_manual(name = "Cluster",
                       values = c("#56B4E9", "#009E73", "#E69F00", "#0072B2", "#CC79A7")) +
    scale_fill_manual(name = "Cluster",
                      values = c("#56B4E9", "#009E73", "#E69F00", "#0072B2", "#CC79A7")) +  
    # scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#CC79A7")) +
    # scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#CC79A7")) +  
    theme1 +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12))
  
  ggsave("plots/somResults_clusterMap_vs_runoff.pdf", width = 7.5, height = 8, units = "in", dpi = 150)
  
# Calculate yield differences in spring and at the end of the water year
  yields_spr <- 
    mets_sub %>% 
    group_by(site, wyear) %>% 
    do(tail(., 1)) %>% 
    select(site, wyear, q_mm_cum) %>% 
    pivot_wider(names_from = site, values_from = q_mm_cum) %>% 
    mutate(WBminusRB_abs_spr = WB - RB,
           WBminusRB_per_spr = (WB - RB)/RB) %>% 
    select(-c(WB, RB))

  yields_end <- 
    mets %>% 
    group_by(site, wyear) %>% 
    do(tail(., 1)) %>% 
    filter(!is.na(site)) %>% 
    select(site, wyear, q_mm_cum) %>% 
    pivot_wider(names_from = site, values_from = q_mm_cum) %>% 
    mutate(WBminusRB_abs_end = WB - RB,
           WBminusRB_per_end = (WB - RB)/RB) %>% 
    select(-c(WB, RB))
  
  yields_comp <- 
    full_join(yields_spr, yields_end)
  
  
# For many years, the divergence in runoff yields occurs after switch from colder to warmer days (from cluster 1 to 2)  
# Does focusing on the warmer period yield any insight?  
  # Here I use a running mode to identify when the switch occurs from predominantly cluster 1 to 2 or 4 (cold to warm transition)  
  # Transition is plotted as the vertical black line  
  # Note: the running mode (width = 3) worked well for all years but 2017; I shifted that one to first occurrence of cluster 2

  # Let's try identifying where the cluster time series switches from predominantly cold to warm
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  mode_4cl <- 
    datWithCluster_4cl %>% 
    filter(site == "WB") %>% 
    group_by(wyear) %>% 
    mutate(cluster_mode = rollapply(data = cluster,
                                    width = 3,
                                    FUN = Mode,
                                    align = "center",
                                    fill = NA))
  
  mode_4cl_first <-
    mode_4cl %>%
    # Slice from the first occurrence of 2 as the cluster mode to the end of the time series
    group_by(wyear) %>%
    slice(ifelse(wyear == 2017, min(which(cluster %in% c(2, 4))), min(which(cluster_mode %in% c(2, 4))))) %>%
    select(wyear, date)
  
  
  datWithCluster_4cl %>%
    mutate(cluster = factor(cluster)) %>%
    ggplot(aes(x = date)) +
    facet_wrap(~wyear, ncol = 4, scales = "free_x") +
    geom_rect(aes(xmin = date, xmax = date,
                  ymin = -Inf, ymax = Inf,
              color = cluster, fill = cluster)) +
    geom_vline(data = mode_4cl_first, aes(xintercept = date), size = 0.9) +
    geom_path(aes(y = q_mm_cum, group = site, linetype = site)) +
    scale_linetype_manual(name = "Watershed",
                          values = c("solid", "dashed")) +
    ylab("Runoff (mm)") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_color_manual(name = "Cluster",
                       values = c("#56B4E9", "#009E73", "#E69F00", "#0072B2", "#CC79A7")) +
    scale_fill_manual(name = "Cluster",
                      values = c("#56B4E9", "#009E73", "#E69F00", "#0072B2", "#CC79A7")) +  
    # scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#CC79A7")) +
    # scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#CC79A7")) +  
    theme1 +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12))
  
  # ggsave("plots/somResults_clusterMap_vs_runoff.pdf", width = 7.5, height = 8, units = "in", dpi = 150)
  
# Plot runoff diff. ~ total precip. after last cold day
  lastcold <-
    datWithCluster_4cl %>%
    filter(site == "WB") %>%
    group_by(wyear) %>%
    # Slice after the last occurrence of 1 as the cluster mode to the end of the time series
    group_by(wyear) %>%
    slice(max(which(cluster == 1)):nrow(.))
  
  lastcold_precip <-
    lastcold %>%
    group_by(wyear) %>%
    summarize(precip_mm_lastcold = sum(precip_mm)) %>%
    full_join(yields_comp)
  
  # Linear regression excluding wyear 2012
  lm12 <- lm(WBminusRB_abs_spr ~ precip_mm_lastcold, data = lastcold_precip %>% filter(wyear != 2012))
  summary(lm12)
  r2_lm12 <- round(summary(lm12)$r.squared, 2)
  p_lm12 <- round(summary(lm12)$coefficients[2,4], 3)
  noquote(paste0("R2 = ", r2_lm12))
  noquote(paste0("p = ", p_lm12))
  label_r2 <- as.character(expression(italic(R)^2~"="~0.41)) 
  label_p <- as.character(expression(italic(p)~"="~0.004))
  
  # Plot 2012, but don't include it in the linear regression
  lastcold_precip %>%
    # filter(wyear != 2012) %>% 
    ggplot(aes(x = precip_mm_lastcold, y = WBminusRB_abs_spr)) +
    geom_smooth(data = lastcold_precip %>% filter(wyear != 2012), method = "lm", se = FALSE, color = "black") +
    geom_point(shape = 1, size = 2) +
    geom_text(aes(label = wyear), hjust = -0.3, vjust = -0.3, size = 2.5) +
    scale_x_continuous(limits = c(0, 210), breaks = seq(0, 200, by = 50)) +
    xlab("Total precip. after last cold day (cluster 1; mm)") +
    ylab("Runoff difference in spring (mm)") +   
    theme_classic() +
    annotate("text", x=15, y=300, size=3.5, label=label_r2, parse=TRUE) +
    annotate("text", x=15, y=280, size=3.5, label=label_p, parse=TRUE) +
    theme(axis.title.x = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.text = element_text(size = 11))
  
  ggsave("plots/somResults_predictYieldDiff.pdf", width = 5, height = 4, units = "in", dpi = 150)
  
  # plot.margin = unit(c(1, 2, 1, 1), "cm"))

# Per the reviewer's comment, let's just look at runoff differential between the last cold day & end of spring (wday = 245)
# RESULT: linear regression fits were worse (R2 = 0.23 vs 0.41)  
  # Filter that period for both sites
  lastcold_2 <-
    datWithCluster_4cl %>%
    # filter(site == "WB") %>%
    # Slice after the last occurrence of 1 as the cluster mode to the end of the time series
    group_by(site, wyear) %>%
    slice(max(which(cluster == 1)):nrow(.))
  
  # Calculate the runoff differential for that period
  lastcold_q <-
    lastcold_2 %>% 
    group_by(site, wyear) %>%
    summarize(q_mm_lastcold = sum(q_mm)) %>% 
    pivot_wider(names_from = site, values_from = q_mm_lastcold) %>% 
    mutate(WBminusRB_abs_lastcold = WB - RB) %>% 
    select(-c(WB, RB))
  
  # Join lastcold_q to lastcold_precip
  lastcold_all <-
    full_join(lastcold_precip, lastcold_q, by = "wyear")
  
  # Linear regression excluding wyear 2012
  lm_lc <- lm(WBminusRB_abs_lastcold ~ precip_mm_lastcold, data = lastcold_all %>% filter(wyear != 2012))
  summary(lm_lc)
  r2_lmlc <- round(summary(lm_lc)$r.squared, 2)
  p_lmlc <- round(summary(lm_lc)$coefficients[2,4], 3)
  noquote(paste0("R2 = ", r2_lmlc))
  noquote(paste0("p = ", p_lmlc))
  label_r2_lc <- as.character(expression(italic(R)^2~"="~0.23)) 
  label_p_lc <- as.character(expression(italic(p)~"="~0.05))
  
  # Plot 2012, but don't include it in the linear regression
  lastcold_all %>%
    # filter(wyear != 2012) %>% 
    ggplot(aes(x = precip_mm_lastcold, y = WBminusRB_abs_lastcold)) +
    geom_smooth(data = lastcold_all %>% filter(wyear != 2012), method = "lm", se = FALSE, color = "black") +
    geom_point(shape = 1, size = 2) +
    geom_text(aes(label = wyear), hjust = -0.3, vjust = -0.3, size = 2.5) +
    scale_x_continuous(limits = c(0, 210), breaks = seq(0, 200, by = 50)) +
    xlab("Total precip. after last cold day (cluster 1; mm)") +
    ylab("Runoff difference in for melt period (mm)") +   
    theme_classic() +
    annotate("text", x=15, y=200, size=3.5, label=label_r2_lc, parse=TRUE) +
    annotate("text", x=15, y=180, size=3.5, label=label_p_lc, parse=TRUE) +
    theme(axis.title.x = element_text(size = 12, margin = margin(t = 5, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.text = element_text(size = 11))
  
  ggsave("plots/somResults_predictYieldDiff.pdf", width = 5, height = 4, units = "in", dpi = 150)
  
  
# Quick calcs for MS
  # Snow depth metrics in cm
  mets %>% 
    filter(snow_dep_m > 0) %>% 
    summarize(mean = mean(snow_dep_m*100, na.rm = T),
              sd = sd(snow_dep_m*100, na.rm = T))
  
  # Peak snow depth day
  mets %>% 
    filter(snow_dep_m > 0) %>% 
    group_by(wyear) %>% 
    slice(which.max(snow_dep_m):nrow(.)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    summarize(mean = mean(doy_wyear),
              sd = sd(doy_wyear))
  
  
  # Plot max snow depth against year
  maxSnow <- mets %>% 
    filter(snow_dep_m > 0) %>% 
    group_by(wyear) %>% 
    slice(which.max(snow_dep_m):nrow(.)) %>% 
    slice(1) %>% 
    ungroup()
  
  maxSnow %>% 
    ggplot(aes(x = wyear, y = snow_dep_m)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(data = maxSnow %>% filter(wyear >= 2008), method = "lm", se = FALSE) +
    theme_classic()

  lm_all <- lm(snow_dep_m ~ wyear, data = maxSnow)
  lm_later <- lm(snow_dep_m ~ wyear, data = maxSnow %>% filter(wyear >= 2008))
  summary(lm_all)
  summary(lm_later) 
  
  mets %>% 
    filter(wyear %in% c(2001, 2017)) %>% 
    ggplot(aes(x = doy_wyear, y = snow_dep_m), group = wyear) +
    geom_path(aes(color = factor(wyear))) +
    theme_classic()
  
# Looking into relationship between spring runoff differential and max snow depth
  # Calculate max snow depth each year
  snow_depth_max <-
    mets %>% 
    filter(site == "WB") %>% 
    group_by(wyear) %>% 
    summarize(snow_depth_max = max(snow_dep_m, na.rm = TRUE))
  
  # That plot doesn't quite look like Fig. S7 that Beverley made. Maybe different date in May?
  # For reference water year days-of-year: Mar 15 = 166; Apr 1 = 183; May 1 = 213; May 15 = 227; May 31 = 243
  mets_sub_may1 <- mets %>%
    filter(doy_wyear >= 170 & doy_wyear <= 213) 
  
  mets_sub_may15 <- mets %>%
    filter(doy_wyear >= 170 & doy_wyear <= 227)  
  
  mets_sub_may31 <- mets %>%
    filter(doy_wyear >= 170 & doy_wyear <= 243)  
  
  yields_diff_may1 <- 
    mets_sub_may1 %>% 
    group_by(site, wyear) %>% 
    do(tail(., 1)) %>% 
    select(site, wyear, q_mm_cum) %>% 
    pivot_wider(names_from = site, values_from = q_mm_cum) %>% 
    mutate(WBminusRB_abs_spr = WB - RB,
           WBminusRB_per_spr = (WB - RB)/RB) %>% 
    select(-c(WB, RB)) 
  
  yields_diff_may15 <- 
    mets_sub_may15 %>% 
    group_by(site, wyear) %>% 
    do(tail(., 1)) %>% 
    select(site, wyear, q_mm_cum) %>% 
    pivot_wider(names_from = site, values_from = q_mm_cum) %>% 
    mutate(WBminusRB_abs_spr = WB - RB,
           WBminusRB_per_spr = (WB - RB)/RB) %>% 
    select(-c(WB, RB)) 
  
  yields_diff_may31 <- 
    mets_sub_may31 %>% 
    group_by(site, wyear) %>% 
    do(tail(., 1)) %>% 
    select(site, wyear, q_mm_cum) %>% 
    pivot_wider(names_from = site, values_from = q_mm_cum) %>% 
    mutate(WBminusRB_abs_spr = WB - RB,
           WBminusRB_per_spr = (WB - RB)/RB) %>% 
    select(-c(WB, RB))   
  
  # Plot runoff differential on May 1
  full_join(snow_depth_max, yields_diff_may1, by = "wyear") %>% 
    ggplot(aes(x = snow_depth_max, y = WBminusRB_abs_spr)) + 
    geom_point(shape = 1) +
    geom_text(aes(label = wyear), hjust = -0.25, vjust = 0, size = 2) +
    theme_classic()
  
  # Plot runoff differential on May 15
  full_join(snow_depth_max, yields_diff_may15, by = "wyear") %>% 
    ggplot(aes(x = snow_depth_max, y = WBminusRB_abs_spr)) + 
    geom_point(shape = 1) +
    geom_text(aes(label = wyear), hjust = -0.25, vjust = 0, size = 2) +
    theme_classic()
  
  # Plot runoff differential on May 31
  full_join(snow_depth_max, yields_diff_may31, by = "wyear") %>% 
    ggplot(aes(x = snow_depth_max, y = WBminusRB_abs_spr)) + 
    geom_point(shape = 1) +
    geom_text(aes(label = wyear), hjust = -0.25, vjust = 0, size = 2) +
    theme_classic() 
  
  # None of these quite match either; maybe it's just the difference between the runoff that occurred just in May?
  mets_sub_may <- mets %>%
    filter(doy_wyear >= 213 & doy_wyear <= 243) %>% 
    group_by(site, wyear) %>% 
    mutate(q_mm_cum_may = cumsum(q_mm))
  
  yields_diff_may <- 
    mets_sub_may %>% 
    group_by(site, wyear) %>% 
    do(tail(., 1)) %>% 
    select(site, wyear, q_mm_cum_may) %>% 
    pivot_wider(names_from = site, values_from = q_mm_cum_may) %>% 
    mutate(WBminusRB_abs_may = WB - RB,
           WBminusRB_per_may = (WB - RB)/RB) %>% 
    select(-c(WB, RB))
  
  # This is the correct plot!
  full_join(snow_depth_max, yields_diff_may, by = "wyear") %>% 
    ggplot(aes(x = snow_depth_max, y = WBminusRB_abs_may)) + 
    geom_point(shape = 1) +
    geom_text(aes(label = wyear), hjust = -0.25, vjust = 0, size = 2) +
    theme_classic()
  
  # Now try coloring the points by various variables
    # First create new df
    df_new <-
      full_join(snow_depth_max, yields_diff_may, by = "wyear") %>% 
      full_join(yields_comp, by = 'wyear') %>% 
      full_join(lastcold_precip, by = c("wyear", "WBminusRB_abs_spr", "WBminusRB_per_spr", "WBminusRB_abs_end", "WBminusRB_per_end")) 
  
    # Plot by precip total after last cold period
    df_new %>% 
      ggplot(aes(x = snow_depth_max, y = WBminusRB_abs_may)) + 
      geom_point(aes(size = precip_mm_lastcold, color = precip_mm_lastcold)) +
      geom_text(aes(label = wyear), hjust = -0.25, vjust = 0, size = 4) +
      theme_classic()
    
    # Maybe it's the # of really rainy days i.e., cluster 4 in May?
    num_clusters <-
      datWithCluster_4cl %>% 
      filter(site == "WB") %>% 
      filter(month(date) == 5) %>% 
      group_by(wyear) %>% 
      count(cluster) %>% 
      mutate(clusterID = paste("clust", cluster, sep = "_")) %>% 
      select(-cluster) %>% 
      pivot_wider(names_from = clusterID, values_from = n) %>% 
      mutate_at(vars(starts_with("clust")),
                list(~ replace_na(., 0)))
    
    # Add to df_new
    df_new <-
      full_join(df_new, num_clusters, by = "wyear")
  
    # Plot by number of days in cluster 4 in May
    df_new %>% 
      ggplot(aes(x = snow_depth_max, y = WBminusRB_abs_may)) + 
      geom_point(aes(size = clust_4, color = clust_4)) +
      geom_text(aes(label = wyear), hjust = -0.25, vjust = 0, size = 4) +
      theme_classic()
    
    # I should try classifying some sort or rain intensity (eg. mm per day) and looking to see if that explains it?
    # But look just in May
    rain_int_3d <-
      datWithCluster_4cl %>% 
      filter(site == "WB") %>% 
      filter(month(date) == 5) %>% 
      # Rank daily precip for each year
      arrange(wyear, desc(precip_mm_3d)) %>% 
      group_by(wyear) %>% 
      mutate(precip_rank = row_number()) %>% 
      slice(1:1) %>% 
      select(wyear, precip_mm_3d)
    
    # Add to df_new
    df_new <-
      full_join(df_new, rain_int_3d, by = "wyear")    

    # Plot by max 3d precip
    df_new %>% 
      ggplot(aes(x = snow_depth_max, y = WBminusRB_abs_may)) + 
      geom_point(aes(size = precip_mm_3d, color = precip_mm_3d)) +
      geom_text(aes(label = wyear), hjust = -0.25, vjust = 0, size = 4) +
      theme_classic()    
  