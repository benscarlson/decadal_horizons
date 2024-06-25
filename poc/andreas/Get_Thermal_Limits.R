library(tidyverse)
library(furrr)

# function to compute the limits
get_niche_limits <- function(species_ranges, temperature_matrix){
  
  # first: first month in the historical period
  # last: last month in the historical period
  
  
  data <- temperature_matrix %>% 
    filter(world_id %in% species_ranges) %>% 
    select(-world_id) %>% 
    na.omit()
  
  # when the range don't overlap with the climate data, return NA
  if(nrow(data) == 0) return(tibble(niche_max = NA, niche_min = NA))
  
  means <- apply(data, 1, mean)
  sds <- apply(data, 1, sd) * 3
  
  upper_limit <- means + sds
  lower_limit <- means - sds
  
  upper_outliers <- sweep(data ,1, upper_limit)
  lower_outliers <- sweep(data ,1, lower_limit)
  
  data[upper_outliers > 0] <- NA
  data[lower_outliers < 0] <- NA
  
  row_max <- apply(data, 1, max, na.rm = T)
  row_min <- apply(data, 1, min, na.rm = T)
  
  row_max_mean <- mean(row_max)
  row_max_sd <- sd(row_max) * 3
  
  row_min_mean <- mean(row_min)
  row_min_sd <- sd(row_min) * 3
  
  if(!is.na(row_max_sd)){
    
    row_max_upper <- row_max_mean + row_max_sd
    row_max_lower <- row_max_mean - row_max_sd
    
    row_min_upper <- row_min_mean + row_min_sd
    row_min_lower <- row_min_mean - row_min_sd    
    
    pre_max <- row_max[which(row_max <= row_max_upper & row_max >= row_max_lower)]
    pre_min <- row_min[which(row_min <= row_min_upper & row_min >= row_min_lower)]
    
    niche_max <- max(pre_max)
    niche_min <- min(pre_min)
    
    res <- data.frame(niche_max,niche_min)
    
  } else {
    
    niche_max <- apply(data, 1, max, na.rm = T)
    niche_min <- apply(data, 1, min, na.rm = T)
    
    res <- data.frame(niche_max,niche_min)
    
  }
  
  return(as_tibble(res))
  
}


# load temperature matrix
temp_matrix <- readRDS("data/tas_Amon_EC-Earth3_dcppB-forecast_s2023-r1i4p1f1.rds")
spp_data <- readRDS("data/Amphibians.rds")

# run
plan("multisession", workers = availableCores())

res <- future_map_dfr(spp_data, ~ get_niche_limits(.x, temp_matrix), .id = "species", .progress = T)

res <- na.omit(res)

res

