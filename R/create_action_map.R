# Define a function to create the action map
create_action_map <- function(
    
  # Define function variables ----
  dir,
  island_data,
  ram_year,
  time_period,
  tc_min,
  tc_max,
  probability_level_tc,
  tc_ann_min,
  tc_ann_max,
  tc_acc_min,
  tc_acc_max,
  ndmi_min,
  ndmi_max,
  probability_level_ndmi,
  ndmi_ann_min,
  ndmi_ann_max,
  ndmi_acc_min,
  ndmi_acc_max,
  prod_min,
  prod_max,
  probability_level_prod,
  prod_ann_min,
  prod_ann_max,
  prod_acc_min,
  prod_acc_max,
  slope_min,
  slope_max,
  elev_min,
  elev_max,
  aspect_num,
  twi_min,
  twi_max,
  eco_num
) {
  
  # Grab island variables
  island <- tolower(str_replace(island_data$Isl, "_", ""))
  gid2 <- island_data$GID2_ID
  res <- island_data$Res
  prod_met <- island_data$Prod_Met
  selected_time <- change_map[time_period]
  
  # Grab user selections about time and probability
  if (probability_level_tc == "All Pixels") {
    
    timeperiod_name_tc <- glue("posterior.mean_{selected_time}")
    
  } else { 
    
    probability_level_num_tc <- as.numeric(gsub("[^0-9.]", "", probability_level_tc))
    timeperiod_name_tc <- glue("posterior.mean_hpdi{probability_level_num_tc}_{selected_time}")
    
  }
  
  # Grab user selections about time and probability
  if (probability_level_ndmi == "All Pixels") {
    
    timeperiod_name_ndmi <- glue("posterior.mean_{selected_time}")
    
  } else { 
    
    probability_level_num_ndmi <- as.numeric(gsub("[^0-9.]", "", probability_level_ndmi))
    timeperiod_name_ndmi <- glue("posterior.mean_hpdi{probability_level_num_ndmi}_{selected_time}")
    
  }
  
  # Grab user selections about time and probability
  if (probability_level_prod == "All Pixels") {
    
    timeperiod_name_prod <- glue("posterior.mean_{selected_time}")
    
  } else { 
    
    probability_level_num_prod <- as.numeric(gsub("[^0-9.]", "", probability_level_prod))
    timeperiod_name_prod <- glue("posterior.mean_hpdi{probability_level_num_prod}_{selected_time}")
    
  }
  
  # Read in necessary datasets ----
  predicted_annual_tc <- rast(file.path(dir, glue("{island}_{gid2}/tcc/m1_tcc_{res}m_allcells_posterior.mean_annual_prediction.tif")))
  annualized_change_tc <- rast(file.path(dir, glue("{island}_{gid2}/tcc/m1_tcc_{res}m_annualized.tif")))
  accumulated_change_tc <- rast(file.path(dir, glue("{island}_{gid2}/tcc/m1_tcc_{res}m_accumulated.tif")))
  
  predicted_annual_ndmi <- rast(file.path(dir, glue("{island}_{gid2}/ndmi/m1_ndmi_{res}m_allcells_posterior.mean_annual_prediction.tif")))
  annualized_change_ndmi <- rast(file.path(dir, glue("{island}_{gid2}/ndmi/m1_ndmi_{res}m_annualized.tif")))
  accumulated_change_ndmi <- rast(file.path(dir, glue("{island}_{gid2}/ndmi/m1_ndmi_{res}m_accumulated.tif")))
  
  predicted_annual_prod_met <- rast(file.path(dir, glue("{island}_{gid2}/{prod_met}/m1_{prod_met}_{res}m_allcells_posterior.mean_annual_prediction.tif")))
  annualized_change_prod_met <- rast(file.path(dir, glue("{island}_{gid2}/{prod_met}/m1_{prod_met}_{res}m_annualized.tif")))
  accumulated_change_prod_met <- rast(file.path(dir, glue("{island}_{gid2}/{prod_met}/m1_{prod_met}_{res}m_accumulated.tif")))
  
  terrain <- rast(file.path(dir, glue("{island}_{gid2}/terrain/terrain_{res}m.tif")))
  terrain <- crop(terrain, predicted_annual_tc[[1]])
  
  # Handle TC rasters ----
  annualized_tc <- annualized_change_tc[[timeperiod_name_tc]]
  annualized_tc_all <- annualized_change_tc[[glue("posterior.mean_{selected_time}")]]
  accumulated_tc <- accumulated_change_tc[[timeperiod_name_tc]]
  accumulated_tc_all <- accumulated_change_tc[[glue("posterior.mean_{selected_time}")]]
  predicted_tc_year_layer <- predicted_annual_tc[[as.character(ram_year)]]
  
  # Filter based on user inputs & create sf objects
  # Predicted TC
  filtered_predicted_tc <- classify(predicted_tc_year_layer, 
                                    rcl = matrix(c(-Inf, tc_min, NA,
                                                   tc_max, Inf, NA),
                                                 ncol = 3, byrow = TRUE))
  
  filtered_predicted_tc_sf <- st_as_sf(as.polygons(filtered_predicted_tc, 
                                                   aggregate = FALSE))
  
  # Annualized TC
  # If the probability level is not inverse, filter normally
  if (grepl("Inverse", probability_level_tc) == FALSE)  {
    
    filtered_annualized_tc <- classify(annualized_tc, 
                                       rcl = matrix(c(-Inf, tc_ann_min, NA,
                                                      tc_ann_max, Inf, NA), 
                                                    ncol = 3, byrow = TRUE))
    
    filtered_annualized_tc_sf <- st_as_sf(as.polygons(filtered_annualized_tc, 
                                                      aggregate = FALSE, 
                                                      na.rm = FALSE))
    
  } else {
    
    # If the probability level is inverse, mask first for NOT significant values
    inverse_annualized_tc <- mask(annualized_tc_all, annualized_tc, inverse = TRUE)
    
    filtered_annualized_tc <- classify(inverse_annualized_tc, 
                                       rcl = matrix(c(-Inf, tc_ann_min, NA,
                                                      tc_ann_max, Inf, NA), 
                                                    ncol = 3, byrow = TRUE))
    
    filtered_annualized_tc_sf <- st_as_sf(as.polygons(filtered_annualized_tc, 
                                                      aggregate = FALSE, 
                                                      na.rm = FALSE))
    
  }
  
  # Accumulated TC
  # If the probability level is not inverse, filter normally
  if (grepl("Inverse", probability_level_tc) == FALSE)  {
    
    filtered_accumulated_tc <- classify(accumulated_tc, 
                                        rcl = matrix(c(-Inf, tc_ann_min, NA,
                                                       tc_ann_max, Inf, NA), 
                                                     ncol = 3, byrow = TRUE))
    
    filtered_accumulated_tc_sf <- st_as_sf(as.polygons(filtered_accumulated_tc, 
                                                       aggregate = FALSE, 
                                                       na.rm = FALSE))
    
  } else {
    
    # If the probability level is inverse, mask first for NOT significant values
    inverse_accumulated_tc <- mask(accumulated_tc_all, accumulated_tc, inverse = TRUE)
    
    filtered_accumulated_tc <- classify(inverse_accumulated_tc, 
                                        rcl = matrix(c(-Inf, tc_ann_min, NA,
                                                       tc_ann_max, Inf, NA), 
                                                     ncol = 3, byrow = TRUE))
    
    filtered_accumulated_tc_sf <- st_as_sf(as.polygons(filtered_accumulated_tc, 
                                                       aggregate = FALSE, 
                                                       na.rm = FALSE))
    
  }
  
  # Handle NDMI rasters ----
  annualized_ndmi <- annualized_change_ndmi[[timeperiod_name_ndmi]]
  annualized_ndmi_all <- annualized_change_ndmi[[glue("posterior.mean_{selected_time}")]]
  accumulated_ndmi <- accumulated_change_ndmi[[timeperiod_name_ndmi]]
  accumulated_ndmi_all <- accumulated_change_ndmi[[glue("posterior.mean_{selected_time}")]]
  predicted_ndmi_year_layer <- predicted_annual_ndmi[[as.character(ram_year)]]
  
  # Filter based on user inputs & create sf objects
  # Predicted NDMI
  filtered_predicted_ndmi <- classify(predicted_ndmi_year_layer, 
                                      rcl = matrix(c(-Inf, ndmi_min, NA,
                                                     ndmi_max, Inf, NA),
                                                   ncol = 3, byrow = TRUE))
  
  filtered_predicted_ndmi_sf <- st_as_sf(as.polygons(filtered_predicted_ndmi, 
                                                     aggregate = FALSE))
  
  # Annualized NDMI
  # If the probability level is not inverse, filter normally
  if (grepl("Inverse", probability_level_ndmi) == FALSE)  {
    
    filtered_annualized_ndmi <- classify(annualized_ndmi, 
                                         rcl = matrix(c(-Inf, ndmi_ann_min, NA,
                                                        ndmi_ann_max, Inf, NA), 
                                                      ncol = 3, byrow = TRUE))
    
    filtered_annualized_ndmi_sf <- st_as_sf(as.polygons(filtered_annualized_ndmi, 
                                                        aggregate = FALSE, 
                                                        na.rm = FALSE))
    
  } else {
    
    # If the probability level is inverse, mask first for NOT significant values
    inverse_annualized_ndmi <- mask(annualized_ndmi_all, annualized_ndmi, inverse = TRUE)
    
    filtered_annualized_ndmi <- classify(inverse_annualized_ndmi, 
                                         rcl = matrix(c(-Inf, ndmi_ann_min, NA,
                                                        ndmi_ann_max, Inf, NA), 
                                                      ncol = 3, byrow = TRUE))
    
    filtered_annualized_ndmi_sf <- st_as_sf(as.polygons(filtered_annualized_ndmi, 
                                                        aggregate = FALSE, 
                                                        na.rm = FALSE))
    
  }
  
  # Accumulated NDMI
  # If the probability level is not inverse, filter normally
  if (grepl("Inverse", probability_level_ndmi) == FALSE)  {
    
    filtered_accumulated_ndmi <- classify(accumulated_ndmi, 
                                          rcl = matrix(c(-Inf, ndmi_ann_min, NA,
                                                         ndmi_ann_max, Inf, NA), 
                                                       ncol = 3, byrow = TRUE))
    
    filtered_accumulated_ndmi_sf <- st_as_sf(as.polygons(filtered_accumulated_ndmi, 
                                                         aggregate = FALSE, 
                                                         na.rm = FALSE))
    
  } else {
    
    # If the probability level is inverse, mask first for NOT significant values
    inverse_accumulated_ndmi <- mask(accumulated_ndmi_all, accumulated_ndmi, inverse = TRUE)
    
    filtered_accumulated_ndmi <- classify(inverse_accumulated_ndmi, 
                                          rcl = matrix(c(-Inf, ndmi_ann_min, NA,
                                                         ndmi_ann_max, Inf, NA), 
                                                       ncol = 3, byrow = TRUE))
    
    filtered_accumulated_ndmi_sf <- st_as_sf(as.polygons(filtered_accumulated_ndmi, 
                                                         aggregate = FALSE, 
                                                         na.rm = FALSE))
    
  }
  
  # Handle PROD rasters ----
  annualized_prod <- annualized_change_prod_met[[timeperiod_name_prod]]
  annualized_prod_all <- annualized_change_prod_met[[glue("posterior.mean_{selected_time}")]]
  accumulated_prod <- accumulated_change_prod_met[[timeperiod_name_prod]]
  accumulated_prod_all <- accumulated_change_prod_met[[glue("posterior.mean_{selected_time}")]]
  predicted_prod_year_layer <- predicted_annual_prod_met[[as.character(ram_year)]]
  
  # Filter based on user inputs & create sf objects
  # Predicted PROD
  filtered_predicted_prod <- classify(predicted_prod_year_layer, 
                                      rcl = matrix(c(-Inf, prod_min, NA,
                                                     prod_max, Inf, NA),
                                                   ncol = 3, byrow = TRUE))
  
  filtered_predicted_prod_sf <- st_as_sf(as.polygons(filtered_predicted_prod, 
                                                     aggregate = FALSE))
  
  # Annualized PROD
  # If the probability level is not inverse, filter normally
  if (grepl("Inverse", probability_level_prod) == FALSE)  {
    
    filtered_annualized_prod <- classify(annualized_prod, 
                                         rcl = matrix(c(-Inf, prod_ann_min, NA,
                                                        prod_ann_max, Inf, NA), 
                                                      ncol = 3, byrow = TRUE))
    
    filtered_annualized_prod_sf <- st_as_sf(as.polygons(filtered_annualized_prod, 
                                                        aggregate = FALSE, 
                                                        na.rm = FALSE))
    
  } else {
    
    # If the probability level is inverse, mask first for NOT significant values
    inverse_annualized_prod <- mask(annualized_prod_all, annualized_prod, inverse = TRUE)
    
    filtered_annualized_prod <- classify(inverse_annualized_prod, 
                                         rcl = matrix(c(-Inf, prod_ann_min, NA,
                                                        prod_ann_max, Inf, NA), 
                                                      ncol = 3, byrow = TRUE))
    
    filtered_annualized_prod_sf <- st_as_sf(as.polygons(filtered_annualized_prod, 
                                                        aggregate = FALSE, 
                                                        na.rm = FALSE))
    
  }
  
  # Accumulated PROD
  # If the probability level is not inverse, filter normally
  if (grepl("Inverse", probability_level_prod) == FALSE)  {
    
    filtered_accumulated_prod <- classify(accumulated_prod, 
                                          rcl = matrix(c(-Inf, prod_ann_min, NA,
                                                         prod_ann_max, Inf, NA), 
                                                       ncol = 3, byrow = TRUE))
    
    filtered_accumulated_prod_sf <- st_as_sf(as.polygons(filtered_accumulated_prod, 
                                                         aggregate = FALSE, 
                                                         na.rm = FALSE))
    
  } else {
    
    # If the probability level is inverse, mask first for NOT significant values
    inverse_accumulated_prod <- mask(accumulated_prod_all, accumulated_prod, inverse = TRUE)
    
    filtered_accumulated_prod <- classify(inverse_accumulated_prod, 
                                          rcl = matrix(c(-Inf, prod_ann_min, NA,
                                                         prod_ann_max, Inf, NA), 
                                                       ncol = 3, byrow = TRUE))
    
    filtered_accumulated_prod_sf <- st_as_sf(as.polygons(filtered_accumulated_prod, 
                                                         aggregate = FALSE, 
                                                         na.rm = FALSE))
    
  }
  
  #Handle terrain variables ---- 
  # Slope
  slope_layer <- terrain[["slope"]]
  filtered_slope <- classify(slope_layer, 
                             rcl = matrix(c(-Inf, slope_min, NA,
                                            slope_max, Inf, NA), 
                                          ncol = 3, byrow = TRUE))
  filtered_slope_sf <- st_as_sf(as.polygons(filtered_slope, aggregate = FALSE, na.rm = FALSE))
  
  # DEM
  dem_layer <- terrain[["dem"]]
  filtered_dem <- classify(dem_layer,
                           rcl = matrix(c(-Inf, elev_min, NA,
                                          elev_max, Inf, NA),
                                        ncol = 3, byrow = TRUE))
  filtered_dem_sf <- st_as_sf(as.polygons(filtered_dem, aggregate = FALSE, na.rm = FALSE))
  
  # Aspect
  aspect_layer <- terrain[["aspect"]]
  filtered_aspect <- filtered_aspect <- ifel(aspect_layer %in% aspect_num, aspect_layer, NA)
  filtered_aspect_sf <- st_as_sf(as.polygons(filtered_aspect, aggregate = FALSE, na.rm = FALSE))
  
  # TWI
  twi_layer <- terrain[["twi"]]
  filtered_twi <- classify(twi_layer, 
                           rcl = matrix(c(-Inf, twi_min, NA,
                                          twi_max, Inf, NA), 
                                        ncol = 3, byrow = TRUE))
  filtered_twi_sf <- st_as_sf(as.polygons(filtered_twi, aggregate = FALSE, na.rm = FALSE))
  
  # Ecosystem
  eco_layer <- terrain[["ecosystem"]]
  filtered_eco <- filtered_eco <- ifel(eco_layer %in% eco_num, eco_layer, NA)
  filtered_eco_sf <- st_as_sf(as.polygons(filtered_eco, aggregate = FALSE, na.rm = FALSE))
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Create a mask starting with the available tree cover cell id layer -- change this later to start with cell_id?
  updated_mask <- mask(predicted_annual_tc[[1]], filtered_predicted_tc)
  updated_mask <- mask(updated_mask, filtered_predicted_ndmi)
  updated_mask <- mask(updated_mask, filtered_predicted_prod)
  
  # If not na, mask for filtered annualized tc
  if (any(!is.na(values(filtered_annualized_tc)))) {
    updated_mask <- mask(updated_mask, filtered_annualized_tc)
  }
  
  # If not na, mask for filtered accumulated tc
  if (any(!is.na(values(filtered_accumulated_tc)))) {
    updated_mask <- mask(updated_mask, filtered_accumulated_tc)
  }
  
  # If not na, mask for filtered annualized ndmi
  if (any(!is.na(values(filtered_annualized_ndmi)))) {
    updated_mask <- mask(updated_mask, filtered_annualized_ndmi)
  }
  
  # If not na, mask for filtered accumulated ndmi
  if (any(!is.na(values(filtered_accumulated_ndmi)))) {
    updated_mask <- mask(updated_mask, filtered_accumulated_ndmi)
  }
  
  # If not na, mask for filtered annualized prod
  if (any(!is.na(values(filtered_annualized_prod)))) {
    updated_mask <- mask(updated_mask, filtered_annualized_prod)
  }
  
  # If not na, mask for filtered accumulated prod
  if (any(!is.na(values(filtered_accumulated_prod)))) {
    updated_mask <- mask(updated_mask, filtered_accumulated_prod)
  }
  
  # If not na, mask for filtered slope
  if (any(!is.na(values(filtered_slope)))) {
    updated_mask <- mask(updated_mask, filtered_slope)
  }
  
  # If not na, mask for filtered dem
  if (any(!is.na(values(filtered_dem)))) {
    updated_mask <- mask(updated_mask, filtered_dem)
  }
  
  # If not na, mask for filtered aspect
  if (any(!is.na(values(filtered_aspect)))) {
    updated_mask <- mask(updated_mask, filtered_aspect)
  }
  
  # If not na, mask for filtered twi
  if (any(!is.na(values(filtered_twi)))) {
    updated_mask <- mask(updated_mask, filtered_twi)
  }
  
  # If not na, mask for filtered ecosystem
  if (any(!is.na(values(filtered_eco)))) {
    updated_mask <- mask(updated_mask, filtered_eco)
  }
  
  updated_mask_sf <- st_as_sf(as.polygons(updated_mask, aggregate = FALSE))
  
  # Transform to WGS84 -- already in 4326 -- don't need to transform again
  # filtered_predicted_tc_sf <- st_transform(filtered_predicted_tc_sf, 4326)
  # filtered_annualized_tc_sf <- st_transform(filtered_annualized_tc_sf, 4326)
  # filtered_accumulated_tc_sf <- st_transform(filtered_accumulated_tc_sf, 4326)
  # filtered_slope_sf <- st_transform(filtered_slope_sf, 4326)
  # updated_mask_sf <- st_transform(updated_mask_sf, 4326)
  
  
  
  
  
  
  # # Color palettes
  # pal_tc <- colorNumeric("YlGn", domain = filtered_predicted_tc_sf[[1]], na.color = "transparent")
  # 
  # if (all(!is.na(values(filtered_annualized_tc)))) {
  #   pal_trend <- colorNumeric("PuBu", domain = filtered_annualized_tc_sf %>% pull(timeperiod_name_tc), na.color = "transparent")
  # } else {
  #   pal_trend <- colorNumeric("OrRd", domain = c(-1, 1), na.color = "transparent")
  # }
  # 
  # if (all(!is.na(values(filtered_slope)))) {
  #   pal_slope <- colorNumeric("OrRd", domain = filtered_slope_sf$slope, na.color = "transparent")
  # } else {
  #   pal_slope <- colorNumeric("OrRd", domain = c(0, 90), na.color = "transparent")
  # }
  
  
  # Define all palettes
  pal_tc_pred <- colorNumeric("viridis", domain = filtered_predicted_tc_sf[[1]], na.color = "transparent")
  pal_tc_ann <- colorNumeric(hcl.colors(11, "Red-Green"), domain = filtered_annualized_tc_sf[[1]], na.color = "transparent")
  pal_tc_acc <- colorNumeric(hcl.colors(8, "Cividis"), domain = filtered_accumulated_tc_sf[[1]], na.color = "transparent")
  
  pal_ndmi_pred <- colorNumeric("viridis", domain = filtered_predicted_ndmi_sf[[1]], na.color = "transparent")
  pal_ndmi_ann <- colorNumeric(hcl.colors(11, "Red-Green"), domain = filtered_annualized_ndmi_sf[[1]], na.color = "transparent")
  pal_ndmi_acc <- colorNumeric(hcl.colors(8, "Cividis"), domain = filtered_accumulated_ndmi_sf[[1]], na.color = "transparent")
  
  pal_prod_pred <- colorNumeric("viridis", domain = filtered_predicted_prod_sf[[1]], na.color = "transparent")
  pal_prod_ann <- colorNumeric(hcl.colors(11, "Red-Green"), domain = filtered_annualized_prod_sf[[1]], na.color = "transparent")
  pal_prod_acc <- colorNumeric(hcl.colors(8, "Cividis"), domain = filtered_accumulated_prod_sf[[1]], na.color = "transparent")
  
  pal_slope <- colorNumeric("viridis", domain = filtered_slope_sf[[1]], na.color = "transparent")
  pal_dem <- colorNumeric("magma", domain = filtered_dem_sf[[1]], na.color = "transparent")
  pal_twi <- colorNumeric("viridis", domain = filtered_twi_sf[[1]], na.color = "transparent")
  
  pal_aspect <- colorFactor(brewer.pal(4, "Accent"), domain = filtered_aspect_sf[[1]], na.color = "transparent")
  pal_eco <- colorFactor(brewer.pal(12, "Paired"), domain = filtered_eco_sf[[1]], na.color = "transparent")
  
  # Build map
  ram <- leaflet() %>%
    
    # Set top layer
    addMapPane("topPane", zIndex = 420) %>%
    
    # Add basemap
    addProviderTiles(providers$CartoDB.DarkMatter) %>%
    
    # Add filtered predicted tree cover
    addPolygons(
      data = filtered_predicted_tc_sf,
      fillColor = ~pal_tc_pred(filtered_predicted_tc_sf[[1]]),
      color = ~pal_tc_pred(filtered_predicted_tc_sf[[1]]),
      weight = 0.5,
      fillOpacity = 1,
      opacity = 1,
      popup = ~paste("Estimated Tree Cover:", round(filtered_predicted_tc_sf[[1]], 3), "%"),
      group = "Filtered Predicted TC"
    ) %>% 
    
    # Add filtered annualized tree cover
    addPolygons(
      data = filtered_annualized_tc_sf,
      fillColor = ~pal_tc_ann(filtered_annualized_tc_sf[[1]]),
      color = ~pal_tc_ann(filtered_annualized_tc_sf[[1]]),
      weight = 0.5,
      fillOpacity = 1,
      opacity = 1,
      popup = ~paste("Annualized Tree Cover Change:", round(filtered_annualized_tc_sf[[1]], 3), "% / year"),
      group = "Filtered Annualized TC Change"
    ) %>% 
    
    # Add filtered accumulated tree cover
    addPolygons(
      data = filtered_accumulated_tc_sf,
      fillColor = ~pal_tc_acc(filtered_accumulated_tc_sf[[1]]),
      color = ~pal_tc_acc(filtered_accumulated_tc_sf[[1]]),
      weight = 0.5,
      fillOpacity = 1,
      opacity = 1,
      popup = ~paste("Accumulated Tree Cover Change:", round(filtered_accumulated_tc_sf[[1]], 3), "%"),
      group = "Filtered Accumulated TC Change"
    ) %>% 
    
    # Add filtered predicted ndmi
    addPolygons(
      data = filtered_predicted_ndmi_sf,
      fillColor = ~pal_ndmi_pred(filtered_predicted_ndmi_sf[[1]]),
      color = ~pal_ndmi_pred(filtered_predicted_ndmi_sf[[1]]),
      weight = 0.5,
      fillOpacity = 1,
      opacity = 1,
      popup = ~paste("Estimated Canopy Moisture (NDMI):", round(filtered_predicted_ndmi_sf[[1]], 3)),
      group = "Filtered Predicted NDMI"
    ) %>% 
    
    # Add filtered annualized ndmi
    addPolygons(
      data = filtered_annualized_ndmi_sf,
      fillColor = ~pal_ndmi_ann(filtered_annualized_ndmi_sf[[1]]),
      color = ~pal_ndmi_ann(filtered_annualized_ndmi_sf[[1]]),
      weight = 0.5,
      fillOpacity = 1,
      opacity = 1,
      popup = ~paste("Annualized Canopy Moisture (NDMI) Change:", round(filtered_annualized_ndmi_sf[[1]], 3), " / year"),
      group = "Filtered Annualized NDMI Change"
    ) %>% 
    
    # Add filtered accumulated ndmi
    addPolygons(
      data = filtered_accumulated_ndmi_sf,
      fillColor = ~pal_ndmi_acc(filtered_accumulated_ndmi_sf[[1]]),
      color = ~pal_ndmi_acc(filtered_accumulated_ndmi_sf[[1]]),
      weight = 0.5,
      fillOpacity = 1,
      opacity = 1,
      popup = ~paste("Accumulated Canopy Moisture (NDMI) Change:", round(filtered_accumulated_ndmi_sf[[1]], 3)),
      group = "Filtered Accumulated NDMI Change"
    ) %>% 
    
    # Add filtered predicted prod
    addPolygons(
      data = filtered_predicted_prod_sf,
      fillColor = ~pal_prod_pred(filtered_predicted_prod_sf[[1]]),
      color = ~pal_prod_pred(filtered_predicted_prod_sf[[1]]),
      weight = 0.5,
      fillOpacity = 1,
      opacity = 1,
      popup = ~paste0("Estimated Productivity (", toupper(prod_met), "): ", round(filtered_predicted_prod_sf[[1]], 3)),
      group = "Filtered Predicted Productivity"
    ) %>% 
    
    # Add filtered annualized prod
    addPolygons(
      data = filtered_annualized_prod_sf,
      fillColor = ~pal_prod_ann(filtered_annualized_prod_sf[[1]]),
      color = ~pal_prod_ann(filtered_annualized_prod_sf[[1]]),
      weight = 0.5,
      fillOpacity = 1,
      opacity = 1,
      popup = ~paste0("Annualized Productivity (", toupper(prod_met), ") Change: ", round(filtered_annualized_prod_sf[[1]], 3), " / year"),
      group = "Filtered Annualized Productivity Change"
    ) %>% 
    
    # Add filtered accumulated productivity
    addPolygons(
      data = filtered_accumulated_prod_sf,
      fillColor = ~pal_prod_acc(filtered_accumulated_prod_sf[[1]]),
      color = ~pal_prod_acc(filtered_accumulated_prod_sf[[1]]),
      weight = 0.5,
      fillOpacity = 1,
      opacity = 1,
      popup = ~paste0("Accumulated Productivity (", toupper(prod_met), ") Change: ", round(filtered_accumulated_prod_sf[[1]], 3)),
      group = "Filtered Accumulated Productivity Change"
    ) %>% 
    
    # Add filtered slope
    addPolygons(
      data = filtered_slope_sf,
      fillColor = ~pal_slope(filtered_slope_sf[[1]]),
      color = ~pal_slope(filtered_slope_sf[[1]]),
      weight = 0.5,
      fillOpacity = 1,
      opacity = 1,
      popup = ~paste("Slope:", round(filtered_slope_sf[[1]], 3)),
      group = "Filtered Slope"
    ) %>% 
    
    # Add filtered dem
    addPolygons(
      data = filtered_dem_sf,
      fillColor = ~pal_dem(filtered_dem_sf[[1]]),
      color = ~pal_dem(filtered_dem_sf[[1]]),
      weight = 0.5,
      fillOpacity = 1,
      opacity = 1,
      popup = ~paste("Elevation:", round(filtered_dem_sf[[1]], 3)),
      group = "Filtered Elevation"
    ) %>% 
    
    # Add filtered twi
    addPolygons(
      data = filtered_twi_sf,
      fillColor = ~pal_twi(filtered_twi_sf[[1]]),
      color = ~pal_twi(filtered_twi_sf[[1]]),
      weight = 0.5,
      fillOpacity = 1,
      opacity = 1,
      popup = ~paste("Topographic Wetness Index:", round(filtered_twi_sf[[1]], 3)),
      group = "Filtered TWI"
    ) %>% 
    
    # Add filtered aspect
    addPolygons(
      data = filtered_aspect_sf,
      fillColor = ~pal_aspect(filtered_aspect_sf[[1]]),
      color = ~pal_aspect(filtered_aspect_sf[[1]]),
      weight = 0.5,
      fillOpacity = 1,
      opacity = 1,
      popup = ~paste("Aspect:", filtered_aspect_sf[[1]]),
      group = "Filtered Aspect"
    ) %>% 
    
    # Add filtered ecosystem
    addPolygons(
      data = filtered_eco_sf,
      fillColor = ~pal_eco(filtered_eco_sf[[1]]),
      color = ~pal_eco(filtered_eco_sf[[1]]),
      weight = 0.5,
      fillOpacity = 1,
      opacity = 1,
      popup = ~paste("Ecosystem Type:", filtered_eco_sf[[1]]),
      group = "Filtered Ecosystem"
    ) %>%
    
    # Add filtered ecosystem
    addPolygons(
      data = updated_mask_sf,
      fillColor = "#dd6ba6",
      color = "#dd6ba6",
      weight = 1,
      fillOpacity = 0,
      opacity = 1,
      popup = ~paste("Included in Mask:", updated_mask_sf[[1]]),
      group = "Qualifying Pixels",
      options = pathOptions(pane = "topPane")
    ) %>%
    
    addLayersControl(
      overlayGroups = c("Filtered Predicted TC", 
                        "Filtered Annualized TC Change", 
                        "Filtered Accumulated TC Change",
                        "Filtered Predicted NDMI", 
                        "Filtered Annualized NDMI Change", 
                        "Filtered Accumulated NDMI Change",
                        "Filtered Predicted Productivity", 
                        "Filtered Annualized Productivity Change", 
                        "Filtered Accumulated Productivity Change",
                        "Filtered Slope",
                        "Filtered Elevation",
                        "Filtered TWI",
                        "Filtered Aspect",
                        "Filtered Ecosystem",
                        "Qualifying Pixels"),
      options = layersControlOptions(collapsed = FALSE)
      
    ) %>%
    
    hideGroup(
      c("Filtered Predicted TC", 
        "Filtered Annualized TC Change",
        "Filtered Accumulated TC Change",
        "Filtered Predicted NDMI", 
        "Filtered Annualized NDMI Change",
        "Filtered Accumulated NDMI Change",
        "Filtered Predicted Productivity", 
        "Filtered Annualized Productivity Change",
        "Filtered Accumulated Productivity Change",
        "Filtered Slope",
        "Filtered Elevation",
        "Filtered TWI",
        "Filtered Aspect",
        "Filtered Ecosystem")
    ) %>% 
    
    addScaleBar(position = "bottomright")
  
  list(
    ram = ram,
    
    pal_tc_pred = pal_tc_pred,
    pal_tc_ann = pal_tc_ann,
    pal_tc_acc = pal_tc_acc,
    
    pal_ndmi_pred = pal_ndmi_pred,
    pal_ndmi_ann = pal_ndmi_ann,
    pal_ndmi_acc = pal_ndmi_acc,
    
    pal_prod_pred = pal_prod_pred,
    pal_prod_ann = pal_prod_ann,
    pal_prod_acc = pal_prod_acc,
    
    pal_slope = pal_slope,
    pal_dem = pal_dem,
    pal_twi = pal_twi,
    
    pal_aspect = pal_aspect,
    pal_eco = pal_eco,
    
    filtered_predicted_tc_sf = filtered_predicted_tc_sf,
    filtered_annualized_tc_sf = filtered_annualized_tc_sf,
    filtered_accumulated_tc_sf = filtered_accumulated_tc_sf,
    
    filtered_predicted_ndmi_sf = filtered_predicted_ndmi_sf,
    filtered_annualized_ndmi_sf = filtered_annualized_ndmi_sf,
    filtered_accumulated_ndmi_sf = filtered_accumulated_ndmi_sf,
    
    filtered_predicted_prod_sf = filtered_predicted_prod_sf,
    filtered_annualized_prod_sf = filtered_annualized_prod_sf,
    filtered_accumulated_prod_sf = filtered_accumulated_prod_sf,
    
    filtered_slope_sf = filtered_slope_sf,
    filtered_dem_sf = filtered_dem_sf,
    filtered_twi_sf = filtered_twi_sf,
    
    filtered_aspect_sf = filtered_aspect_sf,
    filtered_eco_sf = filtered_eco_sf
  )
  
}

