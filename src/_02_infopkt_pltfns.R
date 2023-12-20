############################################################
############################################################
# load packages
############################################################
############################################################
# bread-and-butter
library(tidyverse)
library(lubridate)
library(viridis)
library(scales)
library(latex2exp)
# visualization
library(kableExtra)
library(cowplot) # align plots in grid
library(patchwork) # ! better align plots in grid
library(RColorBrewer)
library(ggpubr) #creating and customizing ggplot2-based publication ready plots
library(ggtext) # color text in ggplot
library(USAboundaries)
# spatial analysis
library(sf)
library(lwgeom)
library(terra)

# GGMAP BROKEN ON 10/31/2023
# https://stackoverflow.com/questions/77432892/problem-visualizing-maps-with-staten-maps
# remove.packages("ggmap")
# devtools::install_github("stadiamaps/ggmap")
library(ggmap)
# https://www.youtube-nocookie.com/embed/6jUSyI6x3xg
# ggmap::register_stadiamaps("APIKEY", write = T)
# ggmap::get_stadiamap(c(left = 32.2, bottom = 34.5, right = 34.8, top = 35.8), zoom = 10, maptype = "stamen_terrain") %>% ggmap()

gc()
############################################################
############################################################
# functions
############################################################
############################################################

############################################################
############################################################
# assumptions and methods
############################################################
############################################################
## Analysis Scenarios
  # table theme
    ttheme_temp <- ggpubr::ttheme(
      base_style = "light"
      , colnames.style = colnames_style(
        size = 11
        , face = "bold"
        , fill = "white"
        , linewidth = 0
        , linecolor = "transparent"
      )
      , tbody.style = tbody_style(
        size = 10
        , fill = c("white", "gray96")
        , linewidth = 0
        , linecolor = "transparent"
      )
    )
    # Text plot
      text_temp <- paste(
            "Hierarchy of constraints used to determine whether mechanical"
            , "equipment was allowed and operationally feasible for this"
            , "analysis, where spatial analysis was overlaid from L1"
            , "through L5 to attribute the cause of constraint."
        , sep = " ")
      text_plt_temp <- ggpubr::ggparagraph(
        text = text_temp
        , face = "italic"
        , size = 12
      )
    # Arrange the plots on the same page
    
  
  # mechanical mgmt allowed if:
  n_sc_temp <- 3
  plt_scenario_table_temp <-
    data.frame(
      scenario = 1:n_sc_temp
      , cover = rep("Forest or Shrubland",n_sc_temp)
      , protected = rep("Not Protected",n_sc_temp)
      , slope = c("<40%","<60%","<60%")
      , road = c("<1,000ft","<2,000ft","<2,000ft")
      , riparian = c(">100ft",">100ft",">50ft")
      , admin = c("No Designation","No Designation","Any Designation")
    ) %>% 
      tidyr::pivot_longer(
        cols = -c(scenario)
      ) %>% 
      tidyr::pivot_wider(
        names_from = scenario
        , values_from = value
        , names_prefix = "scenario_"
      ) %>% 
      dplyr::mutate(
        name = factor(
          name
          , levels = c(
            "cover"
            , "protected"
            , "slope"
            , "road"
            , "riparian"
            , "admin"
          )
          , labels = c(
            "NLCD Cover Type"
            , "Protected or\nIRA Status"
            , "Slope"
            , "Distance to\nNearest Road"
            , "Riparian\nBuffer"
            , "Administrative\nDesignation"
          )
          , ordered = T
        )
      ) %>% 
      dplyr::arrange(name) %>% 
      dplyr::mutate(
        lvl = paste0("L",dplyr::row_number()-1,":")
      ) %>% 
      dplyr::relocate(lvl) %>% 
      dplyr::rename(
            " " = 1
            , "Constraint\ntype" = 2
            , "Scenario 1\nmost constrained" = 3
            , "Scenario 2\n " = 4
            , "Scenario 3\nleast constrained" = 5
          ) %>% 
      ggpubr::ggtexttable(rows = NULL, theme = ttheme_temp)
      
  # plot table
    plt_scenario_table <- plt_scenario_table_temp 
    #   ggpubr::ggarrange(
    #     plt_scenario_table_temp
    #     , text_plt_temp
    #   
    #   , ncol = 1
    #   , nrow = 2
    #   , heights = c(1, 0.25)
    # ) + 
    # theme(plot.margin = margin(0,0,0,0,"cm"))
    # plt_scenario_table
## Fireshed Project Area Classifications
  # table theme
  ttheme_temp <- ggpubr::ttheme(
    base_style = "light"
    , colnames.style = colnames_style(
      size = 6.4
      , face = "bold"
      , fill = "white"
      , linewidth = 0
      , linecolor = "transparent"
    )
    , tbody.style = tbody_style(
      size = 7
      , fill = c("white", "gray96")
      , linewidth = 0
      , linecolor = "transparent"
    )
  )
  # Text plot
    text_temp <- paste(
        "Fireshed project areas of approximately 10,000 hectares (25,000 acres)"
        , "in size represent the geographic unit at which vegetation and fuel"
        , "management projects are planned. Fireshed project areas"
        , "were divided into three classes of"
        , "mechanical constraint: high, medium, and low."
      , sep = " ")
    text_plt_temp <- ggpubr::ggparagraph(
      text = text_temp
      , face = "italic"
      , size = 7
    )
  # Arrange the plots on the same page
  
  plt_scenario_table_temp <-
    data.frame(
      lvl = c("High constraint", "Medium constraint", "Low constraint")
      , constrained = c("81–100%", "60–80%", "0–59%")
      , available = c("0–19%", "20–40%","41–100%")
      
    ) %>% 
      dplyr::rename(
            "Level of\nconstraint" = 1
            , "Mechanically\nconstrained area" = 2
            , "Mechanically\navailable area" = 3
          ) %>% 
      ggpubr::ggtexttable(rows = NULL, theme = ttheme_temp)
      
  # plot table
    # plt_cnstrnt_class_table <- ggpubr::ggarrange(
    #     plt_scenario_table_temp + 
    #        theme(plot.margin = margin(0,0,0,0,"cm"))
    #     , NULL
    #     , text_plt_temp
    #   , ncol = 1
    #   , nrow = 3
    #   , heights = c(1,0.01, 0.35)
    # )
  
    plt_cnstrnt_class_table <- cowplot::plot_grid(
        plt_scenario_table_temp + 
           theme(plot.margin = margin(0,0,0.1,0,"cm"))
        , NULL
        , text_plt_temp + 
           theme(plot.margin = margin(0.1,0,0,0,"cm"))
      , ncol = 1
      , nrow = 3
      , rel_heights = c(1,0.02, 0.7)
      , align = "v"
      , axis = "lr"
    ) + 
    theme(plot.margin = margin(0,0,0,0,"cm"))
############################################################
############################################################
### Define Basemap
############################################################
############################################################
# set crs for working with ggmap
plt_crs <- 3857
# color palette for fill of f.p.a.'s
cols_rdylbu_lt <- c(
  "low constraint" = rev(RColorBrewer::brewer.pal(n=3,name="RdYlBu"))[1]
  , "med. constraint" = rev(RColorBrewer::brewer.pal(n=3,name="RdYlBu"))[2]
  , "high constraint" = rev(RColorBrewer::brewer.pal(n=3,name="RdYlBu"))[3]
)
# function to highlight largest patch with fireshed project areas under
cols_rdylbu_dk <- c(
  "low constraint" = rev(RColorBrewer::brewer.pal(n=11,name="RdYlBu"))[2]
  , "med. constraint" = rev(RColorBrewer::brewer.pal(n=11,name="RdYlBu"))[7]
  , "high constraint" = rev(RColorBrewer::brewer.pal(n=11,name="RdYlBu"))[10]
)
# color palette for objective
cols_obj_r <- c("darkcyan", "gray40", "orangered3")
##################hack to align plots for ggmap
ggmap_bbox_fn <- function(map, my_crs=3857) {
    if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
    # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
    # and set the names to what sf::st_bbox expects:
    map_bbox <- setNames(unlist(attr(map, "bb")), c("ymin", "xmin", "ymax", "xmax"))
    # Convert the bbox to an sf polygon, transform it to 3857, 
    # and convert back to a bbox (convoluted, but it works)
    bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), my_crs))
    # Overwrite the bbox of the ggmap object with the transformed coordinates 
    attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
    attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
    attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
    attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
    map
}
# function to plot basemap
landscape_basemap_fn <- function(row_n=1, my_base_size = 11) {
  # buffer for smaller areas
  if_smaller_ha_temp <- wf_landscapes %>% 
    dplyr::pull(hectares) %>% 
    quantile(0.45) %>% 
    purrr::pluck(1)
  # should zoom in?
  zoom_level <- dplyr::case_when(
    area_nm_list[row_n] %in% c(
      "WA: Colville Northeast Washington Vision"
      , "CA: Southern California Fireshed Risk Reduction Strategy"
      ) ~ 7
    , area_nm_list[row_n] %in% c(
      "UT: Wasatch"
      , "CA: Trinity Forest Health and Fire-Resilient Rural Communities"
      , "ID: Nez Perce-Clearwater-Lower Salmon"
      , "NV: Sierra and Elko Fronts"
      , "OR: Mount Hood Forest Health and Fire-Resilient Communities"
    ) ~ 8
    , area_nm_list[row_n] %in% c(
      "CA: Plumas Community Protection"
      , "CA: North Yuba"
      , "MT: Kootenai Complex"
      , "UT: Pine Valley"
    ) ~ 9
    , 
      (wf_landscapes %>% 
        dplyr::filter(
            area_name == area_nm_list[row_n]
        ) %>% 
        dplyr::pull(hectares) %>% 
        purrr::pluck(1)
        <= if_smaller_ha_temp
      ) ~ 10
    , TRUE ~ 8
  )
  # should buffer extend?
  buffer_box <- dplyr::case_when(
    area_nm_list[row_n] %in% c("WA: Colville Northeast Washington Vision") ~ 58000
    , area_nm_list[row_n] %in% c(
      "CA: Southern California Fireshed Risk Reduction Strategy"
      , "ID: Nez Perce-Clearwater-Lower Salmon"
      , "MT: Kootenai Complex"
    ) ~ 40000
    , area_nm_list[row_n] %in% c(
      "NV: Sierra and Elko Fronts"
      , "UT: Pine Valley"
    ) ~ 21000
    , area_nm_list[row_n] %in% c(
      "AZ: San Carlos Apache Tribal Forest Protection"
      , "CA: Plumas Community Protection"
      , "CA: North Yuba"
      , "CA: Trinity Forest Health and Fire-Resilient Rural Communities"
      , "OR: Mount Hood Forest Health and Fire-Resilient Communities"
    ) ~ 35000
    , area_nm_list[row_n] %in% c("UT: Wasatch") ~ 5000
    , 
      (wf_landscapes %>% 
        dplyr::filter(
            area_name == area_nm_list[row_n]
        ) %>% 
        dplyr::pull(hectares) %>% 
        purrr::pluck(1)
        <= if_smaller_ha_temp
      ) ~ 15000
    , TRUE ~ 5000
  )
  
  # bounding box
  bb_temp <- 
    # use extent of fireshed project areas
    fireshed_proj_area %>% 
    dplyr::select(pa_id, geometry) %>% 
    dplyr::mutate(pa_id=as.character(pa_id)) %>% 
    dplyr::inner_join(
      constrained_by_scnro_ls_pa %>% 
        dplyr::filter(
          area_name == area_nm_list[row_n]
          & pct_pa_intrsct>=0.25
        )
      , by = dplyr::join_by("pa_id")
      , multiple = "first"
    ) %>% 
    sf::st_union() %>% 
    sf::st_transform(crs=5070) %>% 
    sf::st_buffer(as.numeric(buffer_box)) %>% 
    sf::st_transform(crs=4326) %>% # same as get_map return
    sf::st_bbox()
  # set bbox for get call
  bbox_temp <- c(
    bottom = bb_temp[[2]]
    , top = bb_temp[[4]]
    , right = bb_temp[[3]]
    , left = bb_temp[[1]]
  )
  # get map
  hey_ggmap <- ggmap::get_stadiamap(
    bbox = bbox_temp
    , zoom = zoom_level
    , maptype = "stamen_terrain" #"toner-hybrid" #"toner-hybrid" # "terrain"
    , crop = T
  )
  # ggmap(hey_ggmap)
  # apply align function
    hey_ggmap_aligned <- ggmap_bbox_fn(hey_ggmap, plt_crs) # Use the function
  # plot
  plt <- ggmap(hey_ggmap_aligned) + 
    # geom_sf(
    #   data = wf_landscapes %>% 
    #     dplyr::filter(
    #         area_name == area_nm_list[row_n]
    #       ) %>% 
    #     sf::st_transform(crs=plt_crs)
    #   , fill = NA, color = "black", lwd = 0.3
    #   , inherit.aes = F
    # ) +
    coord_sf(
      expand = FALSE
    ) +
    labs(
      title = "" #area_nm_list[row_n]
    ) +
    theme_light(base_size = my_base_size) +
    theme(
      legend.position =  "top"
      , legend.direction = "horizontal"
      , legend.title = element_text(size = 8, face = "bold")
      , legend.margin = margin(c(0,0,-5,0))
      , plot.title = element_text(face = "bold") #, hjust = 0.5
      , strip.text = element_text(color = "black", face = "bold")
      , axis.title = element_blank()
      , axis.text = element_blank()
      , axis.ticks = element_blank()
      , panel.grid = element_blank()
      , plot.margin = margin(0, 0, 0, 0, "cm")
      , plot.caption = element_text(color = "black", hjust = 0, vjust = 3)
    )
  # return
  return(plt)
}
###### set plot basemap in global function
# uncomment for testing
plt_basemap <- NULL
# landscape_basemap_fn(row_n = 21)
############################################################
############################################################
### Plot of Nested Fireshed Spatial Framework
############################################################
############################################################
# function to plot basemap + spatial framework
spatial_frmwrk_map_fn <- function(row_n = 1) {
  # join fireshed project area data to spatial data
  fireshed_proj_area_dta_temp <- fireshed_proj_area %>% 
    dplyr::select(pa_id, geometry) %>% 
    dplyr::mutate(pa_id=as.character(pa_id)) %>% 
    dplyr::inner_join(
      constrained_by_scnro_ls_pa %>% 
        dplyr::filter(
          area_name == area_nm_list[row_n]
          & pct_pa_intrsct>=0.25
        )
      , by = dplyr::join_by("pa_id")
      , multiple = "first"
    ) 
  # aggregate for note
  sum_dta_temp <- fireshed_proj_area_dta_temp %>% 
    sf::st_drop_geometry() %>% 
    dplyr::filter(fireshed_crisis_strategy %in% c("USFS-Only", "All-Lands")) %>% 
    dplyr::summarise(total_area_ha = sum(feature_area_ha)) %>% 
    dplyr::mutate(total_area_acres = (total_area_ha*10000)/4046.85642) %>% 
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("total_area")
        , ~ scales::comma(.x, suffix = "k", scale = 1e-3, accuracy = 1)
      )
    )
  # plot
  plt <- plt_basemap +
    geom_sf(data = fireshed_proj_area_dta_temp %>% 
        sf::st_transform(crs=plt_crs)
      , aes(fill = fireshed_crisis_strategy, color = "Project Area\nboundary")
      , alpha = 0.8
      , lwd = 0.7
      , inherit.aes = F
    ) +
    geom_sf(
      data = fireshed %>%
        sf::st_filter(
          wf_landscapes %>%
            dplyr::filter(area_name == area_nm_list[row_n]) %>% 
            dplyr::select(area_name)
          , .predicate=st_intersects
        ) %>% 
        sf::st_transform(crs=plt_crs)
      , aes(color = "Fireshed\nboundary")
      , fill = NA
      , lwd = 0.6
      , linetype = "dashed"
      , inherit.aes = F
    ) +
    geom_sf(
      data = wf_landscapes %>% 
        dplyr::filter(
            area_name == area_nm_list[row_n]
          ) %>% 
        sf::st_transform(crs=plt_crs)
      , mapping = aes(color = "WCS Landscape\nboundary")
      , fill = NA
      # , color = "black"
      , lwd = 0.5
      , inherit.aes = F
    ) +
    scale_fill_manual(values = c("All-Lands"="firebrick","USFS-Only"="orange3","Not High Risk"="gray88")) +
    scale_color_manual(values = c("gray44","skyblue2", "black")) +
    labs(
      fill = "Fireshed\nhigh-risk status"
      , color = "Spatial\nFramework"
      , title = "Spatial framework and risk"
      , caption = paste0(
        "High-Risk Fireshed Project Areas:\n"
        , sum_dta_temp$total_area_acres[1], " ac"
        , " ("
        , sum_dta_temp$total_area_ha[1], " ha"
        , ")"
      )
    ) +
    coord_sf(expand = F) +
    guides(
      color = guide_legend(order = 1, ncol = 2, override.aes = list(orientation = "vertical", size = 6, linewidth = 5, fill = NA))
      , fill = guide_legend(order = 2, ncol = 2, override.aes = list(orientation = "vertical", size = 6,linewidth = 0, alpha = 1))
    )
  # return
  return(plt)
}
# spatial_frmwrk_map_fn(1)
############################################################
############################################################
### Read Raster Data Function
############################################################
############################################################
# This raster data was created via this [Google Earth Engine script](https://code.earthengine.google.com/692417f7747e247fc0545824135a0355?noload=true).
# function to read raster data
raster_data_read_fn <- function(row_n = 1) {
 # set up name like in file name
  nm_temp <- paste0(
    "*"
    , area_nm_list[row_n] %>% 
      stringr::str_split(pattern = ":", simplify = T) %>% 
      purrr::pluck(2) %>% 
      trimws() %>% 
      stringr::str_replace_all(pattern = " ", replacement = "_")
    , ".tif$"
  )
  # read all raster data files and stack
  files_temp <- list.files("./../../wcs_mechanical_constraints/data/wildfirepriority_WCS202308/images/",pattern = nm_temp,recursive = T) 
  layers_names <- files_temp %>% stringr::word(1,sep = "/")
  rast_list_temp <- files_temp %>%  
    purrr::map(function(x){
      terra::rast(paste0("./../../wcs_mechanical_constraints/data/wildfirepriority_WCS202308/images/",x)) 
    }) %>% 
    # name list objects
    setNames(layers_names) 
  # return
  return(rast_list_temp)
}
rast_list_temp <- NULL
# rast_list_temp <- raster_data_read_fn(row_n = 4)

############################################################
############################################################
### create data frame from raster
############################################################
############################################################
# Define function to select, aggregate, filter, and create data frame from raster
# define function to select, aggregate, filter, and df from raster
rast2df_fn <- function(sc_n = 1, layer_name = "is_treatable", row_n = 1, rast_agg_fact = 2, keep_0_1_all = "all"){
  # find index of layer name
  layer_name_index <- rast_list_temp[[sc_n]] %>% 
    names() %>% 
    purrr::detect_index(function(x) x == layer_name)
  # mask values
  mask_l <- ifelse(keep_0_1_all == "all"
    , terra::values(rast_list_temp[[sc_n]][[layer_name_index]]) %>% unique() %>% min()
    , as.numeric(keep_0_1_all)
  )
  mask_u <- ifelse(keep_0_1_all == "all"
    , terra::values(rast_list_temp[[sc_n]][[layer_name_index]]) %>% unique() %>% max()
    , as.numeric(keep_0_1_all)
  )
  # mask raster, aggregate, to df
  rast_list_temp[[sc_n]][[layer_name_index]] %>% 
    terra::mask(
      rast_list_temp[[sc_n]][[layer_name_index]] %>% 
        terra::clamp(lower = mask_l, upper = mask_u, values = FALSE)
    ) %>%
    terra::mask(
      wf_landscapes %>%
        dplyr::filter(area_name==area_nm_list[row_n]) %>% 
        terra::vect() %>%
        terra::project(terra::crs(rast_list_temp[[sc_n]][[layer_name_index]]))
    ) %>%
    terra::aggregate(fact = rast_agg_fact, fun = "modal") %>%
    terra::as.factor() %>% 
    terra::project(
      wf_landscapes %>% 
        dplyr::filter(
            area_name == area_nm_list[row_n]
          ) %>% 
        sf::st_transform(crs=plt_crs) %>% 
        terra::vect() %>%
        terra::crs()
    ) %>% 
    as.data.frame(xy=T) %>% 
    dplyr::rename(f = 3) %>% 
    dplyr::mutate(
      f=factor(f)
      , scenario_lab = paste0("Scenario ", sc_n)
      , layer_name = layer_name
      , area_name = area_nm_list[row_n]
    )
}
############################################################
############################################################
### select raster and aggregate area
############################################################
############################################################
# Define function to select raster and aggregate area
rast_area_fn <- function(sc_n = 1, layer_name = "is_treatable", row_n = 1){
  # find index of layer name
  layer_name_index <- rast_list_temp[[sc_n]] %>% 
    names() %>% 
    purrr::detect_index(function(x) x == layer_name)
  # df area for caption
  area_df <- rast_list_temp[[sc_n]][[layer_name_index]] %>%
    terra::mask(
      wf_landscapes %>%
        dplyr::filter(area_name==area_nm_list[row_n]) %>%
        terra::vect() %>%
        terra::project(terra::crs(rast_list_temp[[sc_n]][[layer_name_index]]))
    ) %>%
    terra::project(
      wf_landscapes %>%
        dplyr::filter(area_name==area_nm_list[row_n]) %>%
        sf::st_transform(5070) %>% # transform to GEE proj used for calcs
        terra::vect() %>%
        terra::crs()
    ) %>%
    terra::freq() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      area_m2 = count * (terra::res(rast_list_temp[[sc_n]][[layer_name_index]]) %>% prod())
      , area_pct = area_m2 / sum(area_m2, na.rm = T)
      # labels
      , area_ha = area_m2/10000
      , area_acres = area_m2/4046.85642
      , area_ha_lab = scales::comma(area_ha, suffix = "k", scale = 1e-3, accuracy = 1)
      , area_acres_lab = scales::comma(area_acres, suffix = "k", scale = 1e-3, accuracy = 1)
      , area_pct_lab = scales::percent(area_pct, accuracy = 1)
      # data str
      , scenario_lab = paste0("Scenario ", sc_n)
      , layer_name = layer_name
      , area_name = area_nm_list[row_n]
    )
  #################overwrite real quick for testing
  # area_df <- data.frame(
  #     area_m2 = 1
  #     , area_pct = 1
  #     # labels
  #     , area_ha = 1
  #     , area_acres = 1
  #     , area_ha_lab = "1000k"
  #     , area_acres_lab = "1000k"
  #     , area_pct_lab = "100%"
  #     # data str
  #     , scenario_lab = paste0("Scenario ", 1)
  #     , layer_name = "layer_name"
  #     , area_name = "hey"
  #     , count = 1
  #     , value = 1
  #   )
  # return
  return(area_df)
}
# rast_area_fn(row_n = 4, layer_name = "is_treatable")

############################################################
############################################################
### Forest and Shrub Land Cover plot
############################################################
############################################################
# function to plot basemap + forest&shrub
landcover_map_fn <- function(row_n = 1, rast_agg_fact = 2) {
  # area for caption using data to match reported in publication
  area_temp <- constrained_by_scnro_ls %>% 
    dplyr::filter(scenario_id==1 & area_name == area_nm_list[row_n]) %>% 
    dplyr::mutate(covertype_area_acres=(covertype_area_ha*10000)/4046.85642) %>% 
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("covertype_area_")
        , ~ scales::comma(.x, suffix = "k", scale = 1e-3, accuracy = 1)
      )
    )
  # plot forest and shrub cover
  plt <- plt_basemap +
    geom_tile(
      data = rast2df_fn(
        sc_n = 1
        , layer_name = "is_selected_nlcd"
        , row_n = row_n
        , rast_agg_fact = rast_agg_fact
        , keep_0_1_all = "1"
      )
      , mapping = aes(x=x,y=y,fill=f)
      , alpha = 0.7
      , inherit.aes = F
    ) +
    geom_sf(
      data = wf_landscapes %>% 
        dplyr::filter(
            area_name == area_nm_list[row_n]
          ) %>% 
        sf::st_transform(crs=plt_crs)
      , fill = NA
      , color = "black"
      , lwd = 0.5
      , inherit.aes = F
    ) + 
    scale_fill_manual(values = c("forestgreen"), na.value = "transparent") +
    labs(
      title = "Forest & Shrub Land Cover"
      , subtitle = "<span><span style='color:forestgreen;'><b>Forest & Shrub</b></span>"
      , caption = paste0(
          "Forest & Shrub: "
          , area_temp$covertype_area_acres[1], " ac"
          , " ("
          , area_temp$covertype_area_ha[1], " ha"
          , ")"
        )
    ) +
    coord_sf(expand = F) +
    theme(
      legend.position = "none"
      , plot.subtitle = ggtext::element_markdown(size = 10)
    )

  # return
  return(plt)
}
# landcover_map_fn(row_n = 4, rast_agg_fact = 5)

############################################################
############################################################
### Protected Area plot
############################################################
############################################################
# function to plot basemap + protected
protected_map_fn <- function(row_n = 1, rast_agg_fact = 2) {
  # area_df
  area_df <- rast_area_fn(sc_n = 1, layer_name = "is_protected", row_n = row_n)
  # plot forest and shrub cover
  plt <- plt_basemap +
    geom_tile(
      data = rast2df_fn(
        sc_n = 1
        , layer_name = "is_protected"
        , row_n = row_n
        , rast_agg_fact = rast_agg_fact
        , keep_0_1_all = "1"
      )
      , mapping = aes(x=x,y=y,fill=f)
      , alpha = 0.7
      , inherit.aes = F
    ) +
    geom_sf(
      data = wf_landscapes %>% 
        dplyr::filter(
            area_name == area_nm_list[row_n]
          ) %>% 
        sf::st_transform(crs=plt_crs)
      , fill = NA
      , color = "black"
      , lwd = 0.5
      , inherit.aes = F
    ) + 
    scale_fill_manual(values = c("#fadb24"), na.value = "transparent") +
    labs(
      title = "Protected & Inventoried Roadless Areas"
      , subtitle = "<span><span style='color:#fadb24;'><b>Protected & IRA</b></span>"
      , caption = paste0(
          "Protected & IRA: "
          , area_df %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_acres_lab)
          , " ac"
          , " ("
          , area_df %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_ha_lab)
          , " ha"
          , ")"
        )
    ) +
    coord_sf(expand = F) +
    theme(
      legend.position = "none"
      , plot.subtitle = ggtext::element_markdown(size = 10)
    )

  # return
  return(plt)
}
# protected_map_fn(row_n = 5, rast_agg_fact = 6)

############################################################
############################################################
### Steep Slopes plot
############################################################
############################################################
# scales::show_col(viridis::plasma(n = 30))
# function to plot basemap + slopes
slopes_map_fn <- function(row_n = 1, rast_agg_fact = 2) {
  # area_df
  area_df1 <- rast_area_fn(sc_n = 1, layer_name = "is_steep_slopes", row_n = row_n)
  # area_df
  area_df2 <- rast_area_fn(sc_n = 2, layer_name = "is_steep_slopes", row_n = row_n)
  # plot forest and shrub cover
  plt <- plt_basemap +
    # scenario 1 slopes
    geom_tile(
      data = rast2df_fn(
        sc_n = 1
        , layer_name = "is_steep_slopes"
        , row_n = row_n
        , rast_agg_fact = rast_agg_fact
        , keep_0_1_all = "1"
      )
      , mapping = aes(x=x,y=y,fill="40")
      , alpha = 0.8
      , inherit.aes = F
    ) +
  # scenario 2 slopes
    geom_tile(
      data = rast2df_fn(
        sc_n = 2
        , layer_name = "is_steep_slopes"
        , row_n = row_n
        , rast_agg_fact = rast_agg_fact
        , keep_0_1_all = "1"
      )
      , mapping = aes(x=x,y=y,fill="60")
      , alpha = 0.7
      , inherit.aes = F
    ) +
    geom_sf(
      data = wf_landscapes %>% 
        dplyr::filter(
            area_name == area_nm_list[row_n]
          ) %>% 
        sf::st_transform(crs=plt_crs)
      , fill = NA
      , color = "black"
      , lwd = 0.5
      , inherit.aes = F
    ) + 
    scale_fill_manual(values = c("#febe2a","#f58c46"), na.value = "transparent") +
    labs(
      title = "Steep Slopes"
      # , subtitle = "<span><span style='color:#febe2a;'><b>Slopes >40%</b></span> | <span style='color:#f58c46;'><b>Slopes >60%</b></span></span>"
      , subtitle = "<b>Slopes</b><span><span style='color:#febe2a;'><b> >40%</b></span> | <span style='color:#f58c46;'><b>>60%</b></span></span>"
      , caption = paste0(
          "Slopes >40%: "
          , area_df1 %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_acres_lab)
          , " ac"
          , " ("
          , area_df1 %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_ha_lab)
          , " ha"
          , ")"
        # sc2 slopes
          , "\nSlopes >60%: "
          , area_df2 %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_acres_lab)
          , " ac"
          , " ("
          , area_df2 %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_ha_lab)
          , " ha"
          , ")"
        )
    ) +
    coord_sf(expand = F) +
    theme(
      legend.position = "none"
      , plot.subtitle = ggtext::element_markdown(size = 10)
    )

  # return
  return(plt)
}
# slopes_map_fn(row_n = 4, rast_agg_fact = 6)

############################################################
############################################################
### Roads Distant plot
############################################################
############################################################
# scales::show_col(viridis::plasma(n = 30))
# function to plot basemap + roads
roads_map_fn <- function(row_n = 1, rast_agg_fact = 2) {
  # area_df
  area_df1 <- rast_area_fn(sc_n = 1, layer_name = "is_roads_distant", row_n = row_n)
  # area_df
  area_df2 <- rast_area_fn(sc_n = 2, layer_name = "is_roads_distant", row_n = row_n)
  # plot forest and shrub cover
  plt <- plt_basemap +
    # scenario 1 roads
    geom_tile(
      data = rast2df_fn(
        sc_n = 1
        , layer_name = "is_roads_distant"
        , row_n = row_n
        , rast_agg_fact = rast_agg_fact
        , keep_0_1_all = "1"
      )
      , mapping = aes(x=x,y=y,fill="1000")
      , alpha = 0.8
      , inherit.aes = F
    ) +
  # scenario 2 roads
    geom_tile(
      data = rast2df_fn(
        sc_n = 2
        , layer_name = "is_roads_distant"
        , row_n = row_n
        , rast_agg_fact = rast_agg_fact
        , keep_0_1_all = "1"
      )
      , mapping = aes(x=x,y=y,fill="2000")
      , alpha = 0.7
      , inherit.aes = F
    ) +
    geom_sf(
      data = wf_landscapes %>% 
        dplyr::filter(
            area_name == area_nm_list[row_n]
          ) %>% 
        sf::st_transform(crs=plt_crs)
      , fill = NA
      , color = "black"
      , lwd = 0.5
      , inherit.aes = F
    ) + 
    scale_fill_manual(values = c("#e56a5d","#be3885"), na.value = "transparent") +
    labs(
      title = "Roads Distant"
      # , subtitle = "<span><span style='color:#e56a5d;'><b>Roads >1,000ft</b></span> | <span style='color:#be3885;'><b>Roads >2,000ft</b></span></span>"
      , subtitle = "<b>Roads</b><span><span style='color:#e56a5d;'><b> >1,000ft</b></span> | <span style='color:#be3885;'><b>>2,000ft</b></span></span>"
      , caption = paste0(
          "Roads >1,000ft: "
          , area_df1 %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_acres_lab)
          , " ac"
          , " ("
          , area_df1 %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_ha_lab)
          , " ha"
          , ")"
          , "\nRoads >2,000ft: "
          , area_df2 %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_acres_lab)
          , " ac"
          , " ("
          , area_df2 %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_ha_lab)
          , " ha"
          , ")"
        )
    ) +
    coord_sf(expand = F) +
    theme(
      legend.position = "none"
      , plot.subtitle = ggtext::element_markdown(size = 10)
    )

  # return
  return(plt)
}
# roads_map_fn(row_n = 4, rast_agg_fact = 6)
############################################################
############################################################
### Riparian Buffer plot
############################################################
############################################################
# scales::show_col(viridis::plasma(n = 30))
# function to plot basemap + riparian
riparian_map_fn <- function(row_n = 1, rast_agg_fact = 2) {
  # area_df
  area_df1 <- rast_area_fn(sc_n = 1, layer_name = "is_riparian_buffer", row_n = row_n)
  # area_df
  area_df3 <- rast_area_fn(sc_n = 3, layer_name = "is_riparian_buffer", row_n = row_n)
  # plot forest and shrub cover
  plt <- plt_basemap +
    # scenario 1 riparian
    geom_tile(
      data = rast2df_fn(
        sc_n = 1
        , layer_name = "is_riparian_buffer"
        , row_n = row_n
        , rast_agg_fact = rast_agg_fact
        , keep_0_1_all = "1"
      )
      , mapping = aes(x=x,y=y,fill="ft100")
      , alpha = 0.8
      , inherit.aes = F
    ) +
  # scenario 3 riparian
    geom_tile(
      data = rast2df_fn(
        sc_n = 3
        , layer_name = "is_riparian_buffer"
        , row_n = row_n
        , rast_agg_fact = rast_agg_fact
        , keep_0_1_all = "1"
      )
      , mapping = aes(x=x,y=y,fill="ft50")
      , alpha = 0.7
      , inherit.aes = F
    ) +
    geom_sf(
      data = wf_landscapes %>% 
        dplyr::filter(
            area_name == area_nm_list[row_n]
          ) %>% 
        sf::st_transform(crs=plt_crs)
      , fill = NA
      , color = "black"
      , lwd = 0.5
      , inherit.aes = F
    ) + 
    scale_fill_manual(values = c("#a01a9c","#6e00a8"), na.value = "transparent") +
    labs(
      title = "Riparian Buffer"
      , subtitle = "<b>Riparian Buffer </b><span><span style='color:#a01a9c;'><b>100ft</b></span> | <span style='color:#6e00a8;'><b>50ft</b></span></span>"
      , caption = paste0(
          "Riparian Bfr 100ft: "
          , area_df1 %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_acres_lab)
          , " ac"
          , " ("
          , area_df1 %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_ha_lab)
          , " ha"
          , ")"
          , "\nRiparian Bfr 50ft: "
          , area_df3 %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_acres_lab)
          , " ac"
          , " ("
          , area_df3 %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_ha_lab)
          , " ha"
          , ")"
        )
    ) +
    coord_sf(expand = F) +
    theme(
      legend.position = "none"
      , plot.subtitle = ggtext::element_markdown(size = 10)
    )

  # return
  return(plt)
}
# riparian_map_fn(row_n = 4, rast_agg_fact = 6)
######## export image
# ggplot2::ggsave(
#     filename = paste0("../data/xxx_riparian_fact2.png")
#     , plot = ggplot2::last_plot()
#     , width = 11
#     , height = 8.5
#     , units = "in"
#     , dpi = "print"
#     , bg = "white"
#   )
############################################################
############################################################
### Administrative Designation plot
############################################################
############################################################
# function to plot basemap + administrative
administrative_map_fn <- function(row_n = 1, rast_agg_fact = 2) {
  # area_df
  area_df <- rast_area_fn(sc_n = 1, layer_name = "is_administrative", row_n = row_n)
  # plot forest and shrub cover
  plt <- plt_basemap +
    geom_tile(
      data = rast2df_fn(
        sc_n = 1
        , layer_name = "is_administrative"
        , row_n = row_n
        , rast_agg_fact = rast_agg_fact
        , keep_0_1_all = "1"
      )
      , mapping = aes(x=x,y=y,fill=f)
      , alpha = 0.7
      , inherit.aes = F
    ) +
    geom_sf(
      data = wf_landscapes %>% 
        dplyr::filter(
            area_name == area_nm_list[row_n]
          ) %>% 
        sf::st_transform(crs=plt_crs)
      , fill = NA
      , color = "black"
      , lwd = 0.5
      , inherit.aes = F
    ) + 
    scale_fill_manual(values = c("#47039F"), na.value = "transparent") +
    labs(
      title = "Administratively Designated Areas"
      , subtitle = "<span><span style='color:#47039F;'><b>Administrative Designation</b></span>"
      , caption = paste0(
          "Administrative: "
          , area_df %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_acres_lab)
          , " ac"
          , " ("
          , area_df %>% 
            dplyr::filter(value == 1) %>% 
            dplyr::pull(area_ha_lab)
          , " ha"
          , ")"
        )
    ) +
    coord_sf(expand = F) +
    theme(
      legend.position = "none"
      , plot.subtitle = ggtext::element_markdown(size = 10)
    )

  # return
  return(plt)
}
# administrative_map_fn(row_n = 4, rast_agg_fact = 6)
############################################################
############################################################
### Overall Mechanically Constrained/Available plot
############################################################
############################################################
# function to plot basemap + treatable
treatable_map_fn <- function(row_n = 1, rast_agg_fact = 2) {
  # plot available/constrained
  plt <- plt_basemap +
    geom_tile(
      data = 1:length(rast_list_temp) %>% 
        purrr::map(
          rast2df_fn
          , layer_name = "is_treatable"
          , row_n = row_n
          , rast_agg_fact = rast_agg_fact
          , keep_0_1_all = "all"
        ) %>% 
        dplyr::bind_rows()
      , mapping = aes(x=x,y=y,fill=f)
      , alpha = 0.7
      , inherit.aes = F
    ) +
    geom_sf(
      data = wf_landscapes %>% 
        dplyr::filter(
            area_name == area_nm_list[row_n]
          ) %>% 
        sf::st_transform(crs=plt_crs)
      , fill = NA
      , color = "black"
      , lwd = 0.5
      , inherit.aes = F
    ) +
    geom_text(
      data = constrained_by_scnro_ls %>% 
        dplyr::filter(area_name==area_nm_list[row_n]) %>% 
        dplyr::mutate(area_pct_lab = scales::percent(pct_rmn5_administrative, accuracy=1))
      , mapping = aes(
        label = paste0(
            "Mechanically available: "
            , area_pct_lab
            # , " ("
            # , area_acres_lab
            # , " acres; "
            # , area_ha_lab
            # , " ha"
            # , ")"
          )
        , x = ggplot2::layer_scales(plt_basemap)$x$range$range[1]
        , y = ggplot2::layer_scales(plt_basemap)$y$range$range[1]
        , hjust = 0
        , vjust = 1.1
      )
      , inherit.aes = F
      # , size = 3
    ) +
    facet_grid(
      cols = vars(scenario_lab)
      , labeller = label_wrap_gen(width = 35, multi_line = TRUE)
      # , switch = "both"
    ) +
    scale_fill_manual(values = c("#7A0403","#466BE3"), na.value = "transparent") +
    labs(
      title = "Mechanical Operability"
      , subtitle = "<span><span style='color:#466BE3;'><b>Available</b></span> | <span style='color:#7A0403;'><b>Constrained</b></span></span>"
    ) +
    coord_sf(expand = F, clip = "off") +
    theme(
      legend.position = "none"
      , plot.subtitle = ggtext::element_markdown(size = 10)
      , strip.text = element_text(color = "black", face = "bold")
    )

  # return
  return(plt)
}
# treatable_map_fn(row_n = 4, rast_agg_fact = 7)
# ######## export image
# ggplot2::ggsave(
#     filename = paste0("../data/xxx_istreatable.png")
#     , plot = ggplot2::last_plot()
#     , width = 11
#     , height = 8.5
#     , units = "in"
#     , dpi = "print"
#     , bg = "white"
#   )

############################################################
############################################################
### Reduction in treatable area table
############################################################
############################################################
### Reduction in treatable area table
reduction_table_fn <- function(row_n = 1) {
  table_temp <- 
    constrained_by_scnro_ls %>% 
      dplyr::filter(area_name == area_nm_list[row_n]) %>%
      dplyr::mutate(
        dplyr::across(
          tidyselect::ends_with("_ha")
          , ~ scales::comma(.x, suffix = "k", scale = 1e-3, accuracy = 1)
        )
        , dplyr::across(
          tidyselect::starts_with("pct_")
          , ~ scales::percent(.x, accuracy = .1)
        )
      ) %>% 
      dplyr::select(
        # area_name
        scenario_lab
        , covertype_area_ha
        , pct_covertype_area
        , pct_rdctn1_protected
        , pct_rdctn2_slope
        , pct_rdctn3_roads
        , pct_rdctn4_riparian
        , pct_rdctn5_administrative
        , rmn5_administrative_area_ha
        , pct_rmn5_administrative
      ) %>% 
      dplyr::arrange(desc(scenario_lab)) %>% 
      dplyr::rename(
        "Scenario\n " = scenario_lab
        , "Forest & Shrub\n(ha)" = covertype_area_ha
        , "Forest & Shrub\n(%)" = pct_covertype_area
        , "Protected\n& IRA " = pct_rdctn1_protected
        , "Slope\nSteepness" = pct_rdctn2_slope
        , "Road\nDistance" = pct_rdctn3_roads
        , "Riparian\nBuffer" = pct_rdctn4_riparian
        , "Administrative\n " = pct_rdctn5_administrative
        , "Mechanically\nAvailable (ha)" = rmn5_administrative_area_ha
        , "Mechanically\nAvailable (%)" = pct_rmn5_administrative
      )
  # table theme
  ttheme_temp <- ggpubr::ttheme(
    base_style = "light"
    , colnames.style = colnames_style(
      size = 10
      , face = "bold"
      , fill = "white"
      , linewidth = 0
      , linecolor = "transparent"
    )
    , tbody.style = tbody_style(
      size = 10
      , fill = c("white", "gray96")
      , linewidth = 0
      , linecolor = "transparent"
    )
  )
  # Text plot
    text <- paste(
          "Combined forest and shrubland area within the landscape boundaries"
          , "and the percent reduction of different types of constraints"
          , "on mechanical treatment based on three scenarios of"
          , "operational constraints using current standards."
          , "At each level of constraint, the reduction in area and percentage"
          , "was calculated based on the area remaining after considering"
          , "the cumulative impact of prior constraints."
      , sep = " ")
    text_plt <- ggpubr::ggparagraph(
      text = text
      , face = "italic"
      , size = 9
    )
  # Arrange the plots on the same page
  
  # plot table
  plt <- ggpubr::ggtexttable(table_temp, rows = NULL, theme = ttheme_temp) + 
  # ggpubr::ggarrange(
  #   
  #     ggpubr::ggtexttable(table_temp, rows = NULL, theme = ttheme_temp)
  #     , text_plt
  #   
  #   , ncol = 1
  #   , nrow = 2
  #   , heights = c(1, 0.25)
  # ) +
  theme(
    plot.margin = margin(0,0,0,0,"cm")
  )
  
  
  
  return(plt)
}
# reduction_table_fn(1)
############################################################
############################################################
# reduction table kable
############################################################
############################################################
reduction_kable_fn <- function(row_n = 1) {
  constrained_by_scnro_ls %>% 
        dplyr::filter(area_name == area_nm_list[row_n]) %>%
        dplyr::mutate(
          dplyr::across(
            tidyselect::ends_with("_ha")
            , ~ scales::comma(.x, suffix = "k", scale = 1e-3, accuracy = 1)
          )
          , dplyr::across(
            tidyselect::starts_with("pct_")
            , ~ scales::percent(.x, accuracy = .1)
          )
        ) %>% 
        dplyr::select(
          # area_name
          scenario_lab
          , covertype_area_ha
          , pct_covertype_area
          , pct_rdctn1_protected
          , pct_rdctn2_slope
          , pct_rdctn3_roads
          , pct_rdctn4_riparian
          , pct_rdctn5_administrative
          , rmn5_administrative_area_ha
          , pct_rmn5_administrative
        ) %>% 
        dplyr::arrange(desc(scenario_lab)) %>% 
        dplyr::rename(
          "Scenario\n " = scenario_lab
          , "Forest & Shrub\n(ha)" = covertype_area_ha
          , "Forest & Shrub\n(%)" = pct_covertype_area
          , "Protected\n& IRA " = pct_rdctn1_protected
          , "Slope\nSteepness" = pct_rdctn2_slope
          , "Road\nDistance" = pct_rdctn3_roads
          , "Riparian\nBuffer" = pct_rdctn4_riparian
          , "Administrative\n " = pct_rdctn5_administrative
          , "Mechanically\nAvailable (ha)" = rmn5_administrative_area_ha
          , "Mechanically\nAvailable (%)" = pct_rmn5_administrative
          # # remove \n for kable to pdf?
          # "Scenario" = scenario_lab
          # , "Forest & Shrub (ha)" = covertype_area_ha
          # , "Forest & Shrub (%)" = pct_covertype_area
          # , "Protected & IRA " = pct_rdctn1_protected
          # , "Slope Steepness" = pct_rdctn2_slope
          # , "Road Distance" = pct_rdctn3_roads
          # , "Riparian Buffer" = pct_rdctn4_riparian
          # , "Administrative" = pct_rdctn5_administrative
          # , "Mechanically Available (ha)" = rmn5_administrative_area_ha
          # , "Mechanically Available (%)" = pct_rmn5_administrative
        ) %>% 
    # knitr::kable(
    kableExtra::kbl(
      # format = "latex"
      booktabs = TRUE
      # , longtable = TRUE
      # , linesep = ""
      # , align = "l"
      # col.names = linebreak(column_names, align = "l"),
      , escape = FALSE
      , digits = 1
      , caption = ""
      ) %>%
    kableExtra::kable_styling(
      latex_options = c("scale_down", "striped", "hold_position")
    )
  
}
# reduction_kable_fn(5)
############################################################
############################################################
### Title plot
############################################################
############################################################
# plot for titler
title_plot_fn <- function(row_n = 1) {
  plt <-
    ggplot() +
    labs(
      title = wf_landscapes %>% dplyr::filter(area_name==area_nm_list[row_n]) %>% dplyr::pull(name)
      , subtitle = paste0(
        wf_landscapes %>% dplyr::filter(area_name==area_nm_list[row_n]) %>% dplyr::pull(state)
        , " ("
        , wf_landscapes %>% dplyr::filter(area_name==area_nm_list[row_n]) %>% dplyr::pull(investment)
        , " investment)"
        # , "\n"
        # , wf_landscapes %>% dplyr::filter(area_name==area_nm_list[row_n]) %>% dplyr::pull(acres) %>% scales::comma(suffix = "k", scale = 1e-3, accuracy = 1)
        # , " acres ("
        # , wf_landscapes %>% dplyr::filter(area_name==area_nm_list[row_n]) %>% dplyr::pull(hectares) %>% scales::comma(suffix = "k", scale = 1e-3, accuracy = 1)
        # , " ha)"
      )
      # , caption = "none"
    ) +
    theme_void() +
    theme(
      panel.background = element_blank()
      , plot.title = element_text(face = "bold", size = 24)
      , plot.subtitle = element_text(size = 14)
    )
  # return
  return(plt)
}
# title_plot_fn(4)

############################################################
############################################################
### scenario constraints + basemap
############################################################
############################################################
# function to plot scenario constraints + basemap
scenario_cnstrnt_map_fn <- function(row_n = 1) {
  plt <- plt_basemap + 
      geom_sf(
        data = 
          # prep fireshed data for plotting
          fireshed_proj_area %>% 
              dplyr::select(pa_id, geometry) %>% 
              dplyr::mutate(pa_id=as.character(pa_id)) %>% 
              dplyr::inner_join(
                constrained_by_scnro_ls_pa %>% 
                  dplyr::filter(
                    area_name == area_nm_list[row_n]
                    & pct_pa_intrsct>=0.25
                  )
                , by = dplyr::join_by("pa_id")
                , multiple = "all"
              ) %>% 
              dplyr::mutate(
                dplyr::across(
                  tidyselect::starts_with("pct_")
                  , ~ scales::percent(as.numeric(.x), accuracy = 0.1)
                )
                , dplyr::across(
                  tidyselect::ends_with("_ha")
                  , ~ scales::comma(as.numeric(.x), accuracy = 1)
                )
                , scenario_lab = scenario_lab %>% forcats::fct_rev()
              ) %>%
          sf::st_transform(crs=plt_crs)
        , aes(fill = cnstrnt_class), lwd = 0.05
        , inherit.aes = F
        , alpha = 0.6
      ) +
      geom_sf(
        data = wf_landscapes %>% 
          dplyr::filter(
              area_name == area_nm_list[row_n]
            ) %>% 
          sf::st_transform(crs=plt_crs)
        , fill = NA, color = "black", lwd = 0.3
        , inherit.aes = F
      ) +
      facet_grid(
        cols = vars(scenario_lab)
        , labeller = label_wrap_gen(width = 35, multi_line = TRUE)
        # , switch = "both"
      ) +
      scale_fill_manual(values = cols_rdylbu_lt) +
      labs(
        fill = "Level of\nConstraint"
      ) +
        guides(
        fill = guide_legend(reverse = T, override.aes = list(alpha = 0.9))
      )
  # return
  return(plt)
}
# scenario_cnstrnt_map_fn(20)

############################################################
############################################################
### largest patch with fireshed project areas
############################################################
############################################################
# function to highlight largest patch with fireshed project areas under
largest_patch_map_fn <- function(row_n = 1){
    plt <- scenario_cnstrnt_map_fn(row_n) +
      geom_sf(
          data = cnstrnt_class_patches %>%
            dplyr::filter(
              area_name == area_nm_list[row_n]
              & patch_area_rank == 1
              & cnstrnt_class != "med. constraint"
            ) %>%
            sf::st_transform(crs=plt_crs)
          , aes(color = cnstrnt_class, fill = cnstrnt_class), lwd = 1
          , inherit.aes = F
          , alpha = 0
          # , show.legend = F
        ) +
      scale_color_manual(values = cols_rdylbu_dk, drop = F) +
      labs(
        color = "Level of\nConstraint"
        , subtitle = "largest patch of interconnected fireshed project areas by level of mechanical constraint" # largest patch of spatially adjacent...
      ) +
      theme(
        # plot.subtitle = element_text(size = 8)
        plot.subtitle = element_blank()
      ) +
      guides(
        color = "none"
        , fill = guide_legend(reverse = T, override.aes = list(alpha = 0.9, linewidth = 0))
      )
  # return
  return(plt)
}
# scenario_cnstrnt_map_fn(4)
# largest_patch_map_fn(4)

############################################################
############################################################
### patch map with proportion of largest patch
############################################################
############################################################
#### combine patch map with proportion of largest patch
# plot large patch proportion of area horizontal to match map
plt_patch_prop_fn <- function(row_n = 1) {
  # bar plot
  plt_patch_prop_temp <- 
    ggplot(
      data = agg_cnstrnt_class_patches %>%
        dplyr::filter(area_name==area_nm_list[row_n]) %>% 
        dplyr::mutate(scenario_lab = forcats::fct_rev(scenario_lab))
      , mapping = aes(
        y = 1
        , x = max_pct_patch_landscape_hack
        , fill = cnstrnt_class
        , group = cnstrnt_class
      )
    ) +
      geom_col(
        width = 0.7, alpha = 0.9
      ) +
      geom_vline(xintercept = 0, color = "gray55", linetype = "solid", linewidth = 0.7) +
      geom_text(
        mapping = aes(
          label = ifelse(
            max_pct_patch_landscape_hack<0
            , scales::percent(max_pct_patch_landscape, accuracy = 1)
            , ""
          )
          , x = max_pct_patch_landscape_hack #+ .07*sign(max_pct_patch_landscape_hack)
          , fontface = "bold"
        )
        , color = "black", size = 2.6
        , hjust = +1
      ) +
      geom_text(
        mapping = aes(
          label = ifelse(
            max_pct_patch_landscape_hack>=0
            , scales::percent(max_pct_patch_landscape, accuracy = 1)
            , ""
          )
          , x = max_pct_patch_landscape_hack #+ .07*sign(max_pct_patch_landscape_hack)
          , fontface = "bold"
        )
        , color = "black", size = 2.6
        , hjust = 0
      ) +
      # label ha below pct
      geom_text(
        mapping = aes(
          label = ifelse(
            max_pct_patch_landscape_hack<0
            , paste0(
                scales::comma(max_patch_area_ha,suffix = "k", scale = 1e-3, accuracy = 1)
                , " ha"
            )
            , ""
          )
          , x = max_pct_patch_landscape_hack #+ .07*sign(max_pct_patch_landscape_hack)
          , fontface = "bold"
        )
        , color = "black", size = 1.9
        , hjust = +1
        , vjust = 1.9
      ) +
      geom_text(
        mapping = aes(
          label = ifelse(
            max_pct_patch_landscape_hack>=0
            , paste0(
                scales::comma(max_patch_area_ha,suffix = "k", scale = 1e-3, accuracy = 1)
                , " ha"
            )
            , ""
          )
          , x = max_pct_patch_landscape_hack #+ .07*sign(max_pct_patch_landscape_hack)
          , fontface = "bold"
        )
        , color = "black", size = 1.9
        , hjust = 0
        , vjust = 1.9
      ) +
      annotate(
        geom = "text"
        , x = -0.88
        , y = -0.5
        , label = "high constraint"
        , color = cols_rdylbu_lt[3]
        , fontface = "bold"
        , size = 2
      ) +
      annotate(
        geom = "text"
        , x = 0.91
        , y = -0.5
        , label = "low constraint"
        , color = cols_rdylbu_lt[1]
        , fontface = "bold"
        , size = 2
      ) +
      scale_fill_manual(values = cols_rdylbu_lt) +
      scale_x_continuous(
        limits = c(-1.12,1.16)
        , labels = scales::percent_format()
        , position = "bottom"
      ) +
      scale_y_discrete() +
      facet_grid(
        cols = vars(scenario_lab)
        , labeller = label_wrap_gen(width = 35, multi_line = TRUE)
        # , switch = "both"
      ) +
      labs(
        fill = "Level of\nConstraint"
        , subtitle = "" # "percentage of total landscape area comprised by the largest patch" # "Largest Patch % of Landscape Area"
        , x = "Percent of Overall Landscape Area Comprised by the Largest Patch" # "Largest Patch % of Landscape Area"
        , y = ""
      ) +
      theme_light(base_size = 9) +
      theme(
        legend.position = "none" # "top"
        , legend.direction  = "horizontal"
        # , legend.title = element_text(size=7)
        # , axis.title.x = element_text(size=8, face = "bold")
        # , axis.title.y = element_blank()
        # , axis.text.x = element_blank()
        # , axis.ticks.x = element_blank()
        # , panel.grid.major = element_blank()
        # , panel.grid.minor = element_blank()
        # , strip.text = element_text(color = "black", face = "bold", size = 8)
        # , strip.text = element_blank()
        ## for use with base_size in theme_light
        , axis.title.x = element_text(face = "bold")
        , axis.title.y = element_blank()
        , axis.text.x = element_blank()
        , axis.ticks.x = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , strip.text = element_text(color = "black", face = "bold")
        # , strip.text = element_blank()
      ) +
      guides(
        fill = guide_legend(reverse = T, override.aes = list(alpha = 0.9))
      )
  return(plt_patch_prop_temp)
}
# plt_patch_prop_fn(20)
############################################################
############################################################
### distribution of fireshed planning areas
############################################################
############################################################
plt_fshed_cnstrnt_lvl_fn <- function(row_n = 1) {
  
  plt_fshed_cnstrnt_lvl_temp <-
    constrained_by_scnro_ls_pa %>% 
      dplyr::filter(
        area_name==area_nm_list[row_n]
        & pct_pa_intrsct>=0.25
      ) %>% 
      dplyr::group_by(pa_id,scenario_id) %>% 
      dplyr::filter(dplyr::row_number()==1) %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(fireshed_crisis_strategy  %in% c("USFS-Only", "All-Lands")) %>% 
      dplyr::count(scenario_id, scenario_desc, scenario_lab, cnstrnt_class) %>% 
      dplyr::group_by(scenario_desc, scenario_lab) %>% 
      dplyr::mutate(
        pct = n/sum(n)
        , dta_src = "High-Risk Firesheds"
      ) %>% 
      dplyr::bind_rows(
        constrained_by_scnro_ls_pa %>% 
          dplyr::filter(
            area_name==area_nm_list[row_n]
            & pct_pa_intrsct>=0.25
          ) %>% 
          dplyr::group_by(pa_id,scenario_id) %>% 
          dplyr::filter(dplyr::row_number()==1) %>% 
          dplyr::ungroup() %>% 
          dplyr::count(scenario_id, scenario_desc, scenario_lab, cnstrnt_class) %>% 
          dplyr::group_by(scenario_desc, scenario_lab) %>% 
          dplyr::mutate(
            pct = n/sum(n)
            , dta_src = "Overall Landscape Area"
          )
      ) %>% 
      dplyr::mutate(
        dta_src = dta_src %>% forcats::fct_rev()
      ) %>% 
  # plot
    ggplot() +
    geom_col(
      mapping = aes(x = pct, y = scenario_lab, fill=cnstrnt_class)
      ,width = 0.7, alpha=0.8
    ) +
    geom_text(
      mapping = aes(x = pct, y = scenario_lab, group=cnstrnt_class
          ,label = scales::percent(ifelse(pct>=0.07,pct,NA), accuracy = 1)
          , fontface = "bold"
        )
      , position = position_stack(vjust = 0.5)
      , color = "black"
      , size = 2.1
      , vjust = 0
    ) +
    geom_text(
      mapping = aes(x = pct, y = scenario_lab, group=cnstrnt_class
          ,label = ifelse(pct>=0.07,
            paste0("n="
              ,scales::comma(n, accuracy = 1)
            )
            , NA)
        )
      , position = position_stack(vjust = 0.5)
      , color = "black"
      , size = 1.8
      , vjust = 1.5
    ) +
    scale_fill_brewer(type = "div", palette = "RdYlBu", direction = -1) +
    facet_grid(cols = vars(dta_src)) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(
      fill = "Level of\nConstraint"
      , y = ""
      , x = "Percent of Fireshed Project Areas"
      # , caption = nlab_temp
      , title = "Distribution of fireshed project areas"
    ) +
    theme_light(base_size = 9) +
    theme(
      legend.position = "top"
      # , legend.title = element_text(size=8)
      # , axis.title = element_text(size=9)
      # , axis.title.x = element_text(size=8, face = "bold")
      # , axis.text.x = element_text(size = 7)
      # , axis.text.y = element_text(color = "black", face = "bold")
      # , strip.text = element_text(size = 10, color = "black", face = "bold")
      # , plot.caption = element_text(size = 8, face = "bold")
      ##### for use with base_size in theme_light()
      , legend.title = element_text(size=7)
      , axis.title.x = element_text(face = "bold")
      , axis.text.y = element_text(color = "black", face = "bold")
      , strip.text = element_text(color = "black", face = "bold")
    ) + 
    guides(
      fill = guide_legend(reverse = T, override.aes = list(alpha = 0.9,size = 8,linewidth = 0.5))
    )
  return(plt_fshed_cnstrnt_lvl_temp)
}
# plt_fshed_cnstrnt_lvl_fn(7)
############################################################
############################################################
### inset map with all landscapes
############################################################
############################################################
### define function to highlight one landscape
plt_ls_us_state_fn <- function(row_n = 1, use_simple = F) {
  states_temp <- USAboundaries::us_states() %>% 
    sf::st_transform(sf::st_crs(wf_landscapes)) %>% 
    sf::st_filter(wf_landscapes %>% dplyr::filter(area_name==area_nm_list[row_n]))
  cities_temp <- USAboundaries::us_cities(
      states = c(
            # usfs region 1-6 states
            "MT","WY","CO","NM","AZ","UT","ID","WA","OR","CA","NV"
            # , "KS","NE","SD","ND"
          )
      ) %>% 
      sf::st_transform(sf::st_crs(wf_landscapes)) %>% 
      dplyr::filter(
        toupper(city) %in% c(
          "DENVER"
          , "TUSCON"
          , "SALT LAKE CITY"
          , "LAS VEGAS"
          , "SAN DIEGO"
          , "LOS ANGELES"
          , "FRESNO"
          , "SAN FRANCISCO"
          , "SACRAMENTO"
          , "PORTLAND"
          , "SEATTLE"
          , "ALBUQUERQUE"
          , "RENO"
          , "BOISE"
          , "HELENA"
          , "BILLINGS"
        )
        | (toupper(city) == "PHOENIX" & state_abbr == "AZ")
      ) %>% 
      dplyr::filter(
        state_abbr %in% unique(states_temp$stusps)
      )
  plt_simple_temp <- 
    ggplot() + 
      geom_sf(
        data = states_temp
        , fill = "white"
        , color = "gray66"
      ) +
      geom_sf_text(
        data = states_temp
        , mapping = aes(label = stusps)
        , color = "gray66"
        , size = 3
      ) +
      coord_sf(expand = F) +
      theme_void()
  return(
    if(use_simple){
      # plot without cities and names
         plt_simple_temp +
          geom_sf(
            data = wf_landscapes %>% 
              dplyr::filter(area_name==area_nm_list[row_n])
            , fill="navy"
            , color="blue4"
            , alpha = 0.8
            , lwd=0.3
          )
    }else{
      # plot with cities and names
        plt_simple_temp +
          geom_sf(
            data = cities_temp
            , shape = 1
            , size = 1
            , color = "gray44"
          ) +
          geom_sf_text(
            data = cities_temp
            , mapping = aes(label = city)
            , color = "gray44"
            , size = 1.8
            # , hjust = -0.15
            , hjust = -0.05
            , vjust = 0.5
          ) +
          geom_sf(
            data = wf_landscapes %>% 
              dplyr::filter(area_name==area_nm_list[row_n])
            , fill="navy"
            , color="blue4"
            , alpha = 0.8
            , lwd=0.3
          )
    }
  )
}
# plt_ls_us_state_fn(row_n = 14, use_simple = F)
############################################################
############################################################
### inset + spatial framework
############################################################
############################################################
spatial_frmwrk_inset_fn <- function(row_n=1) {
  ##!!!!!!!!!!! must define plt_basemap first
  # plt_basemap <<- landscape_basemap_fn(row_n = row_n)
  spatial_frmwrk_map_temp <- spatial_frmwrk_map_fn(row_n = row_n) +
    theme(plot.caption = element_blank())
  plt_ls_us_state_temp <- plt_ls_us_state_fn(row_n = row_n, use_simple = F) + 
    theme(
      plot.background = element_rect(fill = "white", color = "black")
      , plot.margin = margin(0,0,0,0,"cm")
    )
  # set inset location, size
  if(row_n %in% c(1)){ # upper right for short, wide inset
    left_temp=0.80
    bottom_temp=0.85
    right_temp=1.02
    top_temp=0.99
  } else if(row_n %in% c(9,20)){ # upper left for tall, thin main + short, wide inset
    left_temp=-0.40
    bottom_temp=0.88
    right_temp=0.03
    top_temp=0.99
  } else if(row_n %in% c(11,15)){ # upper left for tall, thin main + wide, wide inset
    left_temp=-0.30
    bottom_temp=0.88
    right_temp=0.10
    top_temp=0.99
  } else if(row_n %in% c(6)){ # upper right for tall, thin inset
    left_temp=0.87
    bottom_temp=0.78
    right_temp=1.01
    top_temp=0.99
  } else{ # upper left
    left_temp=-0.01
    bottom_temp=0.78
    right_temp=0.13
    top_temp=0.99
  }
  spatial_frmwrk_map_temp + 
    patchwork::inset_element(
      plt_ls_us_state_temp
      # location of the plot edges
      , left=left_temp, bottom=bottom_temp, right=right_temp, top=top_temp
      , align_to = "panel"
      , clip = T
    )
  # export for testing
  # ggplot2::ggsave(paste0("C:/Users/georg/Downloads/hey_", row_n, ".jpeg"),width = 10, height = 8, units = "in")
}
# uncomment export in function to test writing
# 1:length(area_nm_list) %>%
#   purrr::map(spatial_frmwrk_inset_fn)
############################################################
############################################################
remove(list = ls()[grep("_temp",ls())])
gc()
############################################################
############################################################
# Objective vs. Treatable Plot
############################################################
############################################################
plt_obj_trtbl_fn <- function(row_n = 1) {
  plt <- 
  constrained_by_scnro_ls_cnstrnt_obj %>% 
    dplyr::filter(
      area_name == area_nm_list[row_n]
      & cnstrnt_class!="high constraint"
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      dplyr::across(
        c(cnstrnt_class)
        , ~ forcats::fct_rev(.x)
      )
    ) %>% 
  # plot
  ggplot(
    mapping = aes(
        x = pct_treatable
        , y = scenario_lab
      )
  ) +
    geom_rect(
      # don't put in aes for discrete scale
        xmin = 0.2, xmax=0.4
        , ymin = constrained_by_scnro_ls_cnstrnt_obj$scenario_lab %>% 
              levels() %>% 
              forcats::fct_match("Scenario 1") %>% 
              which()+0.5
          # constrained_by_scnro_ls_cnstrnt_obj$scenario_lab %>% levels() %>% purrr::pluck(1) -0.5
        , ymax = constrained_by_scnro_ls_cnstrnt_obj$scenario_lab %>% 
              levels() %>% 
              forcats::fct_match("Scenario 3") %>% 
              which()-0.5
      , fill = "gray77"
      , alpha = 0.6
    ) +
    annotate("text"
       , x = 0.5
       , y = constrained_by_scnro_ls_cnstrnt_obj$scenario_lab %>% 
              levels() %>% 
              forcats::fct_match("Scenario 3") %>% 
              which()-0.45
       , label = expression(bold("objective")~bold("20-40%"))
       , size = 2.4, color = "gray65", parse = T
      ) +
    geom_col(
      mapping = aes(fill = cnstrnt_class)
      , color = NA, width = 0.7, alpha = 0.8
    ) +
    geom_text(
      mapping = aes(
        group = cnstrnt_class
        , label = scales::percent(
            ifelse(pct_treatable<.06,NA,pct_treatable)
            , accuracy = 1
          )
        # , fontface = "bold"
      )
      , position = position_stack(vjust = 0.5)
      , color = "black"
      # , size = 2.2
    ) +
    # total
    geom_text(
      data = constrained_by_scnro_ls_cnstrnt_obj %>% 
        dplyr::filter(
          area_name == area_nm_list[row_n]
          & cnstrnt_class!="high constraint"
        ) %>%
        dplyr::group_by(scenario_id, scenario_lab, treatment_objective) %>% 
        dplyr::summarise(pct_treatable_lowmed=sum(pct_treatable))
      , mapping = aes(
        x = pct_treatable_lowmed
        , y = scenario_lab
        , label = scales::percent(pct_treatable_lowmed, accuracy = 1) 
        , color = treatment_objective
        , fontface = "bold"
      )
      # , size = 3.5
      , hjust = -0.1
      , show.legend = F
    ) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(n=3,name="RdYlBu")[2:3]) +
    scale_color_manual(values = c(
      ">20% in\nlow constraint"=cols_obj_r[1]
      ,">20% in\nlow+medium constraint"=cols_obj_r[2]
      , "<20% in\nlow+medium constraint"=cols_obj_r[3])
    ) +
    scale_x_continuous(limits = c(0, .78),labels = scales::percent_format(),position = "bottom") +
    labs(
      fill = "Level of\nConstraint"
      , y = ""
      , x = "Percent of Total High-Risk Area"
      , title = area_nm_list[row_n]
    ) +
    theme_light(base_size = 9) +
    theme(
      legend.position = c(0.91,0.71)
      , legend.direction  = "vertical"
      , legend.title = element_text(size=7)
      , axis.title.x = element_text(face = "bold")
      , axis.text.x = element_blank()
      , axis.ticks.x = element_blank()
      , axis.text.y = element_text(color = "black", face = "bold")
      , panel.grid.major.x = element_blank()
      , panel.grid.minor.x = element_blank()
    ) +
    guides(
      fill = guide_legend(reverse = T, override.aes = list(alpha = 0.9))
    )
  return(plt)
}
# plt_obj_trtbl_fn()
############################################################
############################################################
remove(list = ls()[grep("_temp",ls())])
gc()
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
## TESTING
############################################################
############################################################
# # !!!!!!!!!THIS IS SET GLOBALLY!!!!!!!!!
#   plt_basemap <<- landscape_basemap_fn(row_n = 5)
# # load raster data
# # !!!!!!!!!THIS IS SET GLOBALLY!!!!!!!!!
#   rast_list_temp <<- raster_data_read_fn(row_n = 5)
#   # rast_list_temp <- raster_data_read_fn(row_n = 5)
# # PLOTS
# landscape_basemap_temp <- landscape_basemap_fn(row_n = 5)
# spatial_frmwrk_map_temp <- spatial_frmwrk_map_fn(row_n = 5)
# # rast_area_fn(row_n = 5, layer_name = "is_treatable")
# landcover_map_temp <- landcover_map_fn(row_n = 5, rast_agg_fact = 9)
# protected_map_temp <- protected_map_fn(row_n = 5, rast_agg_fact = 9)
# slopes_map_temp <- slopes_map_fn(row_n = 5, rast_agg_fact = 9)
# roads_map_temp <- roads_map_fn(row_n = 5, rast_agg_fact = 9)
# riparian_map_temp <- riparian_map_fn(row_n = 5, rast_agg_fact = 9)
# administrative_map_temp <- administrative_map_fn(row_n = 5, rast_agg_fact = 9)
# treatable_map_temp <- treatable_map_fn(row_n = 5, rast_agg_fact = 9)
# reduction_table_temp <- reduction_table_fn(row_n = 5)
# title_plot_temp <- title_plot_fn(row_n = 5)
# scenario_cnstrnt_map_temp <- scenario_cnstrnt_map_fn(row_n = 5)
# largest_patch_map_temp <- largest_patch_map_fn(row_n = 5)
# plt_patch_prop_temp <- plt_patch_prop_fn(row_n = 5)
# plt_fshed_cnstrnt_lvl_temp <- plt_fshed_cnstrnt_lvl_fn(row_n = 5)
# #####################################################################
# landscape_basemap_temp
# spatial_frmwrk_map_temp
# landcover_map_temp
# protected_map_temp
# slopes_map_temp
# roads_map_temp
# riparian_map_temp
# administrative_map_temp
# treatable_map_temp
# reduction_table_temp
# title_plot_temp
# scenario_cnstrnt_map_temp
# largest_patch_map_temp
# plt_patch_prop_temp
# plt_fshed_cnstrnt_lvl_temp
# # CLEAN
# remove(list = ls()[grep("_temp",ls())])
# gc()
# 
# 

  purrr::pluck(T)
