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
# spatial analysis
library(sf)
library(lwgeom)
library(terra)
# set seed
set.seed(11)
# turn off the s2 processing 
## https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
sf::sf_use_s2(FALSE)
remove(list = ls())
gc()
############################################################
############################################################
# Data load
############################################################
############################################################
  ### Load WCS landscapes
  # WCS priority landscape spatial data from the  [USFS Geospatial Data Discovery](https://data-usfs.hub.arcgis.com/datasets/wildfire-crisis-strategy-landscapes-feature-layer/explore) site (accessed 2023-04-04).
  wf_landscapes <- sf::read_sf("./../../wcs_mechanical_constraints/data/Wildfire_Crisis_Strategy_Landscapes/Wildfire_Crisis_Strategy_Landscapes_(Feature_Layer).shp") %>% 
    dplyr::rename_with(tolower) %>% 
    sf::st_make_valid() %>% 
    dplyr::mutate(
      hectares = (as.numeric(sf::st_area(geometry))/10000) 
      , Mil.Hectares = hectares %>% 
        scales::comma(suffix = " M", scale = 1e-6, accuracy = .01)
      , acres = (as.numeric(sf::st_area(geometry))/4046.85642) 
      , Mil.Acres = acres %>% 
        scales::comma(suffix = " M", scale = 1e-6, accuracy = .01)
    ) %>% 
    dplyr::left_join(
      data.frame(state = datasets::state.name, state_abb = datasets::state.abb)
      , by = join_by("state")
    ) %>% 
    dplyr::mutate(
      area_name = paste0(
        ifelse(is.na(state_abb) | state_abb == "", "UNK", state_abb)
        , ": "
        , name
      )
    )
    # manual label placement
    # dplyr::left_join(
    #   readr::read_csv("./../../wcs_mechanical_constraints/data/wildfirepriority_WCS202308/area_label_placement.csv")
    #   , by = dplyr::join_by("area_name")
    # )
  #rename sf geom column
    names(wf_landscapes)[names(wf_landscapes)==tolower(attr(wf_landscapes, "sf_column"))] = "geometry"
    sf::st_geometry(wf_landscapes) = "geometry"
  # set crs
    transform_crs <- sf::st_crs(wf_landscapes)

### Load landscape-level constraint data
# Landscape-level constraint analysis data (i.e., row unique by WCS landscape) was created via this [Google Earth Engine script](https://code.earthengine.google.com/692417f7747e247fc0545824135a0355?noload=true).
  constrained_by_scnro_ls <-
    list.files("./../../wcs_mechanical_constraints/data/wildfirepriority_WCS202308/landscapes/",pattern = "\\.csv$") %>%
    purrr::keep(stringr::str_starts(.,"wfpriority_all_sc")) %>% 
    purrr::map(function(x){
      readr::read_csv(
        paste0("./../../wcs_mechanical_constraints/data/wildfirepriority_WCS202308/landscapes/",x)
        , name_repair = "universal"
        , col_types = cols(.default = "c")
      ) %>% 
      dplyr::rename_with(tolower) %>% 
      dplyr::rename_with(make.names) %>% 
      dplyr::select(state,name,tidyselect::ends_with("_m2"),tidyselect::starts_with("pct_")) %>% 
      dplyr::mutate(scenario_id = stringr::word(x,3,sep="_") %>% readr::parse_number()) %>% 
      dplyr::relocate(scenario_id)
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::inner_join(
      wf_landscapes %>%
        sf::st_drop_geometry() %>%
        dplyr::select(state,name,area_name)
      , by = dplyr::join_by(state,name)
    ) %>%
    dplyr::relocate(area_name,.after = "scenario_id") %>%
    dplyr::group_by(area_name,scenario_id) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::ends_with("_m2")
        , ~ as.numeric(.x) / 10000
      )
      , dplyr::across(
        tidyselect::starts_with("pct_")
        , ~ as.numeric(.x)
      )
    ) %>%
    dplyr::rename_with(
      ~ gsub("_m2", "_ha", .x)
      , tidyselect::ends_with("_m2")
    ) %>%
    # calculate pct reduction
    dplyr::mutate(
      pct_rdctn1_protected = -1*(1 - pct_rmn1_protected)
      , pct_rdctn2_slope = -1*(pct_rmn1_protected - pct_rmn2_slope)
      , pct_rdctn3_roads = -1*(pct_rmn2_slope - pct_rmn3_roads)
      , pct_rdctn4_riparian = -1*(pct_rmn3_roads - pct_rmn4_riparian)
      , pct_rdctn5_administrative = -1*(pct_rmn4_riparian - pct_rmn5_administrative)
      , pct_rdctn_total = -1*(1 - pct_rmn5_administrative)
      , pct_covertype_area = covertype_area_ha/feature_area_ha
      # scenario description
      , scenario_lab = factor(
        scenario_id
        , levels = 1:3
        , labels = paste0("Scenario ", 1:3)
        , ordered = T
      ) %>% forcats::fct_rev()
      , scenario_desc = factor(
        scenario_id
        , levels = 1:3
        , labels = c("Scenario 1\n(status quo)", "Scenario 2\n(slopes+roads)", "Scenario 3\n(slopes+roads+admin)")
        , ordered = T
      )
    )
remove(list = ls()[grep("_temp",ls())])
gc()

### Load fireshed project area data
# The fireshed registry spatial data was obtained from the USFS Geospatial Data Discovery tool for [firesheds](https://data-usfs.hub.arcgis.com/datasets/fireshed-registry-fireshed-feature-layer/explore) and [project areas](https://data-usfs.hub.arcgis.com/datasets/fireshed-registry-project-area-feature-layer/explore) (accessed 2023-05-03).
  fireshed <- sf::st_read("./../../wcs_mechanical_constraints/data/firesheds/Fireshed_Registry3A_Fireshed/Fireshed_Registry%3A_Fireshed_(Feature_Layer).shp") %>%
    sf::st_transform(transform_crs) %>% 
    setNames(c(
        "shape_id"
        , "area_ha"
        , "fireshed_id"
        , "fireshed_name"
        , "fireshed_code"
        , "fireshed_state"
        , "nopas"
        , "objectid"
        , "fshed_id"
        , "exp_total"
        , "exp_usfs"
        , "exp_nonfs"
        , "exp_usfs_protected"
        , "exp_nonfs_protected"
        , "exp_usfs_managed"
        , "exp_nonfs_managed"
        , "exp_usfs_forest"
        , "exp_nonfs_forest"
        , "exp_usfs_nonforest"
        , "exp_nonfs_nonforest"
        , "exp_usfs_conifer"
        , "exp_nonfs_conifer"
        , "exp_usfs_managedforest"
        , "exp_nonfs_managedforest"
        , "exp_usfs_managedconifer"
        , "exp_nonfs_managedconifer"
        , "exp_nonfs_nonconifer_hihaz"
        , "dist_vs"
        , "crisis_strategy"
        , "key_preformance_indicator"
        , "national_usfs_rank"
        , "national_all_land_rank"
        , "regional_usfs_rank"
        , "regional_all_land_rank"
        , "start_date"
        , "end_date"
        , "geometry"
    )) %>% 
    dplyr::mutate(
      exposure_pct_rank = dplyr::percent_rank(exp_total)
      , exposure_pct_rank_grp = dplyr::case_when(
        exposure_pct_rank >= 1-0.01 ~ "Top 1%"
        , exposure_pct_rank >= 1-0.05 ~ "Top 5%"
        , exposure_pct_rank >= 1-0.10 ~ "Top 10%"
        , exposure_pct_rank >= 1-0.25 ~ "Top 25%"
        , TRUE ~ "Bottom 75%"
      ) %>% 
      factor(
        levels = c("Top 1%","Top 5%","Top 10%","Top 25%","Bottom 75%")
        , ordered = T
      )
      # there is also a national_all_land_rank column
      , ntllandrank_pct_rank = dplyr::percent_rank(-national_all_land_rank)
      , ntllandrank_pct_rank_grp = dplyr::case_when(
          ntllandrank_pct_rank >= 1-0.01 ~ "Top 1%"
          , ntllandrank_pct_rank >= 1-0.05 ~ "Top 5%"
          , ntllandrank_pct_rank >= 1-0.10 ~ "Top 10%"
          , ntllandrank_pct_rank >= 1-0.25 ~ "Top 25%"
          , TRUE ~ "Bottom 75%"
        ) %>% 
        factor(
          levels = c("Top 1%","Top 5%","Top 10%","Top 25%","Bottom 75%")
          , ordered = T
        )
      , crisis_strategy = ifelse(is.na(crisis_strategy),"Not High Risk",crisis_strategy) %>% 
        as.factor() %>% 
        forcats::fct_shift()
    )
    #rename sf geom column
      names(fireshed)[names(fireshed)==tolower(attr(fireshed, "sf_column"))] = "geometry"
      sf::st_geometry(fireshed) = "geometry"
      # calculate area
      fireshed <- fireshed %>% 
        dplyr::mutate(
          fireshed_area_ha = as.numeric(sf::st_area(geometry))/10000
          , fireshed_area_acres = (fireshed_area_ha*10000)/4046.85642
        )
  ## fireshed_proj_area spatial data
  fireshed_proj_area <- sf::st_read("./../../wcs_mechanical_constraints/data/firesheds/Fireshed_Registry3A_Project_Area/Fireshed_Registry%3A_Project_Area_(Feature_Layer).shp") %>%
    sf::st_transform(transform_crs) %>% 
    setNames(c(
        "shape_id"
        , "fireshed_id"
        , "pa_id"
        , "pa_area_ha"
        , "objectid"
        , "pa_id2"
        , "fshed_id"
        , "exp_total"
        , "exp_usfs"
        , "exp_nonfs"
        , "exp_usfs_protected"
        , "exp_nonfs_protected"
        , "exp_usfs_managed"
        , "exp_nonfs_managed"
        , "exp_usfs_forest"
        , "exp_nonfs_forest"
        , "exp_usfs_nonforest"
        , "exp_nonfs_nonforest"
        , "exp_usfs_conifer"
        , "exp_nonfs_conifer"
        , "exp_usfs_managedforest"
        , "exp_nonfs_managedforest"
        , "exp_usfs_managedconifer"
        , "exp_nonfs_managedconifer"
        , "exp_nonfs_nonconifer_hihaz"
        , "dist_vs"
        , "pctrecentlydisturbed"
        , "start_date"
        , "end_date"
        , "geometry"
    )) %>% 
    dplyr::mutate(
      exposure_pct_rank = dplyr::percent_rank(exp_total)
      , exposure_pct_rank_grp = dplyr::case_when(
        exposure_pct_rank >= 1-0.01 ~ "Top 1%"
        , exposure_pct_rank >= 1-0.05 ~ "Top 5%"
        , exposure_pct_rank >= 1-0.10 ~ "Top 10%"
        , exposure_pct_rank >= 1-0.25 ~ "Top 25%"
        , TRUE ~ "Bottom 75%"
      ) %>% 
      factor(
        levels = c("Top 1%","Top 5%","Top 10%","Top 25%","Bottom 75%")
        , ordered = T
      )
    )
    #rename sf geom column
      names(fireshed_proj_area)[names(fireshed_proj_area)==tolower(attr(fireshed_proj_area, "sf_column"))] = "geometry"
      sf::st_geometry(fireshed_proj_area) = "geometry"
      # calculate area
      fireshed_proj_area <- fireshed_proj_area %>% 
        dplyr::mutate(
          pa_area_ha = as.numeric(sf::st_area(geometry))/10000
          , pa_area_acres = (pa_area_ha*10000)/4046.85642
        ) %>% 
        # JOIN WITH FIRESHED DATA
        dplyr::inner_join(
          fireshed %>%
            sf::st_drop_geometry() %>%
            dplyr::select(fireshed_id, crisis_strategy, exp_total
                          , exposure_pct_rank, exposure_pct_rank_grp
            ) %>% 
            dplyr::rename(exposure_total=exp_total) %>% 
            dplyr::rename_with(
              ~ paste0("fireshed_",.x)
              , -c(fireshed_id)
            )
          , by = dplyr::join_by(fireshed_id)
        ) %>%
        dplyr::select(pa_id,pa_area_ha
                      ,exp_total,exposure_pct_rank,exposure_pct_rank_grp
                      , tidyselect::starts_with("fireshed_")
        ) %>% 
        dplyr::rename(exposure_total=exp_total) %>%
        dplyr::rename_with(
          ~ paste0("pa_", .x, recycle0 = TRUE)
          , tidyselect::starts_with("exp")
        )
### load fireshed constraint scenario data
# Fireshed project area constraint analysis data (i.e., row unique by WCS landscape + f.p.a.) was created via this [Google Earth Engine script](https://code.earthengine.google.com/003e3994cfc4c469d52d224225a4b109?noload=true)
  constrained_by_scnro_ls_pa <-
    list.files("./../../wcs_mechanical_constraints/data/wildfirepriority_WCS202308/fireshed/",pattern = "\\.csv$") %>%
    purrr::keep(stringr::str_starts(.,"wfpriority_fireshed_sc")) %>% 
    purrr::map(function(x){
      readr::read_csv(
        paste0("./../../wcs_mechanical_constraints/data/wildfirepriority_WCS202308/fireshed/",x)
        , name_repair = "universal"
        , col_types = cols(.default = "c")
      ) %>% 
      dplyr::rename_with(tolower) %>% 
      dplyr::rename_with(make.names) %>% 
      dplyr::select(state,name,pa_id,tidyselect::ends_with("_m2"),tidyselect::starts_with("pct_")) %>% 
      dplyr::mutate(scenario_id = stringr::word(x,3,sep="_") %>% readr::parse_number()) %>% 
      dplyr::relocate(scenario_id)
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::inner_join(
      wf_landscapes %>%
        sf::st_drop_geometry() %>%
        dplyr::select(state,name,area_name)
      , by = dplyr::join_by(state,name)
    ) %>%
    dplyr::relocate(area_name,.after = "scenario_id") %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::ends_with("_m2")
        , ~ as.numeric(.x) / 10000
      )
      , dplyr::across(
        tidyselect::starts_with("pct_")
        , ~ as.numeric(.x)
      )
    ) %>%
    dplyr::rename_with(
      ~ gsub("_m2", "_ha", .x)
      , tidyselect::ends_with("_m2")
    ) %>%
    dplyr::group_by(area_name,pa_id,scenario_id) %>%
    dplyr::arrange(desc(pct_pa_intrsct)) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::ungroup() %>%
    dplyr::filter(pct_pa_intrsct>=0.00001) %>%
    # calculate pct reduction
    dplyr::mutate(
      pct_rdctn1_protected = -1*(1 - pct_rmn1_protected)
      , pct_rdctn2_slope = -1*(pct_rmn1_protected - pct_rmn2_slope)
      , pct_rdctn3_roads = -1*(pct_rmn2_slope - pct_rmn3_roads)
      , pct_rdctn4_riparian = -1*(pct_rmn3_roads - pct_rmn4_riparian)
      , pct_rdctn5_administrative = -1*(pct_rmn4_riparian - pct_rmn5_administrative)
      , pct_rdctn_total = -1*(1 - pct_rmn5_administrative)
      , pct_covertype_area = covertype_area_ha/feature_area_ha
      # calculate level of constraint
        , cnstrnt_lvl = dplyr::case_when(
            -pct_rdctn_total > 0.8 ~ 1
            , -pct_rdctn_total >= 0.6 ~ 2
            , -pct_rdctn_total >= 0.0 ~ 3
          )
        , cnstrnt_class = factor(
            cnstrnt_lvl 
            , levels = 1:3
            , labels = c("high constraint", "med. constraint", "low constraint")
            , ordered = T
          ) %>% forcats::fct_rev()
        , rmn_cnstrnt_class = factor(
            cnstrnt_lvl 
            , levels = 1:3
            , labels = c("0–19% treatable", "20–40% treatable", ">40% treatable")
            , ordered = T
          ) %>% forcats::fct_rev()
        # scenario description
        , scenario_lab = factor(
            scenario_id
            , levels = 1:3
            , labels = paste0("Scenario ", 1:3)
            , ordered = T
          ) %>% forcats::fct_rev()
        , scenario_desc = factor(
          scenario_id
          , levels = 1:3
          , labels = c("Scenario 1\n(status quo)", "Scenario 2\n(slopes+roads)", "Scenario 3\n(slopes+roads+admin)")
          , ordered = T
        )
    ) %>% 
    # join with fireshed data
    dplyr::inner_join(
      fireshed_proj_area %>%
        sf::st_drop_geometry() %>%
        dplyr::select(pa_id, tidyselect::starts_with("pa_exposure"), tidyselect::starts_with("fireshed_")) %>%
        dplyr::mutate(pa_id=as.character(pa_id))
      , by = dplyr::join_by(pa_id)
    )
##############################################
##############################################
# list of area names to work over
area_nm_list <- sort(unique(constrained_by_scnro_ls$area_name))
remove(list = ls()[grep("_temp",ls())])
gc()
### Patch Data
# create spatial data of interconnected patches by constraint class
# this is to represent the spatial aggregation of fireshed project areas of similar levels of  mechanical constraint
  # aggregation = Tendency of patch or land-cover types to be spatially adjacent or in close proximity
  cnstrnt_class_patches <-
    fireshed_proj_area %>% 
        dplyr::mutate(pa_id=as.character(pa_id)) %>% 
        dplyr::inner_join(
          constrained_by_scnro_ls_pa %>% 
            dplyr::filter(
             pct_pa_intrsct>=0.25
            ) %>% 
            dplyr::select(pa_id, area_name, scenario_id, scenario_lab, cnstrnt_class)
          , by = dplyr::join_by(pa_id)
          , multiple = "all"
        ) %>% 
    # union vectors/polygons by constraint class into one multipolygon
      # this dissolves inner boundaries by constraint class
    dplyr::group_by(area_name, scenario_id, scenario_lab, cnstrnt_class) %>% 
    dplyr::summarize(
      geometry = sf::st_union(geometry, by_feature = F)
    ) %>% 
    # separate a multipolygon geometry into several polygons objects after performing a st_union()
    sf::st_cast(to = "MULTIPOLYGON") %>%
    st_cast(to = "POLYGON") %>% 
    # calculate patch area 
    dplyr::ungroup() %>% 
    dplyr::group_by(area_name, scenario_id, scenario_lab, cnstrnt_class) %>% 
    dplyr::mutate(
      patch_area_ha = as.numeric(sf::st_area(geometry))/10000
      , patch_area_rank = dplyr::dense_rank(dplyr::desc(patch_area_ha))
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(scenario_id, area_name, cnstrnt_class, dplyr::desc(patch_area_ha)) %>% 
    # calculate proportion of total area in each patch
    dplyr::inner_join(
      # total area of fireshed project areas included for analysis
        # after union to remove internal boundaries
      fireshed_proj_area %>% 
        dplyr::mutate(pa_id=as.character(pa_id)) %>% 
        dplyr::inner_join(
          constrained_by_scnro_ls_pa %>% 
            dplyr::filter(
              scenario_id==1
              & pct_pa_intrsct>=0.25
            ) %>% 
            dplyr::select(pa_id, area_name)
          , by = dplyr::join_by(pa_id)
          , multiple = "all"
        ) %>% 
        dplyr::group_by(area_name) %>% 
        dplyr::summarize(
          geometry = sf::st_union(geometry, by_feature = F)
        ) %>%
        dplyr::mutate(
          total_area_ha = as.numeric(sf::st_area(geometry))/10000
        ) %>% 
        sf::st_drop_geometry()
      , by = dplyr::join_by(area_name)
    ) %>% 
    dplyr::mutate(
      pct_patch_landscape = patch_area_ha / total_area_ha
    )
  # aggregate to constraint class level
  agg_cnstrnt_class_patches <- cnstrnt_class_patches %>% 
    sf::st_drop_geometry() %>% 
    dplyr::group_by(area_name, scenario_lab, cnstrnt_class) %>% 
    dplyr::summarize(
      max_patch_area_ha = max(patch_area_ha, na.rm = T)
      , mean_patch_area_ha = mean(patch_area_ha, na.rm = T)
      , max_pct_patch_landscape = max(pct_patch_landscape, na.rm = T)
      , sum_pct_patch_landscape = sum(pct_patch_landscape)
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(
      max_pct_patch_landscape_hack = dplyr::case_when(
        cnstrnt_class=="high constraint" ~ -max_pct_patch_landscape
        , TRUE ~ max_pct_patch_landscape
      )
    ) %>% 
    dplyr::filter(
      cnstrnt_class!="med. constraint"
    )
