############################################################
############################################################
# load data and functions
############################################################
############################################################
# load data
source("_01_infopkt_data.R")
# load functions
source("_02_infopkt_pltfns.R")
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
library(rmarkdown) #rmarkdown
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
remove(list = ls()[grep("_temp",ls())])
gc()
########################################################
############################################################
# Parameterized reports
# see: https://bookdown.org/yihui/rmarkdown-cookbook/parameterized-reports.html
############################################################
############################################################
# create dir for pdf plots
  hey_dir <- "../data/pdf"
  if(dir.exists(hey_dir)==FALSE){
    dir.create(hey_dir)
  }else{ # delete all files if folder exists
    file.remove(list.files(hey_dir, full.names = TRUE))
  }
# function to render report
render_report <- function(area_num) {
  # assuming the output format of input.Rmd is PDF
  rmarkdown::render(
    "index.Rmd"
    , params = list(
      area_num = area_num
      , area_name = paste0(
          wf_landscapes %>% 
            sf::st_drop_geometry() %>% 
            dplyr::filter(area_name == area_nm_list[area_num]) %>% 
            dplyr::pull(name)
          , " ("
          , wf_landscapes %>% 
            sf::st_drop_geometry() %>% 
            dplyr::filter(area_name == area_nm_list[area_num]) %>% 
            dplyr::pull(state)
          , ")"
        )
    )
    , output_file = paste0(
          hey_dir
          , "/"
          # remove spaces from area name for file naming
          , area_nm_list[area_num] %>% stringr::str_remove_all("[^[A-Za-z,]]")
          , '.pdf'
        )
    , envir = parent.frame()
  )
}
# 1:length(area_nm_list) %>% 
# c(4,11,12) %>% 
c(4,12) %>% 
  purrr::map(render_report)


