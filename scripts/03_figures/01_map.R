################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# Description
#
################################################################################
  
# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
pacman::p_load(here,
               tidyverse,
               sf)

## Load data -------------------------------------------------------------------
# This contains the vertices for all the polygons. You will need to convert them to actual polygons with SF, or 
# be creative in how you represent them (e.g. see ?geom_polygon)
data <- read_csv("https://github.com/jcvdav/MAREAmanuscript/raw/refs/heads/master/Data/Spatial/baja_coordinates.csv")


# PROCESSING ###################################################################

## Some step -------------------------------------------------------------------


# VISUALIZE ####################################################################

## Another step ----------------------------------------------------------------


# EXPORT #######################################################################

## Export to a file called map.png using a 9X6 aspect ratio --------------------
  

