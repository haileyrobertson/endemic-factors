######################## LOAD R PACKAGES #######################################

################################################################################
#
#' R packages needed to run any/most {targets} workflows
#
################################################################################

library(targets)
library(geotargets)
library(tarchetypes)
library(tidyverse)
library(here)
library(knitr)
library(rmarkdown)
library(paws)
library(conflicted)
library(curl)

################################################################################
#
#' Additional R packages needed to run your specific workflow
#
################################################################################
library(tidyr)
library(dplyr)
library(rnaturalearth)
library(countrycode)
library(lubridate)
library(ggplot2)
library(ggnewscale)
library(sf)
library(paletteer)
