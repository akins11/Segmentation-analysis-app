library(shiny)
library(shinythemes)
library(shinyWidgets)
library(reactable)

library(cluster)
library(tidyverse)
library(broom)
library(lubridate)
library(parameters)
library(datawizard)
library(NbClust)
library(shinyjs)
library(vroom)

# ------------------------------------------------------------------------------
plt_clr <- list(bars = "gray32", 
                dash = c("burlywood4", "blue1", "darkgoldenrod3", "seagreen3", 
                         "firebrick3", "deeppink2", "gray33", "darkorange2", 
                         "royalblue2",  "springgreen4"))

stat_label <- list(mean = "Average",
                   sum  = "Total",
                   min  = "Minimum",
                   max  = "Maximum",
                   quantile_25 = "25th Quantile",
                   quantile_75 = "75th Quantile")

