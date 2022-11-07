# library(shiny)
# library(bslib)
# library(shinyWidgets)
# library(shinyjs)
# library(reactable)
#
# library(dplyr)
# library(purrr)
# library(tibble)
# library(stringr)
# library(forcats)
# library(ggplot2)
# library(tidyselect)
# library(tidytext)
#
# library(cluster)
# library(broom)
# library(parameters)
# library(datawizard)
# library(NbClust)
#
# library(vroom)
# library(readxl)


# ------------------------------------------------------------------------------
plt_clr <- list(bars = "#343A40",
                dash = c("burlywood4", "blue1", "darkgoldenrod3", "seagreen3",
                         "firebrick3", "deeppink2", "gray33", "darkorange2",
                         "royalblue2",  "springgreen4"))

stat_label <- list(mean = "Average",
                   sum  = "Total",
                   min  = "Minimum",
                   max  = "Maximum",
                   quantile_25 = "25th Quantile",
                   quantile_75 = "75th Quantile")

# Colors -----------------------------------------------------------------------
# _BC == background-color
# _BD == border-color
# _TX == text-color

spinner_color <- "#CED4DA"

tbl_header_BC <- "#DEE2E6"
tbl_header_BD <- "#212529"
tbl_body_BC <- "#F8F9FA"

cs_col_BC <- "#343A40"
cs_col_TX <- "#CED4DA"


plt_plot_BC <- "#F8F9FA"



# pal <- c("#F8F9FA", "#E9ECEF", "#DEE2E6", "#CED4DA", "#ADB5BD",
#          "#6C757D", "#495057", "#343A40", "#212529")


# font -------------------------------------------------------------------------
tbl_font_family <- "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica,
                    Arial, sans-serif"
