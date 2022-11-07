# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.package('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "load_data", with_test = TRUE) # Name of the module
golem::add_module(name = "select_variables", with_test = TRUE) # Name of the module
golem::add_module(name = "run_cluster_analysis", with_test = TRUE)
golem::add_module(name = "cluster_summary", with_test = TRUE)
golem::add_module(name = "assign_segments", with_test = TRUE)
golem::add_module(name = "segmentaion_summary", with_test = TRUE)

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("load_data", with_test = TRUE)
golem::add_fct("run_cluster_analysis", with_test = TRUE)
golem::add_fct("cluster_summary", with_test = TRUE)
golem::add_fct("assign_segments", with_test = TRUE)
golem::add_fct("segmentaion_summary", with_test = TRUE)
golem::add_fct("react_table", with_test = TRUE)

golem::add_utils("helpers", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "demo_data", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("segmentationAnalysisApp")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

usethis::use_package("shiny")
usethis::use_package("shinyWidgets")
usethis::use_package("shinyjs")
usethis::use_package("shinycssloaders")
usethis::use_package("bslib")
usethis::use_package("reactable")
usethis::use_package("reactablefmtr")

usethis::use_package("dplyr")
usethis::use_package("purrr")
usethis::use_package("tibble")
usethis::use_package("stringr")
usethis::use_package("forcats")
usethis::use_package("ggplot2")
usethis::use_package("ggtext")
usethis::use_package("rlang")
usethis::use_package("tidyselect")
usethis::use_package("readxl")
usethis::use_package("vroom")

usethis::use_package("cluster")
usethis::use_package("parameters")
usethis::use_package("datawizard")
usethis::use_package("NbClust")
usethis::use_package("tidytext")
usethis::use_package("broom")

usethis::use_package("scales")
usethis::use_package("janitor")



# usethis::use_package("fontawesome")


# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
