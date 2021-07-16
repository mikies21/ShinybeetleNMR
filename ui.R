#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load libraries ----------------------------------------------------------


library(shiny)
library(shinythemes)
library(shinydashboard)
library(datamods)
library(tidyverse)
library(shinyWidgets)
library(shinydashboardPlus)
library(mixOmics)
library(markdown)

# load functions ----------------------------------------------------------

source('Functions/NMRMetab_anova.R')
#source('Functions/NMRMetab_average_prediction_metrics.R')
#source('Functions/NMRMetab_binning.R')
#source('Functions/NMRMetab_CRS.R')
#source('Functions/NMRMetab_foldchange.R')
source('Functions/NMRmetab_normandscale.R')
source('Functions/NMRMetab_PCA.R')
source('Functions/NMRmetab_plot.R')
#source('Functions/NMRMetab_readBruker.R')
source('Functions/NMRMetab_UnivariateTest.R')





# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(options = list(sidebarExpandOnHover = TRUE),
                skin = 'black',
                header = dashboardHeader(title = "ShinybeetlesNMR"), 
                sidebar = dashboardSidebar(collapsed = F,
                                           width = 350,minified = F,
                                           sidebarMenu(id = 'tabs', 
                                                       menuItem(text = 'Introduction', tabName = "introduction", icon = icon("bullhorn")),
                                                       menuItem(text = 'Load Data', tabName = "dashboard", icon = icon("table"))),
                                           fileInput(inputId = 'fileUpload' ,label = 'upload NMR data', multiple = F, accept = '.csv',),
                                           sliderInput("metadata_index", label = "group annotations columns", min = 1, max = 10, step = 1, value = 8),
                                           prettyToggle("norm_before", 
                                                        label_on = h5("normalise data before filtering values.", br(), "ie normalise on full dataset"),
                                                        label_off = "normalise data after filtering values.",
                                                        value = T, 
                                                        status_on = 'primary', 
                                                        status_off = 'warning',
                                                        shape = "curve"),
                                           pickerInput("normalisation", label = "data normalisation", choices = c("None", "PQN", "TotArea")),
                                           pickerInput("scaling", label = "data scaling", choices = c("None", "Pareto", "Range")),
                                           sidebarMenu(id = 'tabs', 
                                                       menuItem(text = 'Univariate', tabName = 'univariate', icon = icon("dice-one")),
                                                       menuItem(text = 'PCA', tabName = 'PCA', icon = icon('dice')),
                                                       menuItem(text = 'PLSDA',tabName = 'PLSDA', icon = icon('angellist'),
                                                                menuSubItem('plots', tabName = 'PLSplots'),
                                                                menuSubItem('test and train', tabName = 'PLS_ML')))),
                body = dashboardBody(tabItems(tabItem(tabName = 'introduction',
                                                      includeMarkdown("frontpage.Rmd")),
                                              tabItem(tabName = 'dashboard',
                                                      fluidRow(infoBoxOutput("textUpload", width = 6),
                                                               infoBoxOutput('ns_info', width = 6)),
                                                      fluidRow(box(filter_data_ui("filtering_data"),width = 2,collapsible = T),
                                                               box(title = 'dataframe',
                                                                   style = "overflow-x: scroll;",
                                                                   collapsible = T,
                                                                   width = 5,
                                                                   downloadButton('download_data_ns', label = 'dowload data as csv'),
                                                                   br(),
                                                                   DT::dataTableOutput('df1')),
                                                               box(title = 'Plot spectra',
                                                                   status = 'success',
                                                                   width = 5,
                                                                   fluidRow(column(6, dropdownButton()),
                                                                            column(6, uiOutput('spectra_groupUI'))),
                                                                   plotOutput('spectra_plot_scaled')))),
                                              tabItem(tabName = 'univariate',
                                                      fluidRow(column(3,
                                                                      uiOutput('univ_groupUI'),
                                                                      infoBoxOutput('textUniv',12),
                                                                      infoBoxOutput('univ_type', 12),
                                                                      uiOutput('univariateUI')),
                                                               box(style = "overflow-x: scroll;",
                                                                   width = 9,
                                                                   DT::dataTableOutput('univ_table')))),
                                              tabItem(tabName = 'PCA',
                                                      fluidRow(column(2,
                                                                      uiOutput('PCA_groupUI'),
                                                                      radioButtons('PCAcorr', label = 'correlation plot', choices = c('simple', 'corrplot'),selected = 'simple', inline = T),
                                                                      uiOutput('PCA_ellipsesUI'),
                                                                      numericInput('pcx', label = 'x', value = 1, min = 1, max = 20, step = 1),
                                                                      numericInput('pcy', label = 'y', value = 2, min = 1, max = 20, step = 1)),
                                                               box(title = 'Principal component analysis', 
                                                                   width = 5,
                                                                   plotOutput('PCA_scores', brush = "PCA_scores_brush")),
                                                               box(title = 'loadings',
                                                                   width = 5,
                                                                   plotOutput('PCA_loading', brush = "PCA_loading_brush"))),
                                                      fluidRow(box(title = 'brushed PCA',
                                                                   style = "overflow-x: scroll;", 
                                                                   collapsible = T,
                                                                   collapsed = T,
                                                                   DT::dataTableOutput('PCA_info')),
                                                               box(title = 'loading boxplots',
                                                                   collapsible = T, 
                                                                   collapsed = T,
                                                                   plotOutput('boxplots_loadings')))),
                                              tabItem(tabName = 'PLSplots',
                                                      fluidRow(column(2,
                                                                      uiOutput('PLS_groupUI'),
                                                                      numericInput('compx', label = 'x', value = 1, min = 1, max = 20, step = 1),
                                                                      numericInput('compy', label = 'y', value = 2, min = 1, max = 20, step = 1)),
                                                               box(title = 'PLS-DA score plot', 
                                                                   width = 5, 
                                                                   dropdownButton(circle = T,status = 'warning', size = 'xs',
                                                                                  awesomeCheckbox(inputId = "PLS_ellipse",
                                                                                                  label = "add elipsess", 
                                                                                                  value = F)),
                                                                   plotOutput('PLS_scores')),
                                                               box(title = 'PLS_DA variables',
                                                                   width = 5,
                                                                   plotOutput('PLS_plotvar')))),
                                              tabItem(tabName = 'PLS_ML',
                                                      box(title = 'PLS settings',
                                                          width = 2, 
                                                          uiOutput('PLS_group_trainUI'),
                                                          sliderInput('testtrainsplit', label = 'spit test and train', min = 0, max = 100, value = 70, step = 1),
                                                          numericInput('ncomp', label = 'number of components', value = 3, min = 0, max = 20, step = 1),
                                                          selectInput('CV_validation', label = 'type of CV', choices = c('loo', 'Mfold'), selected = 'loo', multiple = F)),
                                                      box(title = 'CV plot', 
                                                          width = 5,
                                                          plotOutput('CV_plot'))))))
)
