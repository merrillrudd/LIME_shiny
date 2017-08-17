library(shiny)
library(shinyBS)
library(LIME) ## download from github
library(shinyFiles)
library(TMB)
library(TMBhelper) ## download from github

shinyUI(fluidPage(
  navbarPage("LIME Application", id="navbar",
  tabPanel("Instructions",
    h3("Welcome to the LIME GUI"),
    h4("To use the LIME application you will need:"),
    tags$ol(
      tags$li("Life history information"),
      tags$li("At least one year of length composition from the fishery"),
      tags$li("Optional: an abundance index time series"),
      tags$li("Optional: at least one year of catch data")
    ),
    h3("Instructions for each step:"),
    h4("Life history inputs"),
      h5("You will need to assume values for:"),
      tags$ul(
        tags$li("von Bertalanffy growth parameters"),
        tags$li("Length at 50% maturity (and 95% maturity if there is information to inform the slope of a logistic function)"),
        tags$li("Length-weight parameters"),
        tags$li("Natural mortality (can be informed using the natural mortality tool: https://github.com/shcaba/Natural-Mortality-Tool"),
        tags$li("Starting values for length at 50% and 95% selectivity (Note: if assuming dome-shaped selectivity, will be able to fix selectivity to some assumed curve but cannot estimate selectivity parameters at this time)")
      ),
      h5("Remember to check:"),
        tags$ul(
          tags$li("Input growth curves and maturity curves"),
          tags$li("Input selectivity curve and specify whether you'd like to estimate or fix it when fitting the model."),
          tags$li("Predicted size at age for the species. If there are gaps between the size-at-age distributions, consider running on a shorter-than-annual time step (e.g. quarterly, monthly, weekly). This may be the case for short-lived species.")
        ),
      h4("Load data"),
      h5("Upload a CSV (comma separated variable) file for each data type and check to make sure input data look correct."),
      h6("Example data files are included. Download the CSV files for examples on how to set up each data type"), 
      h5("Length data format:"),
      tags$ul(
        tags$li("Length data matrix as length frequency (counts), with years along the rows and length bins along the columns."),
        tags$li("Each column must be named with the upper end of the length bin (e.g. 1,2,3,4,...J for 1cm length bins, 2,4,6,8...J for 2cm length bins)."),
        tags$li("Each row must be named with the time step of length frequency data. The name can be in true years (e.g. 2015, 2016, 2017) or indexed years (e.g. 1,2,3). Do not include years with zero counts (i.e. years without length data). The named rows allow LIME to skip years of length data (e.g. 2015, 2017 or 1, 3).")
      ),
      h5("Catch and abundance index data format:"),
      tags$ul(
        tags$li("Row 1 should have the years of each observation of catch or index"),
        tags$li("Row 2 should have the observed catch or index value")
      ),
    h4("Run Model"),
      h5("After checking that the data and life history information are set up correctly, it is time to fit the model."),
      tags$ol(
        tags$li("Click 'Fit model' to run LIME. It may take a few minutes to run, and may take up to an hour if there are more than 20 time steps to be modeled"),
        tags$li("When model run is complete, the model fits for length composition, catch, and abundance index (if included) will appear."),
        tags$li("Check model convergence. You will be looking to make sure the final gradient of each estimated parameter is less than 0.001."),
        tags$li("Examine results.")
      ),
    h4("Sensitivities"),
      h5("Check how sensitive the model results are to:"),
      tags$ul(
        tags$li("Alternate life history inputs"),
        tags$li("Selectivity model structure"),
        tags$li("Excluding portions of the data")
      )
  ),
  tabPanel("Life history", 
    sidebarLayout(
      sidebarPanel(
        uiOutput("InputPars"),
        actionButton("defPars", "Reset Example Parameters", icon("gear"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Life history",
              h4("Adjust growth parameters, natural mortality, length-at-maturity, and length-weight parameters"),
              plotOutput("plotLH")
          ),
          tabPanel("Selectivity",
              h4("Adjust selectivity parameters and model structure"),
              plotOutput("plotSelex")    
          ),
          tabPanel("Size-at-age",
              h4("Check that the distributions of possible lengths for each age overlap."),
              h4("If there is no overlap (possible for short-lived fish), change the number of time steps per year (e.g. 12 for monthly, 4 for quarterly, etc.)"),
              h5("Note: Resolution of length data must be able to match time steps (i.e. length data contains information on the time of year it was collected)."),
              plotOutput("SizeAtAge")
          ))
        
      )
    )
  ),
  tabPanel("Data", 
    sidebarLayout(
      sidebarPanel(
        h4("Choose data CSV files to upload:"),
        fileInput("file1", "Length data",
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                  )),
        fileInput("file2", "Catch data",
                  accept=c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                  )),
        fileInput("file3", "Abundance index",
                  accept=c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                  )),
        bsTooltip(id = "file1", title = "CSV or text file only",
                  placement = "right", trigger = "hover"),
        bsTooltip(id = "file2", title = "CSV or text file only",
                  placement = "right", trigger = "hover"),
        bsTooltip(id = "file3", title = "CSV or text file only",
                  placement = "right", trigger = "hover"),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        h4("Reminder: Each row of the length data and each observation of catch or abundance data should be named with the year of the observation relative to the year for the model."),
        h5("Example: If only one year of length data is available, could name the row 5 if the user wants to model 5 total years with the 5th year having length data.")
        
      ),
      mainPanel(
            plotOutput("plot_LC1"),
            plotOutput("CatchIndex")
        )
      )         
  ),
  tabPanel("Run model", 
        sidebarLayout(
          sidebarPanel(
            shinyDirButton("directory", "Choose results directory", "Create a new folder for saving results."),
            uiOutput("printPath"),
            h4("Data to include in model:"),
            checkboxInput("lc_avail", label="Length data", TRUE),
            checkboxInput("i_avail", label="Abundance index", FALSE),
            checkboxInput("c_avail", label="Catch", FALSE),
            conditionalPanel(
              condition = "input.c_avail==true",
              radioButtons("C_type", label="Catch in biomass or numbers?", choices=c("Biomass", "Numbers"), selected="Biomass")
            ),
            h4("Parameter estimation options:"),
            checkboxInput("est_sigmaR", label="Estimate recruitment variation", TRUE),
            checkboxInput("est_selex", label="Estimate selectivity parameters", TRUE),
            h6("Note: If dome-shaped selectivity was selected on 'Life history' tab, will not estimate selectivity parameters and will fix selectivity to the input selectivity."),
            uiOutput("clickAssess")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Data and fits",
                       plotOutput("plot_LC2"), 
                       plotOutput("CatchIndex2")
                       ),
              tabPanel("Check convergence",
                       tableOutput("checkConverge")),
              tabPanel("Examine results",
                       plotOutput("plotResults"))
            )
          )
        )
      )
  
  
)))