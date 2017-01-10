library(shiny)
library(shinyFiles)

shinyUI(fluidPage(
  titlePanel("Length-based Integrated Mixed Effects (LIME)"),
  fluidRow(
    column(1,
    h4("Inputs"),
	  sliderInput("linf", label = ("Asymptotic length (Linf)"), value = 64, step=1, min=5, max=500),
    sliderInput("vbk", label = ("von Bertalanffy growth (k)"), value = 0.2, step=0.01, min=0.05, max=0.99),
    h6("Lengths at 50% maturity and selectivity must be smaller than Linf"),
	  numericInput("ML50", label = ("Length at 50% maturity"), value = 34, step=1, min=1),
	  numericInput("SL50", label = ("Length at 50% selectivity"), value = 37, step=1, min=1),
	  h4("Histogram control"),
	  sliderInput("binwidth",
	              "Width of length bins:",
	              min = 1,
	              max = 10,
	              value = 1),
	  tags$hr()
	),
	column(1, 
	    numericInput("Nyears", label="Number of years to model", value=20, step=1),
	    h4("Variability"),
	    sliderInput("CVlen", label=("Growth"), value=0.1, step=0.05, min=0, max=1),
	    sliderInput("SigmaR", label=("Recruitment"), value=0.05, step=0.05, min=0.05, max=1),
	    sliderInput("SigmaF", label=("Fishing"), value=0.05, step=0.05, min=0.05, max=1),
	    conditionalPanel(
	      "$('li.active a').first().html()==='Simulation'",
        radioButtons("Fdynamics", label=h4("Fishing pattern"), choices=c("Constant", "Endogenous", "Increasing", "Ramp"), selected = NULL, inline = FALSE,
                        width = NULL),
	      radioButtons("Rdynamics", label=h4("Recruitment pattern"), choices=c("Constant", "Pulsed"), selected = NULL, inline = FALSE,
	                 width = NULL),
        actionButton("saveButton", "Save as Data"),
        textOutput("SaveSimulation")
	    ),
	  conditionalPanel(
	    "$('li.active a').first().html()==='Upload Data'",
		fileInput('file1', 'Choose length composition file to upload',
          accept = c(
            'text/csv',
            'text/comma-separated-values',
            'text/tab-separated-values',
            'text/plain',
            '.csv',
            '.tsv'
            )
        ),
		fileInput('file4', 'Choose effective sample size file to upload',
		          accept = c(
		            'text/csv',
		            'text/comma-separated-values',
		            'text/tab-separated-values',
		            'text/plain',
		            '.csv',
		            '.tsv'
		          )
		),
		fileInput('file2', 'Choose abundance index file to upload',
		          accept = c(
		            'text/csv',
		            'text/comma-separated-values',
		            'text/tab-separated-values',
		            'text/plain',
		            '.csv',
		            '.tsv'
		          )
		),
		 fileInput('file3', 'Choose catch file to upload',
		                    accept = c(
		                      'text/csv',
		                      'text/comma-separated-values',
		                      'text/tab-separated-values',
		                      'text/plain',
		                      '.csv',
		                      '.tsv'
		                    )
		          ),
		radioButtons('sep', 'Separator',
		             c(Comma=',',
		               Semicolon=';',
		               Tab='\t'),
		             ',')
	   ),
	   conditionalPanel(
	    "$('li.active a').first().html()==='Assessment'",
      h4("Variance parameters to estimate"),
      checkboxInput("est_sigR", "Recruitment", TRUE),
      checkboxInput("est_CVL", "Growth", FALSE),
      checkboxInput("est_sigC", "Catch", FALSE),
      checkboxInput("est_sigI", "Index", FALSE),
  		h4("Simulation?"),
  		checkboxInput("simcompare", "Compare to simulated truth", FALSE)
	  )
	),
    tags$hr(),
	
	column(10,
	# mainPanel(
	    tabsetPanel(      
	    tabPanel("Simulation", 
        tabsetPanel(
            tabPanel("Population", plotOutput("DataSimulation", height="auto")),
            tabPanel("Length composition", plotOutput("DataSimulation_LC", height="auto"))
          )
        ),
		  tabPanel("Upload Data", 
		    h3("Check that data has been uploaded correctly"),
		    br(),
		    h4("Files should be CSV or text files."),
          h4("Length composition data should be:"),
		    tags$ul(
           tags$li("A matrix with years along the rows and length bins along the columns")
          ),
		    h4("Preview length composition:"),
		    tableOutput("preview_LCtable"),
		    h4("Preview effective sample size input:"),
		    plotOutput("preview_ESS"),
		    h4("Catch and abundance index data should be:"),
		    tags$ul(
		      tags$li("Vectors with values for each year")
		    ),
        column(5,
        h4("Preview index:"),
        plotOutput("plot_Index")
        ),
        column(5,
          h4("Preview catch:"),
          plotOutput("plot_Catch")   
        )
      ),
# 		  h3(textOutput("datSum"))),
      tabPanel("Assessment",
          tabsetPanel(
            tabPanel("Run model", 
                     column(3, h4("Steps"), tags$ol(
                       tags$li("Ensure that data has been uploaded correctly (Upload Data tab)"), 
                       tags$li("Adjust width of length bins to suit data"), 
                       tags$li("Set values for population parameters"), 
                       tags$li("Click 'Run Assessment'"),
                       tags$li("Visually check model fit to data"),
                       tags$li("Check estimated parameters")
                     ),
                     actionButton("goButton", "Run Assessment"),
                     # checkboxInput("goButton", "Run Assessment", FALSE),
                     textOutput("runLIME"),
                     p("Be patient! Assessment may take a minute to run.")),
                     column(9, plotOutput("LIME_assessment", height="auto")),
                     p("")
                     ),
            tabPanel("Model fits", plotOutput("ModelFits")),
            tabPanel("Estimated parameters", tableOutput("ReportTable")),
            tabPanel("Sensitivities", plotOutput("PlotSensitivities"))
            )

      ),
         id = "tabs1"
	     
      )
   )
  )
# )
))
  