library(shiny)

shinyUI(fluidPage(
  titlePanel("Length-based Integrated Mixed Effects (LIME)"),
  fluidRow(
    column(1,
      h4("Life history parameters"),
	  numericInput("linf", label = ("Asymptotic length (Linf)"), value = 64, step=1, min=10),
    numericInput("vbk", label = ("von Bertalanffy growth (k)"), value = 0.2, step=0.05, min=0.05),
	  numericInput("ML50", label = ("Length at 50% maturity"), value = 34, step=1, min=1),
    numericInput("lwa", label= ("Length-weight scaling"), value=0.025, step=0.005, min=0.005),
    numericInput("lwb", label=("Length-weight allometric"), value=3, step=0.005, min=2),
    h4("Starting values for estimated parameters"),
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
	    h4("Variability"),
	    sliderInput("CVlen", label=("Growth"), value=0.1, step=0.05, min=0, max=1),
	    sliderInput("SigmaR", label=("Recruitment"), value=0.6, step=0.01, min=0, max=1),
	    sliderInput("SigmaF", label=("Fishing"), value=0.2, step=0.01, min=0, max=1),
	    conditionalPanel(
	      "$('li.active a').first().html()==='Simulation'",
        radioButtons("Fdynamics", label=h4("Fishing pattern"), choices=c("Constant", "Endogenous", "Increasing", "Ramp"), selected = NULL, inline = FALSE,
                        width = NULL),
	      radioButtons("Rdynamics", label=h4("Recruitment pattern"), choices=c("Constant"), selected = NULL, inline = FALSE,
	                 width = NULL)
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
        checkboxInput('header', 'Header LC', FALSE),
		fileInput('file2', 'Choose abundance index file to upload',
		          accept = c(
		            'text/csv',
		            'text/comma-separated-values',
		            'text/tab-separated-values',
		            'text/plain',
		            '.csv',
		            '.tsv'
		          ),
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
		checkboxInput('header_time', "Header time series", FALSE),
		radioButtons('sep', 'Separator',
		             c(Comma=',',
		               Semicolon=';',
		               Tab='\t'),
		             ',')
	   ),
	   conditionalPanel(
	    "$('li.active a').first().html()==='Assessment'",
		br()
	  )
	),
    tags$hr(),
	
	column(10,
	mainPanel(
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
		    tableOutput("head_LC"),
        
		    h4("Catch and abundance index data should be:"),
		    tags$ul(
		      tags$li("Vectors with values for each year")
		    ),
        column(5,
        h4("Preview index:"),
        tableOutput("head_Index")
        ),
        column(5,
          h4("Preview catch:"),
          tableOutput("head_Catch")   
        )
      ),
# 		  h3(textOutput("datSum"))),
      tabPanel("Assessment",
          tabsetPanel(
            tabPanel("Steps", 
                     tags$ol(
                       tags$li("Ensure that data has been uploaded correctly (Upload Data tab)"), 
                       tags$li("Adjust width of length bins to suit data"), 
                       tags$li("Set values for biological parameters"), 
                       tags$li("Set starting values for parameters to be estimated"),
                       tags$li("Click 'Run Assessment'"),
                       tags$li("Visually check model fit to data"),
                       tags$li("Check estimated parameters")
                     ),
                     actionButton("goButton", "Run Assessment"),
                     p("")
                     )
            )

      ),
         id = "tabs1"
	     
      )
   )
  )
)
))
  
  # , # End simulation tab 
  # # Assessment tab 
    # tabPanel("Assessment",
   # fluidRow(
    # column(2,
      # h4("Life history parameters"),
      # numericInput("MK2", label = ("M/K ratio"), value = 1.5, step=0.05, min=0.1, max=10),
	  # numericInput("Linf2", label = ("Linf"), value = 100, step=1, min=10),
	   # h4("Maturity-at-length"),
	  # numericInput("L502", label = ("L50"), value = 66, step=1, min=1),
	  # numericInput("L952", label = ("L95"), value = 70, step=1, min=1),
	  # sliderInput("steepness",
                  # h4("Steepness"),
                  # min = 0.2,
                  # max = 1,
                  # value = 1,
				  # step=0.01),
	  # tags$hr()
	  # ),
	  # column(2,
	  # h4("Histogram control"),
	  # sliderInput("binswidth2",
                  # "Width of length bins:",
                  # min = 1,
                  # max = 10,
                  # value = 2),
	  # tags$hr(),
 	  # fileInput('file1', 'Choose file to upload',
                # accept = c(
                  # 'text/csv',
                  # 'text/comma-separated-values',
                  # 'text/tab-separated-values',
                  # 'text/plain',
                  # '.csv',
                  # '.tsv'
                # )
      # ),
	  # checkboxInput('header', 'Header', FALSE),
      # radioButtons('sep', 'Separator',
                   # c(Comma=',',
                     # Semicolon=';',
                     # Tab='\t'),
                   # ','),
      # tags$hr()
	 
    # ),
    # column(7, 
      # plotOutput("SPRSAssessment")
	# )
  # )
  # )
# ))


# # library(shiny)

# # shinyUI(fluidPage(
  # # titlePanel("Length-based SPR"),
  # # sidebarLayout(
    # # sidebarPanel(
      # fileInput('file1', 'Choose file to upload',
                # accept = c(
                  # 'text/csv',
                  # 'text/comma-separated-values',
                  # 'text/tab-separated-values',
                  # 'text/plain',
                  # '.csv',
                  # '.tsv'
                # )
      # ),
      # checkboxInput('header', 'Header', FALSE),
      # radioButtons('sep', 'Separator',
                   # c(Comma=',',
                     # Semicolon=';',
                     # Tab='\t'),
                   # ','),
      # tags$hr(), 
	  # # sliderInput("binswidth",
                  # # "Width of length bins:",
                  # # min = 1,
                  # # max = 30,
                  # # value = 2),
	  # # tags$hr(), 
	  # # h2("Life history parameters"),
      # # # Copy the line below to make a slider bar 
      # # numericInput("MK", label = ("M/K ratio"), value = 1.5, step=0.05, min=0.1, max=10),
	  # # numericInput("Linf", label = ("Linf"), value = 100, step=1, min=10),
	  # # numericInput("L50", label = ("L50"), value = 66, step=1, min=1),
	  # # numericInput("L95", label = ("L95"), value = 70, step=1, min=1)
	
    # # ),
    
	# # mainPanel(
      # # tabsetPanel(      
	    # # tabPanel("Data Summary", tableOutput("head")),
        # # tabPanel("Histogram", plotOutput("hist")),
		# # tabPanel("Maturity-at-length", plotOutput("MatCurve")),
		# # tabPanel("SPR", plotOutput("SPRplot")),
        # # id = "tabs1"
      # )
    # # mainPanel(
      # # uiOutput("tb")
    # )
  # )
# ))




