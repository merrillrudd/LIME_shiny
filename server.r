library(shiny)
library(LIME)

shinyServer(function(input, output, clientData, session) {
  Sys <- Sys.info()['sysname']

  # Life-history simulation 
  output$DataSimulation <- renderPlot({ 

#     ChkRange()
	lh <- create_lh_list(vbk=input$vbk, linf=input$linf, lwa=input$lwa, lwb=input$lwb, S50=input$SL50, M50=input$ML50, selex_input="length", maturity_input="length", binwidth=input$binwidth, CVlen=input$CVlen, SigmaR=input$SigmaR, SigmaF=input$SigmaF)
  
  par(mfrow=c(2,3), mar=c(3,3,1,1), mgp=c(1.6,0.5,0))
  plot(lh$Mat_a, type="l", lwd=4, col="gray", xlab="Age", ylab="Proportion")
  lines(lh$S_a, lwd=4, lty=2, col="blue")
  legend('bottomright', legend=c("Maturity", "Selectivity"), col=c("gray", "blue"), lty=c(1,2), lwd=4)
  
  plot(lh$L_a, type="l", lwd=4, col="forestgreen", xlab="Age", ylab="Length (cm)")
  
  simdata <- sim_pop(lh=lh, Nyears=20, Fdynamics=input$Fdynamics, Rdynamics=input$Rdynamics, Nyears_comp=20, comp_sample=1000, nburn=50, seed=123, modname="LC1")
	plot(simdata$D_t, ylim=c(0, max(simdata$D_t)*1.5), type="l", lwd=4, col="black", xlab="Year", ylab="Relative biomass")
  plot(simdata$R_t, ylim=c(0, max(simdata$R_t)*1.5), type="l", lwd=4, col="black", xlab="Year", ylab="Relative recruitment")
  plot(simdata$F_t, ylim=c(0, max(simdata$F_t)*1.5), type="l", lwd=4, col="black", xlab="Year", ylab="Fishing mortality")
  plot(simdata$ML_t, ylim=c(0, max(simdata$ML_t)*1.5), type="l", lwd=4, col="black", xlab="Year", ylab="Mean length in catch")
 }, res=100, height= function() session$clientData$output_DataSimulation_width 
  )

  # Life-history simulation -- length comps
  output$DataSimulation_LC <- renderPlot({ 
  
  #     ChkRange()
  lh <- create_lh_list(vbk=input$vbk, linf=input$linf, lwa=input$lwa, lwb=input$lwb, S50=input$SL50, M50=input$ML50, selex_input="length", maturity_input="length", binwidth=input$binwidth, CVlen=input$CVlen, SigmaR=input$SigmaR, SigmaF=input$SigmaF)
  simdata <- sim_pop(lh=lh, Nyears=20, Fdynamics=input$Fdynamics, Rdynamics=input$Rdynamics, Nyears_comp=20, comp_sample=1000, nburn=50, seed=123, modname="LC1")
  par(mfrow=c(4,5), mar=c(0,0,0,0), omi=c(1,1,0.5,0.5))
  for(i in 1:20){
    pLF <- simdata$LF[i,]/sum(simdata$LF[i,])
    barplot(pLF, ylim=c(0,max(simdata$LF[1,]/sum(simdata$LF[1,]))*1.1), xaxt="n", yaxt="n")
    if(i %in% c(1,6,11,16)) axis(2, las=2)
    if(i %in% c(16:20)) axis(1)
    text(x=0.9*length(pLF), y=0.9*max(simdata$LF[1,]/sum(simdata$LF[1,])), i, font=2, cex=2)
    abline(v=input$ML50, col="red", lwd=2)
  }
  mtext("Proportion", side=2, line=3, outer=TRUE)
  mtext("Length bin (cm)", side=1, line=3, outer=TRUE)
}, res=100, height= function() session$clientData$output_DataSimulation_LC_width 
)

# Read CSV file 
 data_LC <- reactive({
  file1 <- input$file1
  if (is.null(file1)) return(NULL)
  d1 <- read.csv(file1$datapath, header = input$header,
           sep = input$sep, quote = input$quote)
  d1[,2:ncol(d1)]
})	
data_Index <- reactive({
  file2 <- input$file2
  if (is.null(file2)) return(NULL)
  d1 <- read.csv(file2$datapath, header = input$header_time,
                 sep = input$sep, quote = input$quote)
  as.matrix(d1[,2])
})
data_Catch <- reactive({
  file3 <- input$file3
  if (is.null(file3)) return(NULL)
  d1 <- read.csv(file3$datapath, header = input$header_time,
                 sep = input$sep, quote = input$quote)
  as.matrix(d1[,2])
})

years_LC <- reactive({
  file1 <- input$file1
  if(is.null(file1)) return(NULL)
  d1 <- read.csv(file1$datapath, header=input$header, sep=input$sep, quote=input$quote)
  d1[,1]
})
years_Index <- reactive({
  file2 <- input$file2
  if (is.null(file2)) return(NULL)
  d1 <- read.csv(file2$datapath, header = input$header_time,
                 sep = input$sep, quote = input$quote)
  d1[,1]
})
years_Catch <- reactive({
  file3 <- input$file3
  if (is.null(file3)) return(NULL)
  d1 <- read.csv(file3$datapath, header = input$header_time,
                 sep = input$sep, quote = input$quote)
  d1[,1]
})

##Print out first 6 observations 
output$head_LC <- renderTable({
  if(is.null(data_LC())) return(NULL)
  dat <- head(data_LC())
})
output$head_Index <- renderTable({
  if(is.null(data_Index())) return(NULL)
  dat <- head(data_Index())
})
output$head_Catch <- renderTable({
  if(is.null(data_Catch())) return(NULL)
  dat <- head(data_Catch())
})

# output$datSum <- renderText({
#   if(is.null(data())) return(NULL)
#   dat <- data()
#   if(!is.null(dim(dat))) {
#     # 	  if (ncol(dat) > 2) {
#     # 	    str1 <- paste("Warning! More than two columns in data file. ")
#     #         str2 <- paste("Only Columns 1 and 2 will be used")
#     #         str3 <- (paste(str1, str2))
#     # 	    return(str3)
#     # 	  } 
#     if (ncol(dat) == 2) return(paste("A total of", sum(dat[,2]), "observations"))
#   }
#   if (ncol(dat)==1) return(paste("A total of", length(unlist(dat)), "observations"))
# })
 })  
# 

#   
#   values <- reactiveValues(shouldShow = FALSE)
#   observeEvent(input$goButton, {
#     if(input$goButton == 0) return(NULL)
#     values$shouldShow = TRUE
#   })
#   
#   observeEvent(input$file1, {
#     values$shouldShow = FALSE
#   })
#    
#   output$SPRAssessment <- renderPlot({
#     if(is.null(data())) return(NULL)
#     linf <- input$linf
# 	ML50 <- input$ML50
# 	SL50 <- input$SL50
# 	ChkRange()
# 	
# 	loadDat <- data()
# 	Dim <- dim(loadDat)
# 	if (Dim[2] == 1) {
# 	  lendat <- unlist(loadDat)
# 	  MaxLen <- round(max(1.25*linf, 1.1*max(lendat)),0)
# 	  LenBins <- seq(from=0, to=MaxLen, by=input$binswidth)
# 	  LenMids <- seq(from=0.5*input$binswidth, by=input$binswidth, length.out=length(LenBins)-1)
# 	  LenDat <- as.vector(table(cut(lendat,LenBins)))
# 	  SizeBins <- NULL
#       SizeBins$Linc <- input$binswidth
#       SizeBins$ToSize <- max(LenBins)
# 	}
# 	if (Dim[2] == 2) {
# 	  LenDat <- loadDat[,2]
# 	  LenMids <- loadDat[,1]
# 	  By <- LenMids[2] - LenMids[1]
# 	  LenBins <- seq(from=0, by=By, length.out=length(LenMids)+1)
# 	  SizeBins <- NULL
#       SizeBins$Linc <- By
#       SizeBins$ToSize <- max(LenBins)
# 	}
# 	setSPR <- input$setSPR
#     Stock <- NULL
#     Stock$NGTG <- 41
#     Stock$GTGlinfBy <- NA
#     Stock$linf <- input$linf
#     Stock$CVlinf <- 0.1 # NEED TO ADD THIS TO INPUT VARIABLES
#     Stock$MaxSD <- 2 
#     Stock$MK  <- input$MK
#     Stock$L50 <- input$L50
#     Stock$L95 <- input$L95
#     Stock$Walpha <- 1
#     Stock$Wbeta <- 3
#     Stock$FecB  <- 3
#     Stock$Steepness <- h <- max(0.20001, input$steepness) 
# 	Stock$Steepness <- h <- min(h, 0.99)
#     Stock$Mpow <- 0
#     Stock$R0  <- 1000
#     Stock$Centlinf <- NULL
#     Stock$CentMpar <- NULL
#     Stock$CentKpar <- NULL
#     Stock$Mslope <- 0 
# 	Fleet <- NULL
#     Fleet$SL50 <- NA
#     Fleet$SL95 <- NA
#     Fleet$MLLKnife <- NA
#     Fleet$FM <- 0
# 	
# 	SL50Start <- LenMids[which.max(LenDat)]
#     DeltaStart <- 0.1 * SL50Start
#     FMStart <- 1 
#     Starts <- log(c(SL50Start/linf, DeltaStart/linf, FMStart))
#     Lower <- log(c(0.1, 0.1, 0.001))
#     Upper <- log(c(0.9, 0.9, 20))
# 	
# 	MCex <- 1.3
#     par(mfrow=c(2,2), mar=c(3,4,2,0), oma=c(2,2,2,1))
# 	# tt <- barplot(runMod$ExpLenCatchUnfished2*R1, names.arg=round(runMod$LenMids[ind:ind2],0), axes=FALSE)
# 	N <- sum(LenDat)
# 	tt <- barplot(LenDat, names.arg=round(LenMids,0))#, axes=FALSE)
# 	axis(side=1, at=tt, label=FALSE)
# 	axis(side=2, label=FALSE)
# 	
# 	if (values$shouldShow) {
#       # run optimization
#       Opt <- nlminb(Starts, OptRoutine, LenDat=LenDat, Stock=Stock, SizeBins=SizeBins, 
# 	  lower=Lower, upper=Upper)
# 	  N <- sum(LenDat)
#       Fleet <- NULL
#       Fleet$SL50 <- exp(Opt$par)[1] * Stock$linf
#       Fleet$SL95 <- Fleet$SL50  + exp(Opt$par)[2] * Stock$linf
#       Fleet$MLLKnife <- NA
#       Fleet$FM <- exp(Opt$par)[3]
#       runMod <- EqSimMod_LB(Stock, Fleet, SizeBins, FitControl=NULL)
# 	  
# 	  lines(tt, runMod$ExpLenCatchFished*N, lwd=3)
# 	  title(paste0("Estimated SPR = ", round(runMod$SPR,2)), xpd=NA, cex=1.5)
# 	  
# 	  	# Calc SPR v F/M 
#       FMVec <- seq(from=0, to=5, length.out=100)
#       run <- sapply(1:length(FMVec), function (xx) {
#         Fleet$FM <- FMVec[xx]
#         EqSimMod_LB(Stock, Fleet, SizeBins, FitControl=NULL)
#       })
# 	  SaveSPR <- sapply(1:length(FMVec), function(X) run[,X]$SPR)
# 	  SaveYield1 <- sapply(1:length(FMVec), function(X) run[,X]$Yield)
#       SaveYield <- SaveYield1/max(SaveYield1)
# 	  currYield <- runMod$Yield/max(SaveYield1)
#       FTarg <- FMVec[max(which(SaveSPR >= setSPR))]
# 	  
# 	  lens <- seq(from=0, to=max(L50, linf), length.out=100)
# 	  Matprob <- 1.0/(1+exp(-log(19)*(lens-L50)/(L95-L50)))
# 	  Selprob <- 1.0/(1+exp(-log(19)*(lens-Fleet$SL50)/(Fleet$SL95 -Fleet$SL50)))
# 	  
# 	  plot(lens, Matprob, ylim=c(0,1), bty="n", lwd=3, type="l", las=1, xlab="", ylab="")
# 	  lines(lens, Selprob, lty=2, lwd=3)
# 	  mtext(side=1, line=2.5, "Length", cex=MCex)
# 	  mtext(side=2, line=2.5, "Probability", cex=MCex)
# 	  legend(c(lens[1], 1.3), bty="n", c("Maturity", "Selectivity"), lwd=3, lty=1:2,
# 	    seg.len=3, xpd=NA)
# 	  estSL50 <- round(Fleet$SL50,0)
# 	  estSL95 <- round(Fleet$SL95,0)
# 	  text(lens[length(lens)], 1.2, bquote(SL[50]~ " = "~ .(estSL50)), xpd=NA, cex=1.25, pos=2)
# 	  text(lens[length(lens)], 1.1, bquote(SL[95]~ " = "~ .(estSL95)), xpd=NA, cex=1.25, pos=2)
# 	  
# 	  plot(c(0,5), c(0,1), type="n", bty="n", las=1, xlab="", ylab="")
# 	  lines(FMVec, SaveSPR, lwd=3)
# 	  mtext(side=1, line=2.5, expression(italic(F/M)), cex=MCex)
# 	  mtext(side=2, line=2.5, "SPR", cex=MCex)
# 	  points(Fleet$FM, runMod$SPR, pch=19, cex=3, xpd=NA)
#       
# 	  SPRcrash <- (1-h)/(4*h)
# 	  SPRlim <- -(2*(h-1))/(3*h+1)
# 
# 	  if (h < 0.99) {
# 	    lines(range(FMVec), c(SPRcrash, SPRcrash), lty=3, col="red", lwd=2)
# 	    lines(range(FMVec), c(SPRlim, SPRlim), lty=3, col="blue", lwd=2)
# 	    legend("topright", bty="n", lty=c(3,3), legend=c(expression(SPR[crash]), expression(SPR[limit])),
# 	      col=c("red", "blue"), lwd=2)
# 	  }
# 	  
# 	  plot(c(0,5), c(0,1), type="n", bty="n", las=1, xlab="", ylab="")
# 	  lines(FMVec, SaveYield, lwd=3)
# 	  mtext(side=1, line=2.5, expression(italic(F/M)), cex=MCex)
# 	  mtext(side=2, line=2.5, "Relative Yield", cex=MCex)
# 	  points(Fleet$FM, currYield, pch=19, cex=3, xpd=NA)
# 	}  
# 	
# 	
#   # }, res=100, height=600)
#   }, res=100, height= function() session$clientData$output_SPRSimulation_width )
#   
#   

#   
#   # Create histogram of data
#   output$hist <- renderPlot({
#     if(is.null(data())) return(NULL)
# 	lendat <- unlist(data())
#     bins <- seq(0, max(lendat)*1.25, by = input$binswidth)
# 	hist(lendat, breaks=bins, col="darkgray", border="white")
#   })
#   
#   
#   
#   # Not used 
#   # output$tb <- renderUI({
#     # ChkRange()
#     # if(is.null(data())) {
# 	  # p("no data")
# 	# } else {
# 	  # tabsetPanel(tabPanel("Data", tableOutput("head")), 
# 		# tabPanel("Histogram", plotOutput("hist")),
# 		# tabPanel("Maturity curve", plotOutput("MatCurve"))
# 		# )
#     # }
#   # })
# 
# })
