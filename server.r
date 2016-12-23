library(shiny)
library(LIME)
library(TMB)


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

  output$SaveSimulation <- renderText({
    savesim <- eventReactive(input$saveButton,{
      TRUE
    })
    if(savesim()==TRUE){
      
      dir.create(file.path(".", "sim"), showWarnings=FALSE)
      outputDir <- "sim"
      
      lh <- create_lh_list(vbk=input$vbk, linf=input$linf, lwa=input$lwa, lwb=input$lwb, S50=input$SL50, M50=input$ML50, selex_input="length", maturity_input="length", binwidth=input$binwidth, CVlen=input$CVlen, SigmaR=input$SigmaR, SigmaF=input$SigmaF) 
      simdata <- sim_pop(lh=lh, Nyears=20, Fdynamics=input$Fdynamics, Rdynamics=input$Rdynamics, Nyears_comp=20, comp_sample=1000, nburn=50, seed=123, modname="LC1")
      
      saveData <- function(data, name) {
        # Create a unique file name
        fileName <- paste0(name, ".csv")
        # Write the file to the local system
        write.csv(
          x = data,
          file = file.path(outputDir, fileName), 
          row.names = TRUE, quote = TRUE
        )
      }
      
      saveData(simdata$LF, name="SimLenFreq20")
      saveData(simdata$LF[(nrow(simdata$LF)-9):nrow(simdata$LF),], name="SimLenFreq10")
      LF1 <- t(simdata$LF[nrow(simdata$LF),])
      rownames(LF1) <- nrow(simdata$LF)
      saveData(LF1, name="SimLenFreq1")
      saveData(simdata$I_t, "SimIndex")
      saveData(simdata$C_t, "SimCatch")
      saveData(simdata$F_t, "TrueF")
      saveData(simdata$R_t, "TrueR")
      saveData(simdata$SPR, "TrueSPR")
    }
    
  })

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
  d1 <- read.csv(file1$datapath, header = TRUE,
           sep = input$sep, quote = input$quote)
  d1[,2:ncol(d1)]
})	
data_Index <- reactive({
  file2 <- input$file2
  if (is.null(file2)) return(NULL)
  d1 <- read.csv(file2$datapath, header = TRUE,
                 sep = input$sep, quote = input$quote)
  as.matrix(d1[,2])
})
data_Catch <- reactive({
  file3 <- input$file3
  if (is.null(file3)) return(NULL)
  d1 <- read.csv(file3$datapath, header = TRUE,
                 sep = input$sep, quote = input$quote)
  as.matrix(d1[,2])
})

years_LC <- reactive({
  file1 <- input$file1
  if(is.null(file1)) return(NULL)
  d1 <- read.csv(file1$datapath, header=TRUE, sep=input$sep)
  as.numeric(d1[,1])
})
years_Index <- reactive({
  file2 <- input$file2
  if (is.null(file2)) return(NULL)
  d1 <- read.csv(file2$datapath, header = TRUE,
                 sep = input$sep)
  as.numeric(d1[,1])
})
years_Catch <- reactive({
  file3 <- input$file3
  if (is.null(file3)) return(NULL)
  d1 <- read.csv(file3$datapath, header = TRUE,
                 sep = input$sep)
  as.numeric(d1[,1])
})

##Print out first 6 observations 
output$head_LC <- renderTable({
  if(is.null(data_LC())) return(NULL)
  dat <- head(data_LC())
})
output$plot_Index <- renderPlot({
  if(is.null(data_Index())) return(NULL)
  plot(x=years_Index(), y=data_Index(), lwd=3, type="o", ylim=c(0, max(data_Index())*1.1), xlab="Year", ylab="Abundance index")
})
output$plot_Catch <- renderPlot({
  if(is.null(data_Catch())) return(NULL)
  plot(x=years_Catch(), y=data_Catch(), lwd=3, type="o", ylim=c(0, max(data_Catch())*1.1), xlab="Year", ylab="Catch")
})

  output$LIME_assessment <- renderPlot({
    Nyears <- input$Nyears
    LF <- as.matrix(data_LC())
    bins <- seq(input$binwidth, by=input$binwidth, length=ncol(LF))
    colnames(LF) <- bins
    rownames(LF) <- years_LC()
    It <- as.vector(t(data_Index()))
    names(It) <- years_Index()
    Ct <- as.vector(t(data_Catch()))
    names(Ct) <- years_Catch()
    obs_per_year <- rowSums(LF)
    input_data <- list("years"=1:Nyears, "LF"=LF, "I_t"=It, "C_t"=Ct, "obs_per_year"=obs_per_year)
    est_sigma <- NULL
    if(input$est_sigR) est_sigma <- c(est_sigma, "log_sigma_R")
    if(input$est_CVL) est_sigma <- c(est_sigma, "log_CV_L")
    if(input$est_sigC) est_sigma <- c(est_sigma, "log_sigma_C")
    if(input$est_sigI) est_sigma <- c(est_sigma, "log_sigma_I")
    if(all(is.null(It))==FALSE & all(is.null(Ct)==FALSE)) data_avail <- "Index_Catch_LC"
    if(all(is.null(It))==FALSE & all(is.null(Ct))) data_avail <- "Index_LC"
    if(all(is.null(It)) & all(is.null(Ct)==FALSE)) data_avail <- "Catch_LC"
    if(all(is.null(It)) & all(is.null(Ct))) data_avail <- "LC"
    lh <- create_lh_list(vbk=input$vbk, linf=input$linf, lwa=input$lwa, lwb=input$lwb, S50=input$SL50, M50=input$ML50, selex_input="length", maturity_input="length", binwidth=input$binwidth, CVlen=input$CVlen, SigmaR=input$SigmaR, SigmaF=input$SigmaF)
    
    param_adjust <- NULL
    if(lh$SigmaR==0) param_adjust <- c(param_adjust, "SigmaR")
    if(lh$SigmaF==0) param_adjust <- c(param_adjust, "SigmaF")
    if(lh$SigmaC==0) param_adjust <- c(param_adjust, "SigmaC")
    if(lh$SigmaI==0) param_adjust <- c(param_adjust, "SigmaI")
    
    val_adjust <- FALSE
    if(all(is.null(param_adjust))==FALSE) val_adjust <- rep(0.01, length(param_adjust))
    if(all(is.null(param_adjust))) param_adjust <- FALSE
   
    run <- eventReactive(input$goButton, {
      TRUE
    })
    
    if(run()==TRUE){
      res <- run_LIME(modpath=NULL, write=FALSE, lh=lh, input_data=input_data, est_sigma=est_sigma, data_avail=data_avail, itervec=NULL, REML=FALSE, rewrite=TRUE, fix_f=0, simulation=FALSE, param_adjust=param_adjust, val_adjust=val_adjust, f_true=FALSE, fix_param=FALSE)
      
      dir.create(file.path(".", "results"), showWarnings=FALSE)
      outputDir <- "results"
      
      rep <- res$Report
      sdrep <- res$Sdreport
      der <- res$Derived
      inp <- res$Inputs
      
      saveRDS(rep, file.path(outputDir, "Report.rds"))
      saveRDS(sdrep, file.path(outputDir, "Sdreport.rds"))
      saveRDS(der, file.path(outputDir, "Derived.rds"))
      saveRDS(inp, file.path(outputDir, "Inputs.rds"))
      
      FUN <- function(InputMat, log=TRUE, rel=FALSE){
                index <- which(is.na(InputMat[,2])==FALSE)
                if(log==TRUE) return(c( exp(InputMat[index,1]-1.96*InputMat[index,2]), rev(exp(InputMat[index,1]+1.96*InputMat[index,2]))))
                if(log==FALSE) return(c( InputMat[index,1]-1.96*InputMat[index,2], rev(InputMat[index,1]+1.96*InputMat[index,2])))
      }
      par(mfrow=c(2,2))
      plot(rep$F_t, type="l", lwd=2, ylim=c(0, max(rep$F_t)*3), col="blue", xaxs="i", yaxs="i", xlab="Year", ylab="Fishing mortality")
      polygon( y=FUN(summary(sdrep)[which(rownames(summary(sdrep))=="lF_t"),], log=TRUE), x=c(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="lF_t"),2])==FALSE), rev(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="lF_t"),2])==FALSE))), col=rgb(0,0,1,alpha=0.2), border=NA)
      
      plot(rep$R_t, type="l", lwd=2, ylim=c(0, max(rep$R_t)*3), col="blue", xaxs="i", yaxs="i", xlab="Year", ylab="Recruitment")
      polygon( y=FUN(summary(sdrep)[which(rownames(summary(sdrep))=="lR_t"),], log=TRUE), x=c(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="lR_t"),2])==FALSE), rev(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="lR_t"),2])==FALSE))), col=rgb(0,0,1,alpha=0.2), border=NA)
      
      plot(rep$SPR_t, type="l", lwd=2, ylim=c(0, max(rep$SPR_t)*3), col="blue", xaxs="i", yaxs="i", xlab="Year", ylab="SPR")
      polygon( y=FUN(summary(sdrep)[which(rownames(summary(sdrep))=="SPR_t"),], log=FALSE), x=c(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="SPR_t"),2])==FALSE), rev(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="SPR_t"),2])==FALSE))), col=rgb(0,0,1,alpha=0.2), border=NA)
      abline(h=0.3, lty=2, lwd=3)
      
      plot(x=1, y=1, type="n", xlim=c(0,1), ylim=c(0,3), xaxs="i", yaxs="i", xlab="SPR", ylab="F/F30")
      polygon(x=c(0.3,1,1,0.3), y=c(0,0,1,1), col="#00AA0030", border=NA)
      polygon(x=c(0,0.3,0.3,0), y=c(1,1,3,3), col="#AA000030", border=NA)
      abline(h=1, lty=2, lwd=3)
      abline(v=0.3, lty=2, lwd=3)
      points(x=rep$SPR_t[length(rep$SPR_t)], y=rep$F_t[length(rep$F_t)]/der$F30, col="blue", pch=19, cex=2)
      
      mtext("LIME results overview", outer=TRUE, line=-2, side=3, cex=1.2)
      
    }
        
    
    }, res=100, height= function() session$clientData$output_LIME_assessment_width )

output$ReportTable <- renderTable({
  if(file.exists(file.path("results","Report.rds"))==FALSE) return(NULL)
  sdrep <- readRDS(file.path("results", "Sdreport.rds"))
  summary(sdrep)
})

})
 
  
  
  