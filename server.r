library(shiny)
library(LIME)
library(TMB)


shinyServer(function(input, output, clientData, session) {
  Sys <- Sys.info()['sysname']

  # Life-history simulation 
  output$DataSimulation <- renderPlot({ 


	lh <- create_lh_list(vbk=input$vbk, linf=input$linf, lwa=0.025, lwb=3, S50=input$SL50, M50=input$ML50, selex_input="length", maturity_input="length", binwidth=input$binwidth, CVlen=input$CVlen, SigmaR=input$SigmaR, SigmaF=input$SigmaF) 

  
  par(mfrow=c(2,3), mar=c(3,4,1,1), mgp=c(1.6,0.5,0))
  plot(lh$Mat_a, type="l", ylim=c(0, 1.1), lwd=4, col="gray", xlab="Age", ylab="", xaxs="i", yaxs="i")
  lines(lh$S_a, lwd=4, lty=2, col="blue")
  par(new=TRUE)
  plot(lh$L_a, ylim=c(0, max(lh$L_a)*1.1), xaxt="n", yaxt="n", type="l", lwd=4, col="forestgreen", xlab="", ylab="", xaxs="i", yaxs="i")
  axis(4, at=pretty(c(0,max(lh$L_a)*1.1)))
  legend('bottomright', legend=c("Maturity", "Selectivity", "Length"), col=c("gray", "blue", "forestgreen"), lty=c(1,2,1), lwd=4)
  mtext("Length (cm)", side=4, line=2)
  mtext("Proportion", side=2, line=2)
  
  simdata <- sim_pop(lh=lh, Nyears=input$Nyears, Fdynamics=input$Fdynamics, Rdynamics=input$Rdynamics, Nyears_comp=input$Nyears, comp_sample=1000, nburn=50, seed=123, modname="LC1")
  plot(simdata$D_t, ylim=c(0, max(simdata$D_t)*1.5), type="l", lwd=4, col="black", xlab="Year", ylab="")
  mtext("Relative biomass", side=3, line=-2)
  plot(simdata$SPR_t, ylim=c(0, max(simdata$SPR_t)*1.5), type="l", lwd=4, col="black", xlab="Year", ylab="")
  mtext("Spawning potential ratio (SPR)", side=3, line=-2)
  plot(simdata$R_t, ylim=c(0, max(simdata$R_t)*1.5), type="l", lwd=4, col="black", xlab="Year", ylab="")
  mtext("Relative recruitment", side=3, line=-2)
  plot(simdata$F_t, ylim=c(0, max(simdata$F_t)*1.5), type="l", lwd=4, col="black", xlab="Year", ylab="")
  mtext("Fishing mortality", side=3, line=-2)
  plot(simdata$ML_t, ylim=c(0, max(simdata$ML_t)*1.5), type="l", lwd=4, col="black", xlab="Year", ylab="")
  mtext("Mean length in catch", side=3, line=-2)
  }, res=100, height= function() session$clientData$output_DataSimulation_width
  )

  output$SaveSimulation <- renderText({
    savesim <- eventReactive(input$saveButton,{
      TRUE
    })
    if(savesim()==TRUE){
      
      dir.create(file.path(".", "sim"), showWarnings=FALSE)
      
      lh <- create_lh_list(vbk=input$vbk, linf=input$linf, lwa=0.025, lwb=3, S50=input$SL50, M50=input$ML50, selex_input="length", maturity_input="length", binwidth=input$binwidth, CVlen=input$CVlen, SigmaR=input$SigmaR, SigmaF=input$SigmaF) 
      simdata <- sim_pop(lh=lh, Nyears=input$Nyears, Fdynamics=input$Fdynamics, Rdynamics=input$Rdynamics, Nyears_comp=input$Nyears, comp_sample=1000, nburn=50, seed=123, modname="LC1")
      
      saveData <- function(data, name, type="csv") {
        # Create a unique file name
        fileName <- paste0(name, ".", type)
        # Write the file to the local system
        if(type=="csv"){
          write.csv(
            x = data,
            file = file.path("sim", fileName), 
            row.names = TRUE, quote = TRUE
          )
        }
        if(type=="rds"){
          saveRDS(data, file=file.path("sim", fileName))
        }
   
      }
      
      if(input$Nyears>=20) saveData(simdata$LF, name="SimLenFreq20")
      if(input$Nyears>=10) saveData(simdata$LF[(nrow(simdata$LF)-9):nrow(simdata$LF),], name="SimLenFreq10")
      if(input$Nyears>=5) saveData(simdata$LF[(nrow(simdata$LF)-4):nrow(simdata$LF),], name="SimLenFreq5")
      if(input$Nyears>=2) saveData(simdata$LF[(nrow(simdata$LF)-1):nrow(simdata$LF),], name="SimLenFreq2")
      LF1 <- t(simdata$LF[nrow(simdata$LF),])
      rownames(LF1) <- nrow(simdata$LF)
      saveData(LF1, name="SimLenFreq1")
      saveData(simdata$I_t, "SimIndex")
      saveData(simdata$C_t, "SimCatch")
      saveData(simdata$F_t, "TrueF")
      saveData(simdata$R_t, "TrueR")
      saveData(simdata$SPR, "TrueSPR")
      saveData(simdata, "Truth", type="rds")
    }
    
  })

  # Life-history simulation -- length comps
  output$DataSimulation_LC <- renderPlot({ 
  
  #     ChkRange()
  lh <- create_lh_list(vbk=input$vbk, linf=input$linf, lwa=0.025, lwb=3, S50=input$SL50, M50=input$ML50, selex_input="length", maturity_input="length", binwidth=input$binwidth, CVlen=input$CVlen, SigmaR=input$SigmaR, SigmaF=input$SigmaF)
  simdata <- sim_pop(lh=lh, Nyears=input$Nyears, Fdynamics=input$Fdynamics, Rdynamics=input$Rdynamics, Nyears_comp=input$Nyears, comp_sample=1000, nburn=50, seed=123, modname="LC1")
  
  par(mfrow=c(4,5), mar=c(0,0,0,0), omi=c(1,1,0.5,0.5))
  for(i in 1:min(input$Nyears,20)){
    pLF <- simdata$LF[i,]/sum(simdata$LF[i,])
    barplot(pLF, ylim=c(0,max(simdata$LF[1,]/sum(simdata$LF[1,]))*1.1), xaxt="n", yaxt="n")
    if(i %in% c(1)) axis(2, las=2)
    if(i %in% c(min(input$Nyears,20))) axis(1)
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
data_ESS <- reactive({
  file4 <- input$file4
  if(is.null(file4)) return(NULL)
  d1 <- read.csv(file4$datapath, header=TRUE, sep=input$sep, quote=input$quote)
  t(as.matrix(d1[,2]))
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
years_ESS <- reactive({
  file4 <- input$file4
  if (is.null(file4)) return(NULL)
  d1 <- read.csv(file4$datapath, header = TRUE,
                 sep = input$sep)
  as.numeric(d1[,1])
})

##Print out first 6 observations 
output$preview_LC <- renderPlot({
  if(is.null(data_LC())) return(NULL)
  dat <- data_LC()
  years <- years_LC()
  years_i <- seq_along(years)
  bins <- seq(input$binwidth, by=input$binwidth, length=ncol(dat))
  nyears <- length(years)
  par(mfrow=c(1,min(length(years),6)))
  for(i in 1:min(length(years),6)){
    plotdat <- as.matrix(dat[i,])
    colnames(plotdat) <- bins
    barplot(plotdat, ylim=c(0, max(dat)), col="black")
    text(x=0.5*ncol(dat), y=0.9*max(dat), paste0("Year ", years[i]), font=2, cex=2)
  }
})
output$preview_LCtable <- renderTable({
  if(is.null(data_LC())) return(NULL)
  LF <- as.matrix(data_LC())
  bins <- seq(input$binwidth, by=input$binwidth, length=ncol(LF))
  colnames(LF) <- bins
  rownames(LF) <- years_LC()
  LF
})
output$plot_Index <- renderPlot({
  if(is.null(data_Index())) return(NULL)
  plot(x=years_Index(), y=data_Index(), xlim=c(0,input$Nyears), lwd=3, type="o", ylim=c(0, max(data_Index())*1.1), xlab="Year", ylab="Abundance index")
})
output$plot_Catch <- renderPlot({
  if(is.null(data_Catch())) return(NULL)
  plot(x=years_Catch(), y=data_Catch(), xlim=c(0, input$Nyears), lwd=3, type="o", ylim=c(0, max(data_Catch())*1.1), xlab="Year", ylab="Catch")
})
output$preview_ESS <- renderPlot({
  if(is.null(data_ESS())) return(NULL)
  par(mfrow=c(1,3))
  barplot(data_ESS(), space=FALSE, col="black", xlab="Year", ylab="Effective Sample Size of Length Data")
  axis(1, at=seq_along(years_ESS()), labels=years_ESS())
  plot(x=1,y=1,type="n",axes=F,ann=F)
  plot(x=1,y=1,type="n",axes=F,ann=F)
})

  output$runLIME <- renderText({
    runsim <- eventReactive(input$goButton,{
      TRUE
    })
    if(runsim()==TRUE){
      Nyears <- input$Nyears
      LF <- as.matrix(data_LC())
      bins <- seq(input$binwidth, by=input$binwidth, length=ncol(LF))
      colnames(LF) <- bins
      rownames(LF) <- years_LC()
      if(is.null(data_Index())==FALSE){
        It <- as.vector(t(data_Index()))
        names(It) <- years_Index()
      }
      if(is.null(data_Index())) It <- NULL
      if(is.null(data_Catch())==FALSE){
        Ct <- as.vector(t(data_Catch()))
        names(Ct) <- years_Catch()
      }
      if(is.null(data_Catch())) Ct <- NULL
      obs_per_year <- data_ESS()
      names(obs_per_year) <- years_ESS()
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
      lh <- create_lh_list(vbk=input$vbk, linf=input$linf, lwa=0.025, lwb=3, S50=input$SL50, M50=input$ML50, selex_input="length", maturity_input="length", binwidth=input$binwidth, CVlen=input$CVlen, SigmaR=input$SigmaR, SigmaF=input$SigmaF)
      print(lh$SigmaR)
      
      param_adjust <- NULL
      if(lh$SigmaR==0) param_adjust <- c(param_adjust, "SigmaR")
      if(lh$SigmaF==0) param_adjust <- c(param_adjust, "SigmaF")
      if(lh$SigmaC==0) param_adjust <- c(param_adjust, "SigmaC")
      if(lh$SigmaI==0) param_adjust <- c(param_adjust, "SigmaI")
      
      val_adjust <- FALSE
      if(all(is.null(param_adjust))==FALSE) val_adjust <- rep(0.01, length(param_adjust))
      if(all(is.null(param_adjust))) param_adjust <- FALSE
      
      dir.create(file.path(".", "results"), showWarnings=FALSE)
    
      res <- run_LIME(modpath=file.path(".", "results"), write=TRUE, lh=lh, input_data=input_data, est_sigma=est_sigma, data_avail=data_avail, itervec=NULL, REML=FALSE, rewrite=TRUE, fix_f=0, simulation=FALSE, param_adjust=param_adjust, val_adjust=val_adjust, f_true=FALSE, fix_param=FALSE)
    }
  })
  
  output$LIME_assessment <- renderPlot({
    if(file.exists(file.path("results", "Report.rds"))==FALSE) return(NULL)
      rep <- readRDS(file.path("results", "Report.rds"))
      sdrep <- readRDS(file.path("results", "Sdreport.rds"))
      inp <- readRDS(file.path("results", "Inputs.rds"))
      der <- readRDS(file.path("results", "Derived_quants.rds"))
      
      if(input$simcompare==TRUE) true <- readRDS(file.path(".", "sim", "Truth.rds"))
      
      FUN <- function(InputMat, log=TRUE, rel=FALSE){
        index <- which(is.na(InputMat[,2])==FALSE)
        if(log==TRUE) return(c( exp(InputMat[index,1]-1.96*InputMat[index,2]), rev(exp(InputMat[index,1]+1.96*InputMat[index,2]))))
        if(log==FALSE) return(c( InputMat[index,1]-1.96*InputMat[index,2], rev(InputMat[index,1]+1.96*InputMat[index,2])))
      }
      par(mfrow=c(2,2))
      plot(rep$F_t, type="l", lwd=2, xlim=c(1,length(rep$F_t)), ylim=c(0, max(rep$F_t)*3), col="blue", xaxs="i", yaxs="i", xlab="Year", ylab="Fishing mortality")
      if(all(is.na(sdrep))==FALSE) polygon( y=FUN(summary(sdrep)[which(rownames(summary(sdrep))=="lF_t"),], log=TRUE), x=c(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="lF_t"),2])==FALSE), rev(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="lF_t"),2])==FALSE))), col=rgb(0,0,1,alpha=0.2), border=NA)
      if(input$simcompare==TRUE) lines(true$F_t, col="black", lwd=2)
      
      plot(rep$R_t, type="l", lwd=2, xlim=c(1,length(rep$F_t)), ylim=c(0, max(rep$R_t)*3), col="blue", xaxs="i", yaxs="i", xlab="Year", ylab="Recruitment")
      if(all(is.na(sdrep))==FALSE) polygon( y=FUN(summary(sdrep)[which(rownames(summary(sdrep))=="lR_t"),], log=TRUE), x=c(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="lR_t"),2])==FALSE), rev(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="lR_t"),2])==FALSE))), col=rgb(0,0,1,alpha=0.2), border=NA)
      if(input$simcompare==TRUE) lines(true$R_t, col="black", lwd=2)
      
      plot(x=1,y=1, type="n", xlim=c(1, length(rep$SPR_t)), ylim=c(0, max(rep$SPR_t)*3), xlab="Year", ylab="SPR", xaxs="i", yaxs="i")
      polygon(x=c(0,input$Nyears, input$Nyears, 0), y=c(0.3,0.3,max(rep$SPR_t)*3, max(rep$SPR_t)*3), border=NA, col="#00AA0030")
      polygon(x=c(0,input$Nyears, input$Nyears, 0), y=c(0,0,0.3,0.3), border=NA, col="#AA000030")
      lines(x=rep$SPR_t, type="l", lwd=2, col="blue", xaxs="i", yaxs="i", xlab="Year", ylab="SPR")
      if(all(is.na(sdrep))==FALSE) polygon( y=FUN(summary(sdrep)[which(rownames(summary(sdrep))=="SPR_t"),], log=FALSE), x=c(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="SPR_t"),2])==FALSE), rev(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="SPR_t"),2])==FALSE))), col=rgb(0,0,1,alpha=0.2), border=NA)
      if(input$simcompare==TRUE) lines(true$SPR_t, col="black", lwd=2)
      
      plot(x=1, y=1, type="n", xlim=c(0,1), ylim=c(0,3), xaxs="i", yaxs="i", xlab="SPR", ylab="F/F30")
      polygon(x=c(0.3,1,1,0.3), y=c(0,0,1,1), col="#00AA0030", border=NA)
      polygon(x=c(0,0.3,0.3,0), y=c(1,1,3,3), col="#AA000030", border=NA)
      abline(h=1, lty=2, lwd=3)
      abline(v=0.3, lty=2, lwd=3)
      points(x=rep$SPR_t[length(rep$SPR_t)], y=rep$F_t[length(rep$F_t)]/der$F30, col="blue", pch=19, cex=2)
      if(input$simcompare==TRUE){
        F30 <- tryCatch(with(true, uniroot(calc_ref, lower=0, upper=50, Mat_a=Mat_a, W_a=W_a, M=M, S_a=S_a, ref=0.3)$root), error=function(e) NA)
        if(is.na(F30)==FALSE) points(x=true$SPR_t[length(true$SPR_t)], y=true$F_t[length(true$F_t)]/F30, col="black", pch=19, cex=2)
      }
      mtext("LIME results overview", outer=TRUE, line=-2, side=3, cex=1.2)
    }, res=100, height= function() session$clientData$output_LIME_assessment_width )

output$ReportTable <- renderTable({
  if(file.exists(file.path("results","df.csv"))==FALSE) return(NULL)
  df <- read.csv(file.path("results","df.csv"),header=TRUE)
  df <- df[,-1]
  colnames(df) <- c("Final_gradient", "Parameter", "Log_estimate", "Estimate")
  if(input$simcompare==TRUE){
    true <- readRDS("sim", "Truth.rds")
    truth <- vector(NA, nrow(df))
    names(truth) <- df$Parameter
    truth[which(names(truth)=="log_F_t_input")] <- true$F_t
    truth[which(names(truth)=="log_sigma_R")] <- true$SigmaR
    truth[which(names(truth)=="logS50")] <- true$S50
    truth[which(names(truth)=="log_q_I")] <- true$qcoef
    truth[which(names(truth)=="beta")] <- true$R0
    df$Truth <- truth
  }
  df
})

output$ModelFits <- renderPlot({
  if(file.exists(file.path("results", "Report.rds"))==FALSE) return(NULL)

  rep <- readRDS(file.path("results", "Report.rds"))
  sdrep <- readRDS(file.path("results", "Sdreport.rds"))
  inp <- readRDS(file.path("results", "Inputs.rds"))
  
  LC <- inp$Data$LF
  I_t <- inp$Data$I_t
  C_t <- inp$Data$C_t
  years_LC <- inp$Data$LC_yrs
  years_T <- inp$Data$T_yrs
  years_I <- inp$Data$I_yrs
  years_C <- inp$Data$C_yrs
  
  ncomp <- length(years_LC)
  nobs <- ncomp + ifelse(is.null(years_I), 0, 1) + ifelse(is.null(years_C), 0, 1)
  par(mfrow=c(5,5), mar=c(1,1,1,1), omi=c(1,1,1,1))
  FUN <- function(InputMat, log=TRUE, rel=FALSE){
    index <- which(is.na(InputMat[,2])==FALSE)
    if(log==TRUE) return(c( exp(InputMat[index,1]-1.96*InputMat[index,2]), rev(exp(InputMat[index,1]+1.96*InputMat[index,2]))))
    if(log==FALSE) return(c( InputMat[index,1]-1.96*InputMat[index,2], rev(InputMat[index,1]+1.96*InputMat[index,2])))
  }
  
  plot(x=1,y=1,type="n",ylim=c(0,10), xlim=c(0,10),axes=F, ann=F)
  legend("topleft", legend=c("Observed", "Predicted length comp + CI", "Predicted time series + CI"), col=c("black", "red", "blue"), lwd=3)
  
  if(is.null(years_I)==FALSE){
    plot(rep$I_t_hat, type="l", lwd=2, ylim=c(0, max(rep$I_t_hat)*3), col="blue", xaxs="i", yaxs="i", xlab="Year", ylab="Recruitment")
    if(all(is.na(sdrep))==FALSE) polygon( y=FUN(summary(sdrep)[which(rownames(summary(sdrep))=="lI_t"),], log=TRUE), x=c(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="lI_t"),2])==FALSE), rev(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="lI_t"),2])==FALSE))), col=rgb(0,0,1,alpha=0.2), border=NA)
    lines(x=years_I, y=I_t, col="black", lwd=2)
    mtext(side=3, "Index", font=2, line=-2)
  }

  if(is.null(years_C)==FALSE){
    plot(rep$C_t_hat, type="l", lwd=2, ylim=c(0, max(rep$C_t_hat)*3), col="blue", xaxs="i", yaxs="i", xlab="Year", ylab="Recruitment")
    if(all(is.na(sdrep))==FALSE) polygon( y=FUN(summary(sdrep)[which(rownames(summary(sdrep))=="lC_t"),], log=TRUE), x=c(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="lC_t"),2])==FALSE), rev(which(is.na(summary(sdrep)[which(rownames(summary(sdrep))=="lC_t"),2])==FALSE))), col=rgb(0,0,1,alpha=0.2), border=NA)
    lines(C_t, col="black", lwd=2)
    mtext(side=3, "Catch", font=2, line=-2)
  }
  
  if(is.null(years_C)==FALSE & is.null(years_I)==FALSE){
    plot(x=1,y=1,type="n",axes=F,ann=F)
    plot(x=1,y=1,type="n",axes=F,ann=F)
  }
  if(is.null(years_C)==FALSE & is.null(years_I)){
    plot(x=1,y=1,type="n",axes=F,ann=F)
    plot(x=1,y=1,type="n",axes=F,ann=F)
    plot(x=1,y=1,type="n",axes=F,ann=F)
  }
  if(is.null(years_C) & is.null(years_I)==FALSE){
    plot(x=1,y=1,type="n",axes=F,ann=F)
    plot(x=1,y=1,type="n",axes=F,ann=F)
    plot(x=1,y=1,type="n",axes=F,ann=F)
  }

  for(i in 1:ncomp){
    index <- which(years_T==years_LC[i])
    plot(x=1:ncol(LC), y=LC[i,]/sum(LC[i,]), pch=19, ylim=c(0,max(LC[1,]/sum(LC[1,]))))
    lines(rep$plb[index,], col="red", lwd=3)
    mtext(side=3, paste0("LC ", index), font=2, line=-2)
  }
  

  
}, res=100, height= function() session$clientData$output_ModelFits_width )
# # 
# output$PlotSensitivities <- renderPlot({
# 
#   M
#   
# })

})
 
  
  
  