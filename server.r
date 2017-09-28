library(shiny)
library(LIME) ## download from github
library(shinyFiles)
library(TMB)
library(TMBhelper) ## download from github


shinyServer(function(input, output){
  
  values <- reactiveValues(useDF=FALSE, default=NULL, goRun=FALSE)
  # Observe Events
  observeEvent(input$defPars, {
    values$useDF <- TRUE
    values$default <- c(65, 0.2, -0.01, 0.38, 13, 34, 39, 0.025, 2.79)
  })
  observeEvent(input$goAssess,{
    values$goRun <- TRUE
  })
  
  output$InputPars <- renderUI({
    times <- input$defPars
    div(id=letters[(times %% length(letters)) + 1],
        h4("von Bertalanffy growth parameters"),
        fluidRow(
          column(4,
                 numericInput("linf", label = HTML(paste0(tags$i("L"), tags$sub(HTML("&infin;")))), value=65)
          ),
          column(4,
                 numericInput("vbk", label=tags$i("k"), value=0.21)
          ),
          column(4,
                 numericInput("t0", label=HTML(paste0(tags$i("t", tags$sub(HTML("0"))))), value=-0.01)
          )),
        h4("Natural mortality"),
        fluidRow(
          column(6,
                 numericInput("M", label = tags$i("M"), value=0.38)
          )),
        h4("Length-at-Maturity"),
        fluidRow(
          column(6,
                 numericInput("ML50", label = tags$i(HTML(paste0("L", tags$sub("50")))), value=34)
          ),
          column(6,
                 numericInput("ML95", label = tags$i(HTML(paste0("L", tags$sub("95")))), value=39)
          )),
        h4("Length-weight parameters"),
        fluidRow(
          column(4,
                 numericInput("lwa", label = tags$i("a"), value=0.025)
          ),
          column(4,
                 numericInput("lwb", label=tags$i("b"), value=2.79)
          )),
        h4("Length-at-Selectivity"),
        fluidRow(
          column(6,
                numericInput("SL50", label=tags$i(HTML(paste0("S", tags$sub("50")))), value=20),
                radioButtons("selex_type", label="Selectivity curve shape", choices=c("logistic", "dome"), selected="logistic"),
                conditionalPanel(
                  condition = "input.selex_type=='dome'",
                  numericInput("dome_sd", label="Right-side standard deviation", value=2, min=0)
                )
          ),
          column(6,
                 numericInput("SL95", label=tags$i(HTML(paste0("S", tags$sub("95")))), value=20*1.3)
          )),
        h4("Time steps per year"),
        fluidRow(
          column(6,
                numericInput("nseasons", label=tags$i(HTML(paste0("n", tags$sub("s")))), value=1)
          )
        )
    )
  })
  
  get_lh <- reactive({
    lh <- create_lh_list(linf=input$linf, vbk=input$vbk, t0=input$t0, M=input$M, M50=input$ML50, M95=input$ML95, maturity_input="length", lwa=input$lwa, lwb=input$lwb, S50=input$SL50, S95=input$SL95, selex_input="length", selex_type=input$selex_type, dome_sd=input$dome_sd, nseasons=input$nseasons)
    return(lh)
  })
  
  output$plotLH <- renderPlot({
    if(is.list(get_lh())==FALSE) return(NULL)
    lh <- get_lh()
    par(mfrow=c(3,1), mar=c(5,6,2,5))
    plot(lh$L_a, type="l", lwd=3, col="blue", xlab="", ylab="", cex.axis=2, yaxt="n")
    axis(2, cex.axis=2, col.axis="blue")
    mtext(side=2, "Length", cex=2, line=3.5, col="blue")
    mtext(side=1, "Age", cex=2, line=3.5)
    
    par(new=TRUE)
    plot(lh$W_a, type="l", lwd=3, col="forestgreen", xlab="", ylab="", xaxt="n", yaxt="n")
    axis(4, cex.axis=2, col.axis="forestgreen")
    mtext(side=4, "Weight", col="forestgreen", cex=2, line=3.5)
    
    plot(lh$Mat_l, type="l", lwd=3, xlab="", ylab="", cex.axis=2, cex.lab=2)
    mtext(side=2, "Proportion mature", cex=2, line=3.5)
    mtext(side=1, "Length", cex=2, line=3.5)
  }, height=800, width=600)
  
  output$plotSelex <- renderPlot({
    if(is.list(get_lh())==FALSE) return(NULL)
    lh <- get_lh()
    par(mfrow=c(1,1), mar=c(5,5,5,0))
    plot(lh$S_l, type="l", lwd=3, col="blue", xlab="Length", ylab="Proportion vulnerable to gear", cex.main=2, main="Selectivity-at-length", cex.axis=2, cex.lab=2)
  })
  
  output$SizeAtAge <- renderPlot({
    if(is.list(get_lh())==FALSE) return(NULL)
    lh <- get_lh()
    highs <- lh$highs
    lows <- lh$lows
    L_a <- lh$L_a
    CVlen <- lh$CVlen
    lbprobs <- function(mnl,sdl) return(pnorm(highs,mnl,sdl)-pnorm(lows,mnl,sdl))
    vlprobs <- Vectorize(lbprobs,vectorize.args=c("mnl","sdl"))
    plba <- t(vlprobs(L_a, L_a*CVlen))
    plba <- plba/rowSums(plba)
    plba_plot <- t(plba[-1,])
    ages <- lh$ages
    
    col_fun <- colorRampPalette(c("red","blue"))
    cols <- col_fun(ncol(plba_plot))
    par(mfrow=c(1,1), mar=c(5,5,5,1))
    matplot(t(plba[-1,]), type="l", lty=1, col=cols, xlab="Length", ylab="Probability", main="Probability of being a length given age", cex.axis=2, cex.lab=2, cex.main=2)
    legend("topright", title="Age", legend=ages[-1], col=cols, lty=1)
  }, height=600, width=800)
  
  
  # output$downloadExample <- renderUI({
  #   if (!is.null(lc_data()) & values$useExamp) {
  #     fluidRow(
  #       h5(strong("Download Example File")),
  #       downloadButton("dnlData", label = "Download", class = NULL)
  #       , style="padding: 5px 15px;")
  #   }
  # })
  # 
  # output$dnlData <- downloadHandler(
  #   filename = function() {
  #     nm <- ExampleDataFile()
  #     nm <- gsub("data/", "", nm)
  #     nm <- gsub('.csv', "", nm)
  #     paste(nm, '.csv', sep='')
  #   },
  #   content = function(file) {
  #     write.table(lc_data(), file, sep=",", row.names=FALSE, col.names=TRUE)
  #   }
  # )
  
  lc_data <- reactive({
      file1 <- input$file1
      if (is.null(file1)) return(NULL)
      dat <- read.csv(file1$datapath, header = TRUE,
                      sep = input$sep, stringsAsFactors=FALSE, check.names=FALSE, row=1)
    if (class(dat) == "data.frame" | class(dat) == "matrix") {
      if (nrow(dat) > 1) {
        chkNAs <- apply(dat, 2, is.na) # check NAs
        dat <- dat[!apply(chkNAs, 1, prod),, drop=FALSE]
        dat <- dat[,!apply(chkNAs, 2, prod), drop=FALSE]
      }
    }
    if (class(dat) == "numeric" | class(dat) == "integer") {
      dat <- dat[!is.na(dat)]
    }
    as.matrix(dat)
  })
  
  c_data <- reactive({
    file2 <- input$file2
    if (is.null(file2)) return(NULL)
    dat <- as.matrix(read.table(file2$datapath, header = FALSE,
                    sep = input$sep, stringsAsFactors=FALSE, check.names=FALSE, row=1))
    c_t <- as.numeric(t(dat))
    names(c_t) <- as.numeric(rownames(dat))
    # if (class(dat) == "data.frame" | class(dat) == "matrix") {
    #   if (nrow(dat) > 1) {
    #     chkNAs <- apply(dat, 2, is.na) # check NAs
    #     dat <- dat[!apply(chkNAs, 1, prod),, drop=FALSE]
    #     dat <- dat[,!apply(chkNAs, 2, prod), drop=FALSE]
    #   }
    # }
    # if (class(dat) == "numeric" | class(dat) == "integer") {
    #   dat <- dat[!is.na(dat)]
    # }
    c_t
  })
  
  i_data <- reactive({
    file3 <- input$file3
    if (is.null(file3)) return(NULL)
    dat <- as.matrix(read.table(file3$datapath, header = FALSE,
                                sep = input$sep, stringsAsFactors=FALSE, check.names=FALSE, row=1))
    i_t <- as.numeric(t(dat))
    names(i_t) <- as.numeric(rownames(dat))
    # if (class(dat) == "data.frame" | class(dat) == "matrix") {
    #   if (nrow(dat) > 1) {
    #     chkNAs <- apply(dat, 2, is.na) # check NAs
    #     dat <- dat[!apply(chkNAs, 1, prod),, drop=FALSE]
    #     dat <- dat[,!apply(chkNAs, 2, prod), drop=FALSE]
    #   }
    # }
    # if (class(dat) == "numeric" | class(dat) == "integer") {
    #   dat <- dat[!is.na(dat)]
    # }
    i_t
  })
  
  total_years <- reactive({
    if(is.null(lc_data()) & is.null(c_data()) & is.null(i_data())) return(NULL)
    minyr <- NULL
    maxyr <- NULL
    if(!is.null(lc_data())){
      lc <- lc_data()
      lc_yrs <- as.numeric(rownames(lc))
      minyr <- min(lc_yrs)
      maxyr <- max(lc_yrs)
    }
    if(!is.null(c_data())){
      c_t <- c_data()
      c_yrs <- as.numeric(names(c_t))
      minyr <- min(c(min(c_yrs), minyr))
      maxyr <- max(c(max(c_yrs), maxyr))
    }
    if(!is.null(i_data())){
      i_t <- i_data()
      i_yrs <- as.numeric(names(i_t))
      minyr <- min(c(min(i_yrs), minyr))
      maxyr <- max(c(max(i_yrs), maxyr))
    }
    
    all_yrs <- minyr:maxyr
    if(length(all_yrs)>1000) stop("Some years may be in indexed numbers (e.g. 1, 2, 3) while other years may be in AD (e.g. 2000, 2001, 2002)")
    if(all_yrs[1]<1000) all_yrs <- 1:maxyr
    return(as.numeric(all_yrs))
  })

  output$displayTyears <- renderTable({
        if(!is.null(total_years())){
          mat <- matrix(as.character(total_years()), nrow=1, ncol=length(total_years()))
        }
  }, colnames=FALSE, caption="Total years:", caption.placement=getOption("xtable.caption.placement", "top"), caption.width=getOption("xtable.caption.width", NULL))
  
  output$peekLC <- renderTable({
    if(!is.null(lc_data())){
      lc_data()
    }
  }, colnames=TRUE, rownames=TRUE, caption="Length data:", caption.placement=getOption("xtable.caption.placement", "top"), caption.width=getOption("xtable.caption.width", NULL))
  
  output$displayLCyears <- renderTable({
    if(!is.null(lc_data())){
      mat <- matrix(as.character(rownames(lc_data())), nrow=1, ncol=nrow(lc_data()))
    }
  }, colnames=FALSE, caption="Length years:", caption.placement=getOption("xtable.caption.placement", "top"), caption.width=getOption("xtable.caption.width", NULL))
  
  output$displayCyears <- renderTable({
    if(!is.null(c_data())){
      mat <- matrix(as.character(names(c_data())), nrow=1, ncol=length(c_data()))
    }
  }, colnames=FALSE, caption="Catch years:", caption.placement=getOption("xtable.caption.placement", "top"), caption.width=getOption("xtable.caption.width", NULL))
  
  output$displayIyears <- renderTable({
    if(!is.null(i_data())){
      mat <- matrix(as.character(names(i_data())), nrow=1, ncol=length(i_data()))
    }
  }, colnames=FALSE, caption="Index years:", caption.placement=getOption("xtable.caption.placement", "top"), caption.width=getOption("xtable.caption.width", NULL))
  
  # output$dataInfo <- renderPlot({
  #   par(mfrow=c(1,1))
  #   tyrs <- total_years()
  #   plot(x=1,y=1,type="n",axes=F, ann=F, xlim=c(1,max(1,length(tyrs))), ylim=c(0,1))
  #   if(!is.null(total_years())){
  #     text(x=1,y=1, "Total years:", xpd=NA, font=2)
  #     sapply(1:length(tyrs), function(x) text(x=x, y=0.9, tyrs[x], xpd=NA))
  #   }
  #   if(!is.null(lc_data())){
  #     text(x=1, y=0.7, "Length years:", xpd=NA, font=2)
  #     lc <- lc_data()
  #     lc_yrs <- as.numeric(rownames(lc))
  #     sapply(1:length(lc_yrs), function(x) text(x=x, y=0.6, lc_yrs[x], xpd=NA))
  #   }
  #   
  # }, height=200, width=500)
  # 
  output$plot_LC1 <- renderPlot({
    if(is.null(lc_data())) return(NULL)
    if(!is.null(lc_data())){
      lc <- lc_data()
      yrs <- as.numeric(rownames(lc))
      lbins <- as.numeric(colnames(lc))
      lc_inp <- list("LF"=lc)
      plot_LCfits(Inputs=lc_inp, true_lc_years=yrs)
    }
  })
  
  output$CatchIndex <- renderPlot({
    if(is.null(c_data()) & is.null(i_data())) return(NULL)
    par(mfrow=c(1,2))
    if(is.null(c_data())==FALSE){
      c_t <- c_data()
      c_yrs <- as.numeric(names(c_t))
      all_yrs <- total_years()
      catch_plot <- rep(NA, length(all_yrs))
      catch_plot[c_yrs] <- c_t
      plot(all_yrs, catch_plot, lwd=4, type="o", cex.axis=1.5, xlab="Year", ylab="Catch", cex.lab=1.5, ylim=c(0, max(c_t)*1.1))
    }
    if(is.null(i_data())==FALSE){
      i_t <- i_data()
      i_yrs <- as.numeric(names(i_t))
      all_yrs <- total_years()
      index_plot <- rep(NA, length(all_yrs))
      index_plot[i_yrs] <- i_t
      plot(all_yrs, index_plot, lwd=4, type="o", cex.axis=1.5, xlab="Year", ylab="Abundance index", cex.lab=1.5, ylim=c(0, max(i_t)*1.1))
    }
  })
  
  output$plot_LC2 <- renderPlot({
    if(is.null(lc_data())) return(NULL)
      lc <- lc_data()
      yrs <- as.numeric(rownames(lc))
      lbins <- as.numeric(colnames(lc))
      Inputs <- list("LF"=lc)
      if(values$goRun){
        res <- doAssess()
        Report <- readRDS(file.path(path(), "Report.rds"))
        Inputs <- readRDS(file.path(path(), "Inputs.rds"))
        plot_LCfits(Inputs=Inputs$Data, Report=Report, true_lc_years=yrs)
      }
       if(values$goRun==FALSE){
         Report <- NULL
         plot_LCfits(Inputs=Inputs, Report=Report, true_lc_years=yrs)
       }
  })
  
  output$CatchIndex2 <- renderPlot({
    if(is.null(c_data()) & is.null(i_data())) return(NULL)
    par(mfrow=c(1,2))
    if(is.null(c_data())==FALSE){
      c_t <- c_data()
      c_yrs <- as.numeric(names(c_t))
      all_yrs <- total_years()
      catch_plot <- rep(NA, length(all_yrs))
      catch_plot[c_yrs] <- c_t
      if(values$goRun==FALSE) plot(all_yrs, catch_plot, lwd=4, type="o", cex.axis=1.5, xlab="Year", ylab="Catch", cex.lab=1.5, ylim=c(0, max(c_t)*1.1))
      if(values$goRun){
        res <- doAssess()
        Report <- readRDS(file.path(path(), "Report.rds"))
        Sdreport <- readRDS(file.path(path(), "Sdreport.rds"))
        if(all(is.na(Sdreport))==FALSE){
          sd <- summary(Sdreport)[which(rownames(summary(Sdreport))=="lC_t"),]
          sd[,2][which(is.na(sd[,2]))] <- 0
          ylim <- c(0, max(read_sdreport(sd, log=TRUE))*1.2)
        }
        plot(all_yrs, catch_plot, lwd=4, type="o", cex.axis=1.5, xlab="Year", ylab="Catch", cex.lab=1.5, ylim=ylim)
        if(all(is.na(Sdreport))==FALSE) polygon(y=read_sdreport(sd, log=TRUE), x=c(which(is.na(sd[,2])==FALSE), rev(which(is.na(sd[,2])==FALSE))), col=rgb(0,0,1,alpha=0.2), border=NA)
        lines(all_yrs, Report$C_t, lwd=2, col="blue", pch=19, type="o")
      }
    }
      if(is.null(i_data())==FALSE){
        i_t <- i_data()
        i_yrs <- as.numeric(names(i_t))
        all_yrs <- total_years()
        index_plot <- rep(NA, length(all_yrs))
        index_plot[i_yrs] <- i_t
        if(values$goRun==FALSE) plot(all_yrs, index_plot, lwd=4, type="o", cex.axis=1.5, xlab="Year", ylab="Index", cex.lab=1.5, ylim=c(0, max(i_t)*1.1))
        if(values$goRun){
          res <- doAssess()
          Report <- readRDS(file.path(path(), "Report.rds"))
          Sdreport <- readRDS(file.path(path(), "Sdreport.rds"))
          if(all(is.na(Sdreport))==FALSE){
            sd <- summary(Sdreport)[which(rownames(summary(Sdreport))=="lI_t"),]
            sd[,2][which(is.na(sd[,2]))] <- 0
            ylim <- c(0, max(read_sdreport(sd, log=TRUE))*1.2)
          }
          plot(all_yrs, index_plot, lwd=4, type="o", cex.axis=1.5, xlab="Year", ylab="Index", cex.lab=1.5, ylim=ylim)
          if(all(is.na(Sdreport))==FALSE) polygon(y=read_sdreport(sd, log=TRUE), x=c(which(is.na(sd[,2])==FALSE), rev(which(is.na(sd[,2])==FALSE))), col=rgb(0,0,1,alpha=0.2), border=NA)
          lines(all_yrs, Report$I_t, lwd=2, col="blue", pch=19, type='o')
        }
    }
  })
  
  output$plotSelex2 <- renderPlot({
    if(is.list(get_lh())==FALSE) return(NULL)
    lh <- get_lh()
    par(mfrow=c(1,1), mar=c(5,5,5,0))
    plot(lh$S_l, type="l", lwd=3, col="blue", xlab="Length", ylab="Proportion vulnerable to gear", cex.main=2, main="Selectivity-at-length", cex.axis=2, cex.lab=2)
  }, height=500, width=600)
  
  output$clickAssess <- renderUI({
    fluidRow(
      actionButton("goAssess", "Fit Model", icon("line-chart"), style="color: #fff; background-color: #00B700; border-color: #006D00"),
      style="padding: 15px 15px 15px 15px;")
    
  })
  
  shinyDirChoose(input, 'directory', roots=c(wd="."), restrictions=system.file(package='base'))
  
  path <- reactive({
    if(is.null(input$directory)) return(NULL)
    parseDirPath(c(wd="."), input$directory)
  })
  
  output$printPath <- renderPrint({
    if(is.null(path())) return(NULL)
    path()
  })
  
  doAssess <- reactive({
    if(values$goRun==FALSE) return(NULL)
    if(values$goRun==TRUE){
      
      run_dir <- path()
      
      lh <- get_lh()
      lf <- lc_data()
      c_t <- c_data()
      i_t <- i_data()
      yrs <- total_years()
      input_data <- list("years"=yrs, "LF"=lf, "C_t"=c_t, "I_t"=i_t)
      
      data_avail <- NULL
      if(input$lc_avail) data_avail <- "LC"
      if(input$i_avail) data_avail <- paste0("Index_", data_avail)
      if(input$c_avail){
        data_avail <- paste0("Catch_", data_avail)
        if(input$C_type=="Biomass") C_opt <- 2
        if(input$C_type=="Numbers") C_opt <- 1
      }
      if(input$c_avail==FALSE) C_opt <- 0
      
      est_sigma <- FALSE
      if(input$est_sigmaR) est_sigma <- "log_sigma_R"
      
      if(input$est_selex) S_l_input <- -1
      if(input$est_selex==FALSE) S_l_input <- lh$S_l
    
      
      res <- run_LIME(modpath=run_dir, lh=lh, input_data=input_data, data_avail=data_avail, est_sigma=est_sigma, S_l_input=S_l_input, C_opt=C_opt, newtonsteps=3, rewrite=TRUE)
      return(res)
    }
  })
  

  output$checkConverge <- renderTable({
    if(values$goRun==FALSE) return(NULL)
    res <- doAssess()
    df <- readRDS(file.path(path(), "check_convergence.rds"))
    colnames(df) <- c("Final gradient","Parameter","Estimate","Transformed estimate")
    df
  }, colnames=TRUE, digits=4, options=list(pageLength=-1, searching=FALSE, paging=FALSE, ordering=FALSE, info=FALSE))

  output$plotResults <- renderPlot({
    if(values$goRun==FALSE) return(NULL)
    if(values$goRun){
      lh <- get_lh()
      res <- doAssess()
      Report <- readRDS(file.path(path(), "Report.rds"))
      Inputs <- readRDS(file.path(path(), "Inputs.rds"))
      Sdreport <- readRDS(file.path(path(), "Sdreport.rds"))
      plot_output(all_years=Inputs$Data$T_yrs, lc_years=Inputs$Data$LC_yrs, true_years=Inputs$Data$T_yrs, Inputs=Inputs, Report=Report, Sdreport=Sdreport, lh=lh, plot=c("Fish", "Rec", "SPR", "Selex"))
    }
  })
  
  
})