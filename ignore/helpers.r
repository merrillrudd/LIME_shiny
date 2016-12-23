dir <- "C:\\Git_Projects\\LIME_shiny"
setwd(dir)
library(LIME)

input <- NULL
input$linf <- 64
input$vbk <- 0.2
input$ML50 <- 34
input$lwa <- 0.025
input$lwb <- 3
input$SL50 <- 37
input$binwidth <- 1
input$CVlen <- 0.1
input$SigmaR <- 0.01
input$SigmaF <- 0.01
input$Fdynamics <- "Constant"
input$Rdynamics <- "Constant"
input$sep <- ","
input$Nyears <- 20
input$est_sigR <- TRUE
input$est_CVL <- FALSE
input$est_sigC <- FALSE
input$est_sigI <- FALSE
input$goButton <- TRUE

file1 <- read.csv(file.path(dir, "sim", "LC20.csv"))
file2 <- read.csv(file.path(dir, "sim", "Index.csv"))
file3 <- read.csv(file.path(dir, "sim", "Catch.csv"))


data_LC <- function(){
	file1[,2:ncol(file1)]
}
data_Index <- function(){
	as.matrix(file2[,2])
}
data_Catch <- function(){
	as.matrix(file3[,2])
}
years_LC <- function(){
	as.numeric(file1[,1])
}
years_Index <- function(){
	as.numeric(file2[,1])
}
years_Catch <- function(){
	as.numeric(file3[,1])
}

modpath=NULL
write=FALSE
lh=lh
input_data=input_data
est_sigma=est_sigma
data_avail=data_avail
rewrite=TRUE
fix_f=0
simulation=FALSE
REML=FALSE
f_true=FALSE
fix_param=FALSE
param_adjust=param_adjust
val_adjust=val_adjust