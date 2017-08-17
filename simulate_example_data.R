dir <- "C:\\Git_Projects\\LIME_shiny"
setwd(dir)

library(LIME)

## life history information based on medium-lived snapper
## include recruitment variation, fishing mortality variation, and recruitment autocorrelation
lh <- create_lh_list(linf=65, vbk=0.21, t0=-0.01, M=0.38, M50=34, M95=39, maturity_input="length", lwa=0.025, lwb=2.79, S50=20, S95=26, selex_input="length", selex_type="logistic", nseasons=1, SigmaR=0.737, SigmaF=0.2, rho=0.436)

## simulate true population with ramped fishing mortality (two-way trip), autocorrelated and variable recruitment
## generate 20 years of data with 200 length measurements annually
## input life history information
## assume initial depletion = 80%
save_pop <- generate_data(modpath=file.path(getwd(), "data"), lh=lh, Fdynamics="Ramp", Rdynamics="AR", Nyears=20, Nyears_comp=20, comp_sample=200, data_avail="Index_Catch_LC", itervec=1, init_depl=0.8, derive_quants=TRUE)

true_pop <- readRDS(file.path(getwd(),"data", 1, "True.rds"))

## full length comp
LF <- true_pop$LF

	## length comp in the last year
	LF1 <- t(as.matrix(LF[nrow(LF),]))
	rownames(LF1) <- rownames(LF)[nrow(LF)]

	## last 10 years of length comp
	LF10 <- as.matrix(LF[(nrow(LF)-9):nrow(LF),])

	## 10 years of length comp, skipping years over 20 year period
	LFskip <- as.matrix(LF[c(5:7,13:15,17:20),])

## catch in weight
C_t <- true_pop$Cw_t

	## ten years of catch data
	C_t10 <- C_t[(length(C_t)-9):length(C_t)]

## abundance index
I_t <- true_pop$I_t
	
	## ten years abundance index
	I_t10 <- I_t[(length(I_t)-9):length(I_t)]

write.csv(LF, file.path(getwd(), "data", "LF_20yrs.csv"))
write.csv(LF1, file.path(getwd(), "data", "LF_1yr.csv"))
write.csv(LF10, file.path(getwd(), "data", "LF_10yrs.csv"))
write.csv(LFskip, file.path(getwd(), "data", "LF_Skipyrs.csv"))

write.csv(t(as.matrix(C_t)), file.path(getwd(), "data", "Catch_20yrs.csv"), row.names=FALSE)
write.csv(t(as.matrix(C_t10)), file.path(getwd(), "data", "Catch_10yrs.csv"), row.names=FALSE)
write.csv(t(as.matrix(I_t)), file.path(getwd(), "data", "Index_20yrs.csv"), row.names=FALSE)
write.csv(t(as.matrix(I_t10)), file.path(getwd(), "data", "Index_10yrs.csv"), row.names=FALSE)