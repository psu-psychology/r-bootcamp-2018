#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#defaults
njobs <- 15
data4D <- ""
data4D <- "/gpfs/group/mnh5174/default/Michael/rest_test_018lq/rest1/nfawuktm_rest1.nii.gz"
fname_brainmask <- ""
fname_brainmask <- "/gpfs/group/mnh5174/default/lab_resources/standard/mni_icbm152_nlin_asym_09c/mni_icbm152_t1_tal_nlin_asym_09c_mask_2.3mm.nii"
stat_prefix <- "arima_statistics"
resid_outfile <- "arima_residuals"
max_d <- 0 #default to ARMA model (don't allow integrated component)
max_p <- 12 #max AR order at 12 by default
max_q <- 5 #max MA order at 5 by default
search_pdq <- TRUE #by default search for best ARIMA model at each voxel
fit_p <- fit_d <- fit_q <- NULL #default to search for order; these are used if user specifies a particular model order
white_lags <- 5 #how far out should we look for residual autocorrelation?

argpos <- 1
while (argpos <= length(args)) {
  if (args[argpos] == "-ts") {
    data4D <- args[argpos + 1] #name of 4D fMRI data to be fit
    argpos <- argpos + 2
  } else if (args[argpos] == "-out_statistics_prefix") {
    stat_prefix <- args[argpos + 1] #prefix of statistics files to be written
    argpos <- argpos + 2
  } else if (args[argpos] == "-out_residuals_file") {
    resid_outfile <- args[argpos + 1]
    argpos <- argpos + 2
  } else if (args[argpos] == "-max_p") {
    max_p <- as.integer(args[argpos + 1])
    if (is.na(max_p)) { stop("Could not understand argument ", args[argpos+1], "to -max_p") }
  } else if (args[argpos] == "-max_d") {
    max_d <- as.integer(args[argpos + 1])
    if (is.na(max_d)) { stop("Could not understand argument ", args[argpos+1], "to -max_d") }
  } else if (args[argpos] == "-max_q") {
    max_q <- as.integer(args[argpos + 1])
    if (is.na(max_q)) { stop("Could not understand argument ", args[argpos+1], "to -max_q") }
  } else if (args[argpos] == "-fit_pdq") {
    #fit ARIMA model of order p, d, q
    #this is an alternative to automated model search and is appropriate if user decides to fit same model to all participants/voxels
    stopifnot(length(args) >= argspos + 3) #make sure we have at least 3 coefs that follow
    fit_p <- as.integer(args[argpos + 1])
    fit_d <- as.integer(args[argpos + 2])
    fit_q <- as.integer(args[argpos + 3])
    search_pdq <- FALSE
    
    if (is.na(fit_p)) { stop("Could not understand first (p) argument to -fit_pdq ", args[argpos+1]) }
    if (is.na(fit_d)) { stop("Could not understand second (d) argument to -fit_pdq ", args[argpos+2]) }
    if (is.na(fit_q)) { stop("Could not understand third (q) argument to -fit_pdq ", args[argpos+3]) }
    
    search_pdq <- FALSE
  } else if (args[argpos] == "-brainmask") {
    fname_brainmask <- args[argpos + 1] #mask file for brain voxels
    argpos <- argpos + 2
    stopifnot(file.exists(fname_brainmask))    
  } else if (args[argpos] == "-njobs") {
    njobs <- as.integer(args[argpos + 1])
    argpos <- argpos + 2
    if (is.na(njobs)) { stop("-njobs must be an integer") }
  } else if (args[argpos] == "-na_string") {
    na_string <- args[argpos + 1]
    argpos <- argpos + 2
  } else if (args[argpos] == "-dropvols") {
    dropvols <- as.numeric(args[argpos + 1]) #number of vols to drop
    if (is.na(dropvols)) { stop("Could not understand argument ", args[argpos+1], "to -dropvols") }
    argpos <- argpos + 2
  } else if (args[argpos] == "-port") {
    clustersocketport <- as.integer(args[argpos + 1])
    argpos <- argpos + 2
    if (is.na(njobs)) { stop("-port must be an integer") }
  } else {
    stop("Not sure what to do with argument: ", args[argpos])
  }
}

#handle package dependencies
for (pkg in c("forecast", "oro.nifti", "doParallel", "lmtest", "tictoc", "pracma")) {
  if (!suppressMessages(require(pkg, character.only=TRUE))) {
    message("Installing missing package dependency: ", pkg)
    install.packages(pkg)
    suppressMessages(require(pkg, character.only=TRUE))
  }
}

#worker function f
arimafit <- function(ts, search_pdq=TRUE, max.p=12, max.d=0, max.q=5, fit.p=NULL, fit.d=NULL, fit.q=NULL, white_lags=5) {
  if (search_pdq) {
    d <- ifelse(max.d > 0, 0, NA) #if max.d is > 0, then allow an integrated component to be in the model. Set d = NA to have auto.arima estimate the initial value.

    #select among alternative arima model orders based on AICC
    mout <- forecast::auto.arima(as.vector(ts), approximation=FALSE, stepwise=FALSE, seasonal=FALSE, allowdrift=TRUE, allowmean=TRUE,
                                 ic="aicc", d=d, max.d=max.d, max.p=max.p, max.q=max.q)
  } else {
    #fit model of specified order
    mout <- forecast::Arima(as.vector(ts), order=c(fit.p, fit.d, fit.q), include.drift=TRUE, include.mean=TRUE)
  }
  
  #whiteness checks using Breusch-Godfrey or Ljung-Box test (do the residuals have remaining autocorrelation?)
  if (white_lags > 0) {
    whitechecks <- sapply(1:white_lags, function(lag) {
      #deprecate Ljung-Box; use Breusch-Godfrey
      #https://stats.stackexchange.com/questions/148004/testing-for-autocorrelation-ljung-box-versus-breusch-godfrey
      #test <- Box.test(mout$residuals, lag=lag, type="Ljung-Box")
      #test$p.value
      test <- lmtest::bgtest(mout$residuals ~ 1, order=lag)
      test$p.value
    })
  } else {
    whitechecks <- c()
  }
  
  return(list(resid_ts=mout$residuals, arima_order=forecast::arimaorder(mout), white_bg_pvals=setNames(whitechecks, paste0("bg_p_", 1:length(whitechecks))), arima_coefs=coef(mout)))
  #arima_ses=sqrt(diag(mout$var.coef)) #omitting arma coefficient standard errors for now for computational speed (not using them for anything)
}

message("Running voxelwise ARIMA model fit")
message("Number of jobs for computation:", njobs)
message("Input 4D dataset:", data4D)
if (search_pdq) {
  message("Using AICC to adjudicate among different ARIMA model orders for p, d, q")
  message("Maximum model orders for each term:")
  message("   AR order (p):", max_p)
  message("   integrated order (d):", max_d, ifelse(max_d==0, " [I component disabled; ARMA only]"))
  message("   MA order (p):", max_q)  
} else {
  if (is.null(fit_p)) { stop("Model order must be passed in using -fit_pdq if not using model search") }
  message("Using user specifications for ARIMA model order:", fit_q, ",", fit_d, ",", fit_q)
}

stopifnot(file.exists(data4D))
fmri_ts_data <- readNIfTI(data4D, reorient=FALSE)
ntimepoints <- dim(fmri_ts_data)[4]

#apply brain mask, if requested
#optional: apply brain mask
if (!is.null(fname_brainmask)) {
  message("Applying brain mask to 4D data: ", fname_brainmask)
  stopifnot(file.exists(fname_brainmask))
  if (grepl("^.*\\.(HEAD|BRIK|BRIK.gz)$", fname_brainmask, perl=TRUE)) {
    brainmask <- readAFNI(fname_brainmask)
  } else {
    brainmask <- readNIfTI(fname_brainmask, reorient=FALSE)
  }
  
  #brain mask and roi mask must be of same dimension
  stopifnot(identical(dim(brainmask)[1:3], dim(fmri_ts_data)[1:3]))
  stopifnot(all.equal(sort(unique(as.vector(brainmask))), c(0, 1))) #make sure mask has only zeros and ones
  
  #use the [] in front to keep the data dimensions the same -- otherwise, apply collapses x,y,z
  fmri_ts_data@.Data[] <- apply(fmri_ts_data@.Data, 4, function(volume) { volume * brainmask })
}

#only apply arima fitting to non-constant voxels (note that masking above sets irrelevant voxels to 0 -- therefore, constant)
nonconst <- apply(fmri_ts_data, c(1,2,3), function(ts) { !all(ts==ts[1]) })
toprocess <- apply(fmri_ts_data, 4, function(x) { x[nonconst] })
rownames(toprocess) <- 1:nrow(toprocess) #used to set progress bar inside loop
mi <- which(nonconst==TRUE, arr.ind=TRUE)

#toprocess = toprocess[1:3000,]
#mi = mi[1:3000,]

if (njobs > 1) {
  cl <- parallel::makePSOCKcluster(njobs, outfile = "")
  registerDoParallel(cl)
} else {
  registerDoSEQ()
}


message("fMRI data to analyze consist of ", nrow(toprocess), " voxels and ", ncol(toprocess), " timepoints")

if (search_pdq) {
  base_coef_vector <- setNames(rep(0, sum(max_p, max_q) + 1), c(paste0("ar", 1:max_p), paste0("ma", 1:max_q), "intercept"))
} else {
  base_coef_vector <- setNames(rep(0, sum(fit_p, fit_q) + 1), c(paste0("ar", 1:fit_p), paste0("ma", 1:fit_q)), "intercept")
}

n_arma_coef <- length(base_coef_vector) #number of coefficients expected in output vector

#use row-wise iterator to avoid exporting the entire voxels x time matrix to each worker
message("Starting voxelwise ARIMA fitting")
pb <- txtProgressBar(0, max = nrow(toprocess), style = 3)
tic("arima fitting")
vox_results <- foreach(v=iter(toprocess, by="row"), .noexport="fmri_ts_data", .packages="forecast", .inorder=TRUE, .multicombine=TRUE, .combine=rbind) %dopar% { #, .options.snow = opts
  it <- as.numeric(row.names(v)[1]) #use row number to set progress bar
  if (it %% 100 == 0) { setTxtProgressBar(pb, it) }
  vfit <- arimafit(v, search_pdq=search_pdq, max.p=max_p, max.d=max_d, max.q=max_q, fit.p=fit_p, fit.d=fit_d, fit.q=fit_q) #fit arima to voxel
  arima_coef <- base_coef_vector #start with empty vector of max order for each model part
  arima_coef[names(vfit$arima_coef)] <- vfit$arima_coef #populate non-zero elements of fitted ARMA coefficients
  c(vfit$resid_ts, vfit$arima_order[c("p", "d", "q")], vfit$white_bg_pvals, arima_coef) #concatenating into a long vector rather than try to get 2+ objects out
}
toc()

close(pb)

#shut down workers
if (njobs > 1) { stopCluster(cl) }

#setup separate nifti outputs for residuals versus statistics
order_out <- fmri_ts_data #model order
white_out <- fmri_ts_data #whiteness checks
coef_out <- fmri_ts_data #arima coefs
resid_out <- fmri_ts_data #residuals

order_out@.Data <- array(0, dim=c(dim(fmri_ts_data)[1:3], 3))
order_out@dim_[5] <- 3 #change size of 4th dim

white_out@.Data <- array(0, dim=c(dim(fmri_ts_data)[1:3], white_lags))
white_out@dim_[5] <- white_lags #change size of 4th dim

coef_out@.Data <- array(0, dim=c(dim(fmri_ts_data)[1:3], n_arma_coef))
coef_out@dim_[5] <- n_arma_coef #change size of 4th dim

pdq <- vox_results[,(ntimepoints+1):(ntimepoints+3)] #pdq are first 3 elements after the residuals
white_mat <- vox_results[,(ntimepoints+3+1):(ntimepoints+4+white_lags-1)] #whiteness p values are the next white_lags elements in time dimension
coef_mat <- vox_results[,(ntimepoints+3+white_lags+1):(ntimepoints+3+white_lags+1+n_arma_coef-1)] #arma coefficients are the remainder

vox_results <- vox_results[,1:ntimepoints] #drop stats from residuals

message("Populating NIfTI outputs with AR(I)MA results")
tic("matrix update")
#back to old repmat strategy
miassign <- cbind(pracma::repmat(mi, ncol(vox_results), 1), rep(1:ncol(vox_results), each=nrow(vox_results)))
resid_out@.Data[miassign] <- vox_results

miassign <- cbind(pracma::repmat(mi, ncol(pdq), 1), rep(1:ncol(pdq), each=nrow(pdq)))
order_out@.Data[miassign] <- pdq

miassign <- cbind(pracma::repmat(mi, ncol(white_mat), 1), rep(1:ncol(white_mat), each=nrow(white_mat)))
white_out@.Data[miassign] <- white_mat

miassign <- cbind(pracma::repmat(mi, ncol(coef_mat), 1), rep(1:ncol(coef_mat), each=nrow(coef_mat)))
coef_out@.Data[miassign] <- coef_mat
toc()

message("Writing NIfTI outputs to disk")
tic("disk i/o")
#add min/max to header to have it play well across packages
order_out@cal_min <- min(order_out)
order_out@cal_max <- max(order_out)
writeNIfTI(order_out, filename=paste0(stat_prefix, "_model_order"))

white_out@cal_min <- min(white_out)
white_out@cal_max <- max(white_out)
writeNIfTI(white_out, filename=paste0(stat_prefix, "_whiteness_pvals"))

coef_out@cal_min <- min(coef_out)
coef_out@cal_max <- max(coef_out)
writeNIfTI(coef_out, filename=paste0(stat_prefix, "_arma_coef"))
  
resid_out@cal_min <- min(resid_out)
resid_out@cal_max <- max(resid_out)
writeNIfTI(resid_out, filename=resid_outfile)
toc()

message("Voxelwise AR(I)MA fitting complete")
#this completely blows the ram...
#abc@.Data <- abind(lapply(data.frame(abc), function(col) {
#            m <- array(0, dim(nonconst)[1:3]) #empty 3d matrix matching dims of images
#            m[nonconst] <- col
#            return(m)
#          }), along=4)
