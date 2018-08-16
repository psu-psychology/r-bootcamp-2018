#Code from MNH Aug2018
#The goal of this script is to run a series of multilevel models in R for each time and frequency bin from several different MEG sensors.
#Conceptually, we have many trials of an experiment, which can evoke power (amplitude) changes at different frequencies.
#Thus, trials are nested within subject.

setwd("/gpfs/group/mnh5174/default/Michael/Clock_MEG")
library(ggplot2)
library(dplyr)
library(cowplot)
library(foreach)
library(doParallel)
library(readr)
library(lme4)

f <- Sys.getenv('PBS_NODEFILE')
nodelist <- if (nzchar(f)) readLines(f) else rep('localhost', 3)

cat("Node list allocated to this job\n")
print(nodelist)

#Setup one cluster to be used for all time x frequency x MEG sensor combinations below
cl <- makePSOCKcluster(nodelist, outfile='')
print(cl)

#potentially run specific commands on each worker process (not currently needed)
#clusterEvalQ(cl, { system("module use /gpfs/group/mnh5174/default/sw/modules; module load curl/7.54.0; module load r/3.4.0") })

#tell foreach which cluster object we've setup in R
registerDoParallel(cl)

#each csv file contains data from one sensor
meg_files <- list.files(path="raw_data/clusters", pattern="\\.csv\\.gz", full.names=TRUE)

#worker function for fitting lmer and returning essential statistics
lmer_df <- function(f, df, mname, REML=FALSE) {
  
  #disable REML by default to allow for AIC comparisons
  #use tryCatch to handle model fitting failures gracefully, avoiding broader crashes of computation loop
  m <- tryCatch(lmer(f, df, REML=REML), error=function(e) { print(e); return(NULL) })
  if (!is.null(m)) {
    ret_df <- broom::tidy(car::Anova(m, type=3)) %>% mutate(group="anova") %>% bind_rows(broom::tidy(m))
    ret_df$Time <- df$Time[1]; ret_df$Freq <- df$Freq[1]; ret_df$model <- mname; ret_df$AIC <- AIC(m)
    diag <- ad.test(resid(m)) #anderson darling normality test
    ret_df$resid_W <- diag$statistic; ret_df$resid_p <- diag$p.value
    
    #DHARMa checks on residuals
    dout <- simulateResiduals(fittedModel = m, refit = FALSE, n=500)
    uniftest <- testUniformity(dout)
    ret_df$simresid_KS_D=uniftest$statistic; ret_df$simresid_KS_p=uniftest$p.value     
    
    return(ret_df)
  } else {
    return(NULL)
  }
}

print(meg_files)

##Note that files are organized by MEG sensor 'cluster' and frequency bin (wavelet decomposition)
##first 12 included here for reference:
# [1] "raw_data/clusters/cluster1_MEG0231_10.88398400730387_data.csv.gz"
# [2] "raw_data/clusters/cluster1_MEG0231_12.89328803558145_data.csv.gz"
# [3] "raw_data/clusters/cluster1_MEG0231_15.273531847980658_data.csv.gz"
# [4] "raw_data/clusters/cluster1_MEG0231_18.09319503818554_data.csv.gz"
# [5] "raw_data/clusters/cluster1_MEG0231_2.0_data.csv.gz"
# [6] "raw_data/clusters/cluster1_MEG0231_2.3692221574249293_data.csv.gz"
# [7] "raw_data/clusters/cluster1_MEG0231_2.806606815616618_data.csv.gz"
# [8] "raw_data/clusters/cluster1_MEG0231_21.43339929153999_data.csv.gz"
# [9] "raw_data/clusters/cluster1_MEG0231_25.390242255226163_data.csv.gz"
# [10] "raw_data/clusters/cluster1_MEG0231_3.3247375273693565_data.csv.gz"
# [11] "raw_data/clusters/cluster1_MEG0231_3.938520908732826_data.csv.gz"
# [12] "raw_data/clusters/cluster1_MEG0231_30.07756226673427_data.csv.gz"

for (f in meg_files) {
  meg_df <- read_csv(f) #read data using readr function, which is much faster than read.csv
  
  meg_df <- meg_df %>% mutate(Pow_dB = 10*log10(Pow + 101)) %>% arrange(Subject, Trial) #-100 is the min
  
  #Pe refers to reward prediction error
  meg_df$Pe_binary <- as.numeric(meg_df$Pe > 0) #binary representation of positive prediction error
  meg_df$Pe_mid <- as.numeric(meg_df$Pe > quantile(meg_df$Pe, .25))
  
  #run lmer by time and frequency bin. each df contains one frequency bin
  #thus, we need to divide over time bins for independent models
  df_split <- split(meg_df, meg_df$Time)
  
  #loop over time points (100) within a given frequency (file)
  #use of iter() ensures that the objects sent to workers are smaller in memory than the global object
  #that is, each worker only receives one element of df_split to chew on, now the whole thing.
  res <- foreach(tp=iter(df_split), .packages=c("lme4", "dplyr", "broom", "DHARMa", "car", "nortest"), .noexport=c("meg_df", "df_split")) %dopar% {
    #transform the data somewhat to 
    tp <- tp %>% mutate(Pe_sqrt=sqrt(Pe)) %>%
      mutate_at(vars(Age, Trial, Pow_dB, Pe, Pe_sqrt), funs(z=as.vector(scale(.)))) %>% #overall z scoring (irrespective of subject)
      group_by(Subject) %>%
      mutate(Pe_wi=Pe - mean(Pe, na.rm=TRUE), Pe_pmean=mean(Pe, na.rm=TRUE), #within subject centering plus person means
             Pe_wi_z=as.vector(scale(Pe_wi))) %>% #w/i person z scoring of PEs to reduce b/w differences in magnitude
      ungroup() %>% mutate(Pe_pmean_c = Pe_pmean - mean(Pe_pmean, na.rm=TRUE),
                           Pe_pmean_z = as.vector(scale(Pe_pmean)),
                           Age.c = Age - mean(Age, na.rm=TRUE),
                           Trial_rescale=(Trial - min(Trial))/100) #to get variance components to be on similar scales, need to lower variance of Trial (if using raw)
    
    
    #These are alternative multilevel models that I'd like to estimate and compare
    #m1 <- lmer_df(Pow_dB ~ 1 + Faces + Age_z + Trial_z + (1 + Trial_z | Subject), tp, "m01")
    #m2 <- lmer_df(Pow_dB ~ 1 + Faces + Age_z * Trial_z + (1 + Trial_z | Subject), tp, "m02")
    m3 <- lmer_df(Pow_dB ~ 1 + Faces * Age_z + Trial_z + (1 + Trial_z | Subject), tp, "baseline")
    #m4 <- lmer_df(Pow_dB ~ 1 + Faces * Age_z * Trial_z + (1 + Trial_z | Subject), tp, "m04")
    
    #add PE in (models 1-4 do not have it)
    pe_baseline <- lmer_df(Pow_dB ~ 1 + Faces + Pe_z + Trial_z + (1 | Subject), tp, "pe_baseline")
    pe_binary <- lmer_df(Pow_dB ~ 1 + Faces + Pe_binary + Trial_z + (1 | Subject), tp, "pe_binary")
    pe_q25 <- lmer_df(Pow_dB ~ 1 + Faces + Pe_mid + Trial_z + (1 | Subject), tp, "pe_q25")
    
    m5 <- lmer_df(Pow_dB ~ 1 + Faces * Pe_z + Faces * Age_z + Trial_z + (1 + Trial_z | Subject), tp, "m05")
    m6 <- lmer_df(Pow_dB ~ 1 + Pe_z * Age_z * Faces + Trial_z + (1 + Trial_z | Subject), tp, "m06")
    m7 <- lmer_df(Pow_dB ~ 1 + Pe_z * Age_z * Faces + Trial_z + (1 + Trial_z + Pe_z | Subject), tp, "m07")
    m8 <- lmer_df(Pow_dB ~ 1 + Pe_z * Age_z * Faces * Trial_z + (1 + Trial_z + Pe_z | Subject), tp, "m08")
    m9 <- lmer_df(Pow_dB ~ 1 + Pe_sqrt_z * Age_z * Faces * Trial_z + (1 + Trial_z + Pe_sqrt_z | Subject), tp, "m09") #same as m8, but with sqrt Pe
    m10 <- lmer_df(Pow_dB ~ 1 + Pe_wi * Age_z * Faces + Pe_pmean_c * Age_z * Faces + Trial_z + (1 + Trial_z | Subject), tp, "m10") #add wi versus between as fixed
    m11 <- lmer_df(Pow_dB ~ 1 + Pe_wi_z * Age_z * Faces + Pe_pmean_z * Age_z * Faces + Trial_z + (1 + Trial_z | Subject), tp, "m11") #add z-scored wi versus between as fixed
    
    #each lmer_df call returns a data.frame object with the relevant fit information
    #rbind all alternative models together and return to foreach for broader combination
    rbind(m3, pe_baseline, pe_binary, pe_q25, m5, m6, m7, m8, m9, m10, m11)
  }
  
  #save the results of this model estimation into an RData object with the same name structure as the input CSVs.
  save(file=file.path("output", sub(".csv.gz", "_lmerfits.RData", basename(f), fixed=TRUE)), res)
}

stopCluster(cl)

