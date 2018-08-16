#build zips

ram_efa <- c("talks/RBootcamp_IntroFactorAnalysis_2018_0816.pdf", "talks/IntroBasicEFA_2018_0815.html", "talks/IntroBasicEFA_2018_0815.Rmd", "data/dataBIG5.csv.gz", "data/ptechdata.csv")
system(paste("zip zips/ram_efa.zip", paste(ram_efa, collapse=" ")))

hallquist_lavaan <- c("talks/lavaan_tutorial.html", "talks/lavaan_tutorial.Rmd", "img/threshold_structure.jpg")
system(paste("zip zips/hallquist_lavaan.zip", paste(hallquist_lavaan, collapse=" ")))
