#build zips

ram_efa <- c("talks/RBootcamp_IntroFactorAnalysis_2018_0816.pdf", "talks/IntroBasicEFA_2018_0815.html", "talks/IntroBasicEFA_2018_0815.Rmd", "data/dataBIG5.csv.gz", "data/ptechdata.csv")
system(paste("zip zips/ram_efa.zip", paste(ram_efa, collapse=" ")))

hallquist_lavaan <- c("talks/lavaan_tutorial.html", "talks/lavaan_tutorial.Rmd", "img/threshold_structure.jpg")
system(paste("zip zips/hallquist_lavaan.zip", paste(hallquist_lavaan, collapse=" ")))

hallquist_parallel <- c("talks/parallel_r.html", "talks/parallel_r.Rmd", "img/few_bottles.jpg",
                        "img/split-apply-combine.png", "img/fork.png", "img/levy_geospatial.png",
                        "img/wine_cellar.jpg")
system(paste("zip zips/hallquist_parallel.zip", paste(hallquist_parallel, collapse=" ")))

johnson_joins <- c("talks/joins_tutorial_bnj.html", "talks/joins_tutorial_bnj.Rmd", "img/anti-join.gif", 
                   "img/full-join.gif", "img/inner-join.gif", "img/left-join-extra.gif", "img/left-join.gif", 
                   "img/right-join.gif", "img/semi-join.gif", "img/original-dfs.png")
system(paste("zip zips/johnson_joins.zip", paste(johnson_joins, collapse=" ")))

