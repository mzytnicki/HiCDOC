exampleHiCDOCDataSet <- HiCDOCDataSetFromTabular("data/example.tsv")
set.seed(123); 
parameters(exampleHiCDOCDataSet) <- list(smallChromosomeThreshold = 100)
exampleHiCDOCDataSetProcessed <- HiCDOC(exampleHiCDOCDataSet)

save(exampleHiCDOCDataSet, file="data/exampleHiCDOCDataSet.rda")
save(exampleHiCDOCDataSetProcessed, file="data/exampleHiCDOCDataSetProcessed.rda")



