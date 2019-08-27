# Perform the classifications on the feature sets

#directory = "Akker feature sets"
directory = "Jensen feature sets"

require(caret)
require(ranger)
require(doMC)
require(data.table)
registerDoMC(cores = 3)

# Create the TrainControl 
trc = trainControl(method = "repeatedcv", number = 10, repeats = 10, savePredictions = T, 
                   allowParallel = T, summaryFunction = twoClassSummary, classProbs = TRUE,)

feature_sets = list.files(directory, pattern = "Feature table for ")
removalColumns = c("ICPC1", "ICPC2", "CUI_A", "DiseaseA", "dpsA", "CUI_B", "DiseaseB", "dpsB", "goldstandard")

for(j in 1:length(feature_sets)){

f = as.data.frame(fread(paste0(directory, feature_sets[j])))
gs = factor(f$goldstandard, levels = c("VALID", "INVALID"))
f = f[, -which(colnames(f) %in% removalColumns)]
model = train(y = gs, x = f, method = "ranger", metric = "ROC", 
            tuneGrid = data.frame(mtry = round(sqrt(ncol(f))),
                                  min.node.size = 1,
                                  splitrule = "gini"),
            save.memory = T,
            trControl = trc)
print(model)

assign(paste0("m", j), model)
}