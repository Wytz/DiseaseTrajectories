# Set the seeds which are used to randomize in Caret, to enable a paired T-test between the AUCs

#directory = "Akker feature sets"
directory = "Jensen feature sets"

require(caret)
require(ranger)
require(doMC)
require(data.table)
registerDoMC(cores = 3)

# Select the seeds
SampledSeeds = sample(c(1:10000), size = 101)

# Create the TrainControl 
trc = trainControl(method = "repeatedcv", number = 10, repeats = 10, savePredictions = T, 
                   allowParallel = T, summaryFunction = twoClassSummary, classProbs = TRUE,
                   seeds = SampledSeeds)
				   
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

print("Between Split directed and Split Undirected")
t.test(m4$resample$ROC, m6$resample$ROC, paired = T, conf.level = 0.95)
print("Between Split Mixed and Split Undirected")
t.test(m5$resample$ROC, m6$resample$ROC, paired = T, conf.level = 0.95)
print("Between Split Mixed and Split Directed")
t.test(m4$resample$ROC, m5$resample$ROC, paired = T, conf.level = 0.95)

print("Between Directed metapaths and Undirected metapaths")
t.test(m1$resample$ROC, m3$resample$ROC, paired = T, conf.level = 0.95)
print("Between Mixed metapaths and Undirected metapaths")
t.test(m2$resample$ROC, m3$resample$ROC, paired = T, conf.level = 0.95)
print("Between Directed metapaths and Mixed metapaths")
t.test(m2$resample$ROC, m1$resample$ROC, paired = T, conf.level = 0.95)