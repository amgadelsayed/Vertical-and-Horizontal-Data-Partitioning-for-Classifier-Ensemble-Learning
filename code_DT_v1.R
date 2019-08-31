# ------------------------------------------------------------------------------------------
# TODOS: 
#    - CHECK IT IS WORKING FOR ALL THE DATASETS 
#            - Fix dataset (or remove it, we have a lot)
#    - INCLUDE OTHER VOTING SCHEMES
#    - MAKE COPIES OF THIS AND USE OTHER SINGLE MODELS
# ------------------------------------------------------------------------------------------



rm(list = ls());cat("\014");graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# ------------------------------------------------------------------------------------------
# Librar??es and implemented functions
# ------------------------------------------------------------------------------------------
library(clusterSim)
library(partykit)
library(caret)

source("functions.R")
rm(list = setdiff(ls(), lsf.str()))

# ------------------------------------------------------------------------------------------
# Parameters of the experimentation
#       - Number of repetitions of cross-validation, 
#       - Folder where i can find the datasets 
#       - File to save resulting table
# ------------------------------------------------------------------------------------------
times.cross.fold =10
file.report = "report.csv"
data.folder = "dat"

# ------------------------------------------------------------------------------------------
# Parameters of the ensembling strategy: 
#       - Percentage internal split among train-valid partitions
#       - Number of models to include in the ensemble
#       - Percentage of (random) attributes used to build each one of the models
# ------------------------------------------------------------------------------------------
ptras = c(0.5)
nmodelss = c(3,10,20,30,40,50)
pattrs = c(0.5)

# ------------------------------------------------------------------------------------------
# Read of the content of the data-folder and generation of the table with experiments
# to run
# CAREFUL! ONLY DATASETS STARTING WITH <B> LISTED
# ------------------------------------------------------------------------------------------
files = dir(path = data.folder,pattern="*.csv")
experiments = expand.grid(file = files, ptra = ptras, nmodels = nmodelss, pattr = pattrs)
report = data.frame()

# ------------------------------------------------------------------------------------------
# LOOP for each one of the experiments       (this could be done in parallel)
# ------------------------------------------------------------------------------------------
for (experiment in 1:nrow(experiments)){
  
    # ------------------------------------------------------------------------------------------
    # Setup of the experimentation in this iteration and print of it. 
    # ------------------------------------------------------------------------------------------
    file    = experiments$file[experiment]
    ptra    = experiments$ptra[experiment]
    nmodels = experiments$nmodels[experiment]
    pattr   = experiments$pattr[experiment]
    print.configuration(file, ptra, nmodels, pattr)
    
    
    # ------------------------------------------------------------------------------------------
    # Read file, preparation (normalization + deletion of constant columns + deletion of factors 
    # with more than 30 levels) and creation of (train-test) partitions
    # ------------------------------------------------------------------------------------------
    data = read.csv(paste0(data.folder,"/",file))
    data = prepare.data(data)
    print("------------summary of the dataset---------------", quote = F)
    print(summary(data))
    index = createMultiFolds(data$class, k = 5, times = times.cross.fold)
    
    
    # ------------------------------------------------------------------------------------------
    # LOOP over each one of the partitions of the dataset
    # ------------------------------------------------------------------------------------------
    for (fold in 1:length(index)){
      
        # ------------------------------------------------------------------------------------------
        # Obtaining Train+Valid+Test sets
        # ------------------------------------------------------------------------------------------
        train = data[ index[[fold]],]
        test  = data[-index[[fold]],]
        
        index2 = createDataPartition(train$class, p = ptra)
        valid  = train[-index2[[1]],]
        train  = train[ index2[[1]],]  
        
        
        # ------------------------------------------------------------------------------------------
        # Initialization of list of models, accuracy table, and tables for individual predictions
        # ------------------------------------------------------------------------------------------
        models = list()
        accuracies = data.frame()
        experts = data.frame(id = row.names(valid))
        experts.test = data.frame(id = row.names(test))
        
        # ------------------------------------------------------------------------------------------
        # LOOP for building each one of the models in the ensemble
        # ------------------------------------------------------------------------------------------
        for (i in 1:nmodels){
          
            # ------------------------------------------------------------------------------------------
            # Selection of the columns (pattr% random) and rows (random resampling with replacement)
            # Rows outside the sampling for training the individual model are also keept
            # ------------------------------------------------------------------------------------------
            columns = c(sample(1:(ncol(train)-1),(ncol(train)-1)*pattr), colclass)
            rows = sample(1:nrow(train), nrow(train), replace = TRUE)
            rows.out = setdiff(1:nrow(train), rows)
            
            
            # ------------------------------------------------------------------------------------------
            # Construction of the model with training data (with default parameters)
            # ------------------------------------------------------------------------------------------  
            model = ctree(class~., data = train[rows, columns])

            
            # ------------------------------------------------------------------------------------------
            # Generation of predictions:
            #  - over the rows not used for training (to calculate out-of-bag error)
            #  - over the validation dataset (for validating the ensemble afterwards)
            #  - over the test set (for later calculation of test error of individual models)
            # ------------------------------------------------------------------------------------------
            predictions.out = predict(model, train[rows.out, columns]) 
            predictions.valid = predict(model,  valid[,columns]) 
            predictions.test = predict(model,  test[,columns])
            
            # ------------------------------------------------------------------------------------------
            # Calculation of accuracies
            #  - over the out-of-bag examples (for weighting models in voting schemes)
            #  - over the test set (for compare individual models against the ensemble)
            # ------------------------------------------------------------------------------------------
            accuracy.out = confusionMatrix(predictions.out, train$class[rows.out])$overall[1]
            accuracy.valid=confusionMatrix(predictions.valid,valid$class)$overall[1]
            accuracy.test  = confusionMatrix(predictions.test, test$class)$overall[1] 
            
            # ------------------------------------------------------------------------------------------
            # Construction of the experts matrix (1 row per data, 1 column per individual model) containing
            # predictions of the individual models
            #  - experts: matrix containing predictions of individual models over the validation dataset
            #             (in order to test voting shemas later)
            #  - experts.test: matrix containing predictions of individual models over the test dataset
            #                  (in order to test voting schemas over test dataset)
            # ------------------------------------------------------------------------------------------
            experts = cbind(experts, predictions.valid) 
            colnames(experts)[ncol(experts)] = paste0("e",i)
            experts.test = cbind(experts.test, predictions.test) 
            colnames(experts.test)[ncol(experts.test)] = paste0("e",i)
            
            
            # ------------------------------------------------------------------------------------------
            # Calculation of a table containing accuracies of each one of the single models and
            # storage of the model on a list
            # ------------------------------------------------------------------------------------------
            accuracies = rbind(accuracies, data.frame(valid=accuracy.valid,out = accuracy.out,test = accuracy.test))
            models[[i]] = model
            
        } # END of LOOP for building ensemble's models
        
        # ------------------------------------------------------------------------------------------
        # Removal of unuseful column in the experts matrices
        # ------------------------------------------------------------------------------------------
        experts$id = NULL
        experts.test$id = NULL
        
        
        # ------------------------------------------------------------------------------------------
        # Calculation of accuracies of single models (test-set)
        # ------------------------------------------------------------------------------------------
        accuracies.single.models = sapply(experts.test, function (x) confusionMatrix(x,test$class)$overall[1])
        weights=log(accuracies$valid/(1- accuracies$valid))
        # ------------------------------------------------------------------------------------------
        # Calculation of accuracy of a model using all the attributes and valid+train rows 
        # (named complete.model)
        # ------------------------------------------------------------------------------------------
        model = ctree(class~., data = rbind(valid,train))
        predictions = predict(model, test)
        accuracy.complete.model = confusionMatrix(predictions, test$class)$overall[1]
        
        # ------------------------------------------------------------------------------------------
        # SIMPLE ENSEMBLING
        # Calculation of accuracies (over validation and test) of an ensemble of all the individual
        # models using simple votation schema
        # ------------------------------------------------------------------------------------------
        predictions.may = factor(apply(experts,1,calc_majority, increment))
        predictions.may.test = factor(apply(experts.test,1,calc_majority, increment))
        accuracy.may = confusionMatrix(predictions.may, valid$class)$overall[1]
        accuracy.may.test = confusionMatrix(predictions.may.test, test$class)$overall[1]
        
        
        
        predictions.weighted=factor(apply(experts,1,calc_weighted,weights, increment))
        predictions.weighted.test = factor(apply(experts.test,1,calc_weighted,weights, increment))
        accuracy.weighted=confusionMatrix(predictions.weighted, valid$class)$overall[1]
        accuracy.weighted.test=confusionMatrix(predictions.weighted.test, test$class)$overall[1]
        
        
        # ------------------------------------------------------------------------------------------
        # REPORTING AND UPDATE OF THE REPORT TABLE:
        #   - file, fold, <values of the attributes>,
        #   - acc_complete: accuracy of the complete model (test)
        #   - acc_best_ind: accuracy of the best of the single individuals of the ensemble (test)
        #   - acc_ENS_Maj: Accuracy of an ensemble containing all the individual models with simple 
        #                   majority voting (test)
        # ------------------------------------------------------------------------------------------
        print("----------------------------------------", quote = F)
        print(paste0("File: ",file,"  - Fold  #",fold," (out of ",length(index),")"), quote = F)
        print(paste0("Accuracy of complete model                : ",accuracy.complete.model), quote = F)
        print(paste0("Accuracy of best single individual (test) : ",max(accuracies.single.models)), quote = F)
        print(paste0("Accuracy of ensemble (Majority) (test)    : ",accuracy.may.test), quote = F)
        
        report.line = data.frame(file = file, fold = fold, 
                                 ptra = ptra, pattr = pattr, nmodels = nmodels,
                                 acc_complete = accuracy.complete.model, acc_best_ind = max(accuracies.single.models),
                                 acc_ENS_maj = accuracy.may.test,acc_ENS_weighted=accuracy.weighted.test)
        report = rbind(report,report.line)
        write.csv(report, file.report, row.names = F)

    } # END of the LOOP for each one of the folds of the experiment
    
    
} # END of the LOOP over the list of experiments



# ------------------------------------------------------------------------------------------
# Calculation of boolean tests over the report:
#   - ENS_Maj_ge_Com: is the ensemble of all of them with majority voting scheme better or 
#                     equal than the complete model?
#   - ENS_Maj_ge_best_ind: is the ensemble of all of them with majority voting scheme better or
#                          equal than the best of its single components 
# ------------------------------------------------------------------------------------------


# ind=which(report$acc_ENS_maj < report$acc_ENS_weighted)
# val=report$acc_ENS_maj
# val[ind]=report$acc_ENS_weighted[ind]
# 
# report$ENS_Maj_ge_Com = val>=report$acc_complete
# report$ENS_Maj_ge_best_ind = val>=report$acc_best_ind

report$ENS_Maj_ge_Com = report$acc_ENS_maj>=report$acc_complete
report$ENS_Weighted_ge_com = report$acc_ENS_weighted>=report$acc_complete


write.csv(report, file.report, row.names = F)


# ------------------------------------------------------------------------------------------
# Note: Play with randomized parameters... mtry



