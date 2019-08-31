library(clusterSim)
library(partykit)
library(caret)

#======majority ensemble============================
calc_majority=function(experts,increment){
  result=rowsum(sample(1,length(experts),T),experts)
  S=sort(unique(experts))
  DD=result[S[result==max(result)],]+increment[S[result==max(result)]]
  return (names(which.max(DD)))
}


#=====weighted ensemble ==================================
calc_weighted=function(experts,weigh,increment){
  
  result=rowsum(weigh,experts)
  S=sort(unique(experts))
  DD=result[S[result==max(result)],]+increment[S[result==max(result)]]
  return (names(which.max(DD)))
}





print.configuration = function(file, ptra, nmodels, pattr){
  print("----------------------------------------", quote = F)
  print(paste0("Dataset                             :   ",file), quote = F)
  print(paste0("Split of training set on Train/Valid:   ",ptra), quote = F)
  print(paste0("Number of models created:               ",nmodels), quote = F)
  print(paste0("Percentage of attributes in each model: ",pattr), quote = F)
}

prepare.data = function(data){
  colclass = ncol(data)      #col class must be the last column
  numeric = sapply(data, is.numeric)
  colnames(data)[colclass] = "class"
  if (numeric[colclass]){
    data$class = as.factor(data$class)
    numeric = sapply(data, is.numeric)
  }
  if (sum(numeric)>0){
    data[,numeric] = data.Normalization(data[,numeric], type = "n4")
  }
  
  valid = sapply(data, function(x) sum(is.nan(x))==0)    # what if one column contains NAN, the column will be removed
  if (sum(!valid)>0){
    data = data[,valid]
    colclass = ncol(data)
  }
  # Remove factors with more than 30 levels  >>>> why!!!!!! and what if the colclass has 32 factors
  correct.levels = sapply(data, function(x) is.factor(x)*length(unique(x)))<31
  data = data[,correct.levels]
  
  # Assign global variables
  colclass  <<- ncol(data)
  nclasses  <<- length(unique(data$class))
  increment <<- table(data$class)/nrow(data)
  return(data)
}
prepare.data(read.csv("dat/car.csv"))


posteriorToClass <- function(predicted) {
  colnames(predicted$posterior)[apply(predicted$posterior, 
                                      MARGIN=1, FUN=function(x) which.max(x))]
}