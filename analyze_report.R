report=read.csv("report.csv")

# =========run this section of code then display the dataframe(f) to show results

library(tidyverse)
f=report %>% 
  group_by(file, ptra, pattr, nmodels) %>%
  summarise(acc_complete = mean(acc_complete),
            acc_best_ind = mean(acc_best_ind),
            acc_ENS_maj = mean(acc_ENS_maj),
            acc_ENS_weighted=mean(acc_ENS_weighted))



# another analysis related to each dataset, try to display dataframe (k) to see the results

k=report %>% 
  group_by(file) %>%
  summarise(acc_complete = mean(acc_complete),
            acc_best_ind = mean(acc_best_ind),
            acc_ENS_maj = mean(acc_ENS_maj),
            acc_ENS_weighted=mean(acc_ENS_weighted))
            # ENS_Maj_ge_Com = mean(ENS_Maj_ge_Com),
            # ENS_Weighted_ge_com = mean(ENS_Weighted_ge_com))


#this part to analyze the number of ensemble size
l=report %>% 
  group_by(nmodels) %>%
  summarise(acc_complete = mean(acc_complete),
            acc_best_ind = mean(acc_best_ind),
            acc_ENS_maj = mean(acc_ENS_maj),
            acc_ENS_weighted=mean(acc_ENS_weighted))

