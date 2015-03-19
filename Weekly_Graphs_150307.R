##
#setwd("C:/Users/Walter/Documents/GitHub/SciCast_Andy/SciCast_Andy")
#source("Weekly_Graphs_150307.R")


source("Get_Data_150203lb.R")
source("Data_Cleaning_150106.R")
source("Analysis_Setup_150107.R")
source("Data_Selection_150110.R")
source("De-Stuttering.R")
source("Comments_per_Day_graph_141229.R")     #contains code for graph y axes lables
source("Trades_per_Day_graph_141229.R")
source("Extra_Smoothing.R")
source("Trades_per_Question_per_Day_graph_150103.R")
source("Conditional_Trades_per_Day_graph_141229.R")
source("Activities_per_Person_graph_150103A.R")
source("Trades_per_Person_per_Day_graph_141229.R")
source("User_retention_graph_150103_JAN15.R")
source("Weighted_Forecasts_150110.R")
source("ULinOP_150128.R")
source("Accuracy and Uniform 150304B  Reset.R")
source("Brier Score vs Choices graph 150304A.R")
#source("Accuracy_and_2_Benchmarks_with_Graphs_150110.R")
#source("Accuracy_and_2_Benchmarks_with_Graphs_mod_grouped_bar_graph.R")
source("4_Forms_of_Accuracy_per_TradeA.R")
rsq <- levels(factor(qiq[raq<=Sys.time()&caq>tstart]))     # vector of unique values question_Ids wiht resolution_time earlier than now and Created_Date after tstart
