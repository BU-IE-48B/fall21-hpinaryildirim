setwd('C:/Users/asus_pinar/desktop/files')
getwd()


require(usedist)
require(ggplot2)
require(data.table)
require(caret)
require(TSrepr)
require(TSdist)
require(tidyverse)
require(dtw)
require(TunePareto)
require(genlasso)

theme_set(theme_bw())
options(scipen=999)
options(repr.plot.width=15, repr.plot.height=8)

large_number = 999999

#5 time series classification datasets are chosen from www.timeseriesclassification.com
######################################
#ECG200_TEST
#ECG200_TRAIN

ECG_train= fread("ECG200_TRAIN.txt")
ECG_test= fread("ECG200_TEST.txt")

print("ECG Train")
str(ECG_train)

print("ECG Test")
str(ECG_test)

#Plane_TEST
#Plane_TRAIN

Plane_train= fread("Plane_TEST.txt")
Plane_test= fread("Plane_TEST.txt")

print("Plane Train")
str(Plane_train)

print("Plane Test")
str(Plane_test)


#Trace_TEST
#Trace_TRAIN

Trace_train= fread("Trace_TRAIN.txt")
Trace_test= fread("Trace_TEST.txt")

print("Trace Train")
str(Trace_train)

print("Trace Test")
str(Trace_test)

#Fish_TEST
#Fish_TRAIN

Fish_train= fread("Fish_TRAIN.txt")
Fish_test= fread("Fish_TEST.txt")

print("Fish Train")
str(Fish_train)

#PowerCons_TEST
#PowerCons_TRAIN

PowerCons_train= fread("PowerCons_TRAIN.txt")
PowerCons_test= fread("PowerCons_TEST.txt")

print("PowerCons Train")
str(PowerCons_train)

print("PowerCons Test")
str(PowerCons_test)

###############################################################


#RAW SERIES 

#euclidean distance
#ECG_train_euc_dist = as.matrix(dist(ECG_train[,-1], upper=TRUE))
#diag(ECG_train_euc_dist)=large_number

#DTW distance
#ECG_train_dtw_dist=as.matrix(dtwDist(ECG_train[,-1]))
#diag(ECG_train_dtw_dist)=large_number

#LCSS distance
#ECG_train_lcss_dist=TSDatabaseDistances(ECG_train[,-1],distance='lcss',epsilon=0.1)
#ECG_train_lcss_dist=as.matrix(ECG_train_lcss_dist)
#diag(ECG_train_lcss_dist)=large_number

#EDR distance
#ECG_train_edr_dist = TSDatabaseDistances(ECG_train[,-1],distance='edr',epsilon=0.1)
#ECG_train_edr_dist=as.matrix(ECG_train_edr_dist)
#diag(ECG_train_edr_dist)=large_number 
 
get_distances = function(dataset){ #remove class column
  large_number = 99999
  
  #euclidean distance
  euc_dist = as.matrix(dist(dataset, upper=TRUE))
  diag(euc_dist)=large_number
  
  #DTW distance
  dtw_dist=as.matrix(dtwDist(dataset))
  diag(dtw_dist)=large_number
  
  #LCSS distance
  lcss_dist=TSDatabaseDistances(dataset,distance='lcss',epsilon=0.1)
  lcss_dist=as.matrix(lcss_dist)
  diag(lcss_dist)=large_number
  
  #EDR distance
  edr_dist = TSDatabaseDistances(dataset,distance='edr',epsilon=0.1)
  edr_dist=as.matrix(edr_dist)
  diag(edr_dist)=large_number
  
  return(list(euc_dist, dtw_dist, lcss_dist, edr_dist))
}


nn_classify_cv=function(dist_matrix,train_class,test_indices,k){
  
  test_distances_to_train=as.matrix(dist_matrix[test_indices,])
  test_distances_to_train=test_distances_to_train[,-test_indices]
  trainclass=train_class[-test_indices]
  #print(str(test_distances_to_train))
  ordered_indices=apply(test_distances_to_train,1,order)
  if(k==1){
    nearest_class=(trainclass[as.numeric(ordered_indices[1,])])
    nearest_class=data.table(id=test_indices,nearest_class)
  } else {
    nearest_class=apply(ordered_indices[1:k,],2,function(x) {trainclass[x]})
    nearest_class=as.data.table(nearest_class)
    nearest_class=data.table(id=test_indices,t(nearest_class))
  }
  
  long_nn_class=melt(nearest_class,'id')
  
  class_counts=long_nn_class[,.N,list(id,value)]
  class_counts[,predicted_prob:=N/k]
  wide_class_prob_predictions=dcast(class_counts,id~value,value.var='predicted_prob')
  wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0
  class_predictions=class_counts[,list(predicted=value[which.max(N)]),by=list(id)]
  
  
  return(list(prediction=class_predictions,prob_estimates=wide_class_prob_predictions))
  
}

test_classification=function(dist_matrix,train_class,test_indices,k){
  
  test_distances_to_train=as.matrix(dist_matrix[test_indices,])
  test_distances_to_train=test_distances_to_train[,-test_indices]
  trainclass=train_class
  #trainclass=train_class[-test_indices]
  #print(str(test_distances_to_train))
  ordered_indices=apply(test_distances_to_train,1,order)
  if(k==1){
    nearest_class=(trainclass[as.numeric(ordered_indices[1,])])
    nearest_class=data.table(id=test_indices,nearest_class)
  } else {
    nearest_class=apply(ordered_indices[1:k,],2,function(x) {trainclass[x]})
    nearest_class=as.data.table(nearest_class)
    nearest_class=data.table(id=test_indices,t(nearest_class))
  }
  
  long_nn_class=melt(nearest_class,'id')
  
  class_counts=long_nn_class[,.N,list(id,value)]
  class_counts[,predicted_prob:=N/k]
  wide_class_prob_predictions=dcast(class_counts,id~value,value.var='predicted_prob')
  wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0
  class_predictions=class_counts[,list(predicted=value[which.max(N)]),by=list(id)]
  
  
  return(list(prediction=class_predictions,prob_estimates=wide_class_prob_predictions))
  
}


eval_distances_for_rep=function(traindata, datarepr){
  
  trainclass = traindata$V1
  nof_rep=5
  n_fold=10
  set.seed(123)
  cv_indices=generateCVRuns(trainclass, 
                          ntimes =nof_rep, 
                          nfold = n_fold,
                          leaveOneOut = FALSE, 
                          stratified = TRUE)


  k_levels=c(1,3,5)
#approach_file=list.files(dist_folder)

  dist_list = get_distances(datarepr)
  result=vector('list',length(dist_list)*nof_rep*n_fold*length(k_levels))

  iter=1


  for(m in 1:length(dist_list)){ 
  
    if(m==1)
      {apprch = "euclidean distance"
      print("euclidean distance")}
    if(m==2)
      {apprch = "dtw distance"
      print("dtw distance")}
    if(m==3)
    {apprch = "lcss distance"
    print("lcss distance")}
    if(m==4)
      {apprch = "edr distance"
     print("edr distance")}
  
    #print(dist_files[m])
  
    dist_mat=as.matrix(dist_list[[m]])
  
    for(i in 1:nof_rep){
      this_fold=cv_indices[[i]]
      for(j in 1:n_fold){
       test_indices=this_fold[[j]]
        for(k in 1:length(k_levels)){
          current_k=k_levels[k]
          current_fold=nn_classify_cv(dist_mat,trainclass,test_indices,k=current_k)
          accuracy=sum(trainclass[test_indices]==current_fold$prediction$predicted)/length(test_indices)
          tmp=data.table(approach=apprch,repid=i,foldid=j,
                       k=current_k,acc=accuracy)
          result[[iter]]=tmp
          iter=iter+1
        
        }
     }
    }
  }
  
  return(result)
}


# FUSEDLASSO 1D

get_FL1D=function(dataset, lam){
  inputs = array(dim=c(nrow(dataset), ncol(dataset)-1))
  outputs=list()
  cross_val = list()
  
  for (i in 1:nrow(dataset)){
    dummy = as.matrix(dataset[i,-1])
    colnames(dummy) = NULL
    inputs[i,] =array(dummy)
  }
  
  for (i in 1:nrow(inputs)){
    
    outputs[[i]] = fusedlasso1d(inputs[i,])
    cross_val[[i]] = cv.trendfilter(outputs[[i]], k=10)
  }
  
  FL_rep = data.frame(time = c(1:length(dataset[,-1])))
  for(i in 1:nrow(dataset)){
    FL_rep = cbind(FL_rep, coef(outputs[[i]], lambda=lam)$beta[,1])
  }
  
  colnames(FL_rep) = 0:nrow(ECG_train)
  FL_rep$`0`=NULL
  
  FL_rep=t(FL_rep)
  
  return(FL_rep)
  
}

# Piecewise Aggregate Approximation

get_paa=function(dataset,segment_length, method){
  
  train_dummy = data.table(dataset)
  setnames(train_dummy,'V1','class')
  train_dummy = train_dummy[order(class)]
  train_dummy[,class:=as.character(class)]
  train_dummy[,id:=1:.N]
  
  
  long_train=melt(train_dummy,id.vars=c('id','class'))
  # need to get numerical part of the variable (represents time)
  # using gsub to set the nonnumerical part to zero length
  long_train[,time:=as.numeric(gsub("\\D", "", variable))-1]
  long_train=long_train[,list(id,class,time,value)]
  long_train=long_train[order(id,time)]
  
  dt_paa = data.table()
  for (i in 1:nrow(dataset)){
    
    selected_series=i
    data_ts=long_train[id==selected_series]$value
    paa_rep=repr_paa(data_ts, segment_length, method)
    #plot(paa_rep,type='l')
    
    dt_paa=rbind(dt_paa,t(paa_rep))
  }
  
  return(dt_paa)
}

complete_paramater_evalutaion=function(dataset){
  
  print("Raw Data")
  raw_data_evalutaion=eval_distances_for_rep(traindata = dataset, datarepr = dataset[,-1])
  raw_data_results = rbindlist(raw_data_evalutaion)
  raw_data_summary = raw_data_results[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(approach,k)]
  raw_data_results[,representation:="Raw"]

  print(ggplot(raw_data_results,aes(x=paste0(representation,'+',approach,'+',k), y=acc)) +
    geom_boxplot()+
     coord_flip()+labs(title="Raw Results", x="Accuracy", y="Approach"))

  #FL lambda=5
  print("FusedLasso lambda=5")
  FL5_repr=get_FL1D(dataset, 5)

  FL5_evalutaion=eval_distances_for_rep(traindata = dataset, datarepr = FL5_repr)

  FL5_results = rbindlist(FL5_evalutaion)
  FL5_summary = FL5_results[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(approach,k)]
  FL5_results[,representation:="FusedLasso5"]

  (ggplot(FL5_results,aes(x=paste0(representation,'+',approach,'+',k), y=acc)) +
    geom_boxplot()+
   coord_flip()+labs(title="FusedLasso lamda=5 Results", x="Accuracy", y="Approach"))

  #FL lambda=2
  print("FusedLasso lambda=2")
  FL2_repr=get_FL1D(dataset, 2)

  FL2_evalutaion=eval_distances_for_rep(traindata = dataset, datarepr = FL2_repr)

  FL2_results = rbindlist(FL2_evalutaion)
  FL2_summary = FL2_results[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),by=list(approach,k)]
  FL2_results[,representation:="FusedLasso2"]

  print(ggplot(FL2_results,aes(x=paste0(representation,'+',approach,'+',k), y=acc)) +
    geom_boxplot()+
   coord_flip()+labs(title="FusedLasso lamda=2 Results", x="Accuracy", y="Approach"))


  #PAA segment length = 5, method=meanC
  
  print("PAA segment length = 5, method=meanC")
  PAA5_mean=get_paa(dataset, 5, meanC)

  PAA5_mean_evalutaion=eval_distances_for_rep(traindata = dataset, datarepr = PAA5_mean)

  PAA5_mean_results = rbindlist(PAA5_mean_evalutaion)
  PAA5_mean_summary = PAA5_mean_results[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(approach,k)]
  PAA5_mean_results[,representation:="PAA5_mean"]

  print(ggplot(PAA5_mean_results,aes(x=paste0(representation,'+',approach,'+',k), y=acc)) +
   geom_boxplot()+
   coord_flip()+labs(title="PAA SL=5 method=mean", x="Accuracy", y="Approach"))

  
  #PAA segment length = 5, method=medianC
  
  print("PAA segment length = 5, method=medianC")
  PAA5_median=get_paa(dataset, 5, medianC)

  PAA5_median_evalutaion=eval_distances_for_rep(traindata = dataset, datarepr = PAA5_median)

  PAA5_median_results = rbindlist(PAA5_median_evalutaion)
  PAA5_median_summary = PAA5_median_results[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(approach,k)]
  PAA5_median_results[,representation:="PAA5_median"]

  print(ggplot(PAA5_median_results,aes(x=paste0(representation,'+',approach,'+',k), y=acc)) +
    geom_boxplot()+
   coord_flip()+labs(title="PAA SL=5 method=median", x="Accuracy", y="Approach"))


  #PAA segment length = 2, method=meanC
  
  print("PAA segment length = 2, method=meanC")
  PAA2_mean=get_paa(dataset, 2, meanC)

  PAA2_mean_evalutaion=eval_distances_for_rep(traindata = dataset, datarepr = PAA2_mean)

  PAA2_mean_results = rbindlist(PAA2_mean_evalutaion)
  PAA2_mean_summary = PAA2_mean_results[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(approach,k)]
  PAA2_mean_results[,representation:="PAA2_mean"]

  print(ggplot(PAA2_mean_results,aes(x=paste0(representation,'+',approach,'+',k), y=acc)) +
   geom_boxplot()+
   coord_flip()+labs(title="PAA SL=2 method=mean", x="Accuracy", y="Approach"))


  #PAA segment length = 2, method=medianC
  print("PAA segment length = 2, method=medianC")
  PAA2_median=get_paa(dataset, 2, medianC)

  PAA2_median_evalutaion=eval_distances_for_rep(traindata = dataset, datarepr = PAA2_median)

  PAA2_median_results = rbindlist(PAA2_median_evalutaion)
  PAA2_median_summary = PAA2_median_results[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(approach,k)]
  PAA2_median_results[,representation:="PAA2_median"]

    print(ggplot(PAA2_median_results,aes(x=paste0(representation,'+',approach,'+',k), y=acc)) +
    geom_boxplot()+
   coord_flip()+labs(title="PAA SL=2 method=median", x="Accuracy", y="Approach"))


  complete_results = rbind(raw_data_results, 
                         FL5_results, FL2_results, 
                         PAA5_mean_results,PAA5_median_results,
                         PAA2_mean_results, PAA2_median_results)

  ggplot(complete_results,aes(x=paste0(representation,'+',approach,'+',k), y=acc)) +
   geom_boxplot()+
   coord_flip()+labs(title="Complete Result Plot of ECG", y="Accuracy", x="Approach")


  complete_summary = complete_results[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(representation,approach,k)]
  print("Complete Results Summary")
  print(complete_summary)
  
  #order by avg accuracy
  complete_summary=complete_summary[order(complete_summary$avg_acc, decreasing = TRUE)]
  print("Complete Results Summary Ordered")
  print(complete_summary)

  return(list(complete_results, complete_summary))
}


## ECG

ECG_train = as.data.table(ECG_train)
ECG_results = complete_paramater_evalutaion(ECG_train)

ECG_complete=ECG_results[[1]]
ECG_summary=ECG_results[[2]]

print(ECG_summary)

ECG_best_representation= ECG_summary[1,]$representation
print("ECG best representation")
print(ECG_best_representation)

ECG_best_dist = ECG_summary[1,]$approach
print("ECG best dist")
print(ECG_best_dist)

ECG_best_k = ECG_summary[1,]$k
print("ECG best k")
print(ECG_best_k)

ECG_best_acc = ECG_summary[1,]$avg_acc
print("ECG best CV average accuracy")
print(ECG_best_acc)

#accuracy on test data

ECG_complete = rbind(ECG_test, ECG_train)
ECG_test_indices = 1:nrow(ECG_test)
ECG_train_indices = (nrow(ECG_test)+1):(nrow(ECG_test)+nrow(ECG_train))
ECG_trainclass = ECG_train$V1
ECG_testclass = ECG_test$V1

ECG_complete_euc = as.matrix(dist(ECG_complete[,-1], upper=TRUE))
diag(ECG_complete_euc)=large_number


ECG_test_classification=test_classification(dist_matrix =ECG_complete_euc,
                                            train_class = ECG_trainclass,
                                            test_indices = ECG_test_indices,
                                            k=ECG_best_k )

ECG_pred_test = ECG_test_classification$prediction$predicted

ECG_test_accuracy=sum(ECG_testclass==ECG_pred_test)/length(ECG_testclass)
print("ECG test accuracy")
print(ECG_test_accuracy)

ECG_df = data.table(dataset="ECG", 
                    best_repr=ECG_best_representation, 
                    best_dist=ECG_best_dist, 
                    best_k=ECG_best_k, 
                    cv_avg_acc = ECG_best_acc,
                    test_acc = ECG_test_accuracy)

print(ECG_df)


## Plane

Plane_train = as.data.table(Plane_train)
Plane_results = complete_paramater_evalutaion(Plane_train)

Plane_complete=Plane_results[[1]]
Plane_summary=Plane_results[[2]]

print(Plane_summary)

Plane_best_representation= Plane_summary[1,]$representation
print("Plane best representation")
print(Plane_best_representation)

Plane_best_dist = Plane_summary[1,]$approach
print("Plane best dist")
print(Plane_best_dist)

Plane_best_k = Plane_summary[1,]$k
print("Plane best k")
print(Plane_best_k)

Plane_best_acc = Plane_summary[1,]$avg_acc
print("Plane best CV average accuracy")
print(Plane_best_acc)

#accuracy on test data

Plane_complete = rbind(Plane_test, Plane_train)
Plane_test_indices = 1:nrow(Plane_test)
Plane_train_indices = (nrow(Plane_test)+1):(nrow(Plane_test)+nrow(Plane_train))
Plane_trainclass = Plane_train$V1
Plane_testclass = Plane_test$V1

###representation = raw

###distance  dtw

Plane_complete_dtw = as.matrix(dtwDist(Plane_complete[,-1], upper=TRUE))
diag(Plane_complete_dtw)=large_number

###k = 1
Plane_test_classification=test_classification(dist_matrix = Plane_complete_dtw,
                                            train_class = Plane_trainclass,
                                            test_indices = Plane_test_indices,
                                            k=Plane_best_k )

Plane_pred_test = Plane_test_classification$prediction$predicted

Plane_test_accuracy=sum(Plane_testclass==Plane_pred_test)/length(Plane_testclass)
print("Plane test accuracy")
print(Plane_test_accuracy)

Plane_df = data.table(dataset="Plane", 
                    best_repr=Plane_best_representation, 
                    best_dist=Plane_best_dist, 
                    best_k=Plane_best_k, 
                    cv_avg_acc = Plane_best_acc,
                    test_acc = Plane_test_accuracy)

print(Plane_df)


## Trace

Trace_train = as.data.table(Trace_train)
Trace_results = complete_paramater_evalutaion(Trace_train)

Trace_complete=Trace_results[[1]]
Trace_summary=Trace_results[[2]]

print(Trace_summary)


Trace_best_representation= Trace_summary[1,]$representation
print("Trace best representation")
print(Trace_best_representation)

Trace_best_dist = Trace_summary[1,]$approach
print("Trace best dist")
print(Trace_best_dist)

Trace_best_k = Trace_summary[1,]$k
print("Trace best k")
print(Trace_best_k)

Trace_best_acc = Trace_summary[1,]$avg_acc
print("Trace best CV average accuracy")
print(Trace_best_acc)

#accuracy on test data

Trace_complete = rbind(Trace_test, Trace_train)
Trace_test_indices = 1:nrow(Trace_test)
Trace_train_indices = (nrow(Trace_test)+1):(nrow(Trace_test)+nrow(Trace_train))
Trace_trainclass = Trace_train$V1
Trace_testclass = Trace_test$V1

###representation raw

###distance 

Trace_complete_dtw = as.matrix(dtwDist(Trace_complete[,-1], upper=TRUE))
diag(Trace_complete_dtw)=large_number

###k
Trace_test_classification=test_classification(dist_matrix = Trace_complete_dtw,
                                              train_class = Trace_trainclass,
                                              test_indices = Trace_test_indices,
                                              k=Trace_best_k )

Trace_pred_test = Trace_test_classification$prediction$predicted

Trace_test_accuracy=sum(Trace_testclass==Trace_pred_test)/length(Trace_testclass)
print("Trace test accuracy")
print(Trace_test_accuracy)

Trace_df = data.table(dataset="Trace", 
                      best_repr=Trace_best_representation, 
                      best_dist=Trace_best_dist, 
                      best_k=Trace_best_k, 
                      cv_avg_acc = Trace_best_acc,
                      test_acc = Trace_test_accuracy)

print(Trace_df)



## Fish

Fish_train = as.data.table(Fish_train)
Fish_results = complete_paramater_evalutaion(Fish_train)

Fish_complete=Fish_results[[1]]
Fish_summary=Fish_results[[2]]

print(Fish_summary)

Fish_best_representation= Fish_summary[1,]$representation
print("Fish best representation")
print(Fish_best_representation)

Fish_best_dist = Fish_summary[1,]$approach
print("Fish best dist")
print(Fish_best_dist)

Fish_best_k = Fish_summary[1,]$k
print("Fish best k")
print(Fish_best_k)

Fish_best_acc = Fish_summary[1,]$avg_acc
print("Fish best CV average accuracy")
print(Fish_best_acc)

#accuracy on test data

Fish_complete = rbind(Fish_test, Fish_train)
Fish_test_indices = 1:nrow(Fish_test)
Fish_train_indices = (nrow(Fish_test)+1):(nrow(Fish_test)+nrow(Fish_train))
Fish_trainclass = Fish_train$V1
Fish_testclass = Fish_test$V1

###representation raw

###distance EDR

Fish_complete_EDR = TSDatabaseDistances(Fish_complete[,-1],distance='edr',epsilon=0.1)
Fish_complete_EDR = as.matrix(Fish_complete_EDR)
diag(Fish_complete_EDR)=large_number

###k
Fish_test_classification=test_classification(dist_matrix = Fish_complete_EDR,
                                             train_class = Fish_trainclass,
                                             test_indices = Fish_test_indices,
                                             k=Fish_best_k )

Fish_pred_test = Fish_test_classification$prediction$predicted

Fish_test_accuracy=sum(Fish_testclass==Fish_pred_test)/length(Fish_testclass)
print("Fish test accuracy")
print(Fish_test_accuracy)

Fish_df = data.table(dataset="Fish", 
                     best_repr=Fish_best_representation, 
                     best_dist=Fish_best_dist, 
                     best_k=Fish_best_k, 
                     cv_avg_acc = Fish_best_acc,
                     test_acc = Fish_test_accuracy)

print(Fish_df)

## PowerCons

PowerCons_train = as.data.table(PowerCons_train)
PowerCons_results = complete_paramater_evalutaion(PowerCons_train)

PowerCons_complete=PowerCons_results[[1]]
PowerCons_summary=PowerCons_results[[2]]

head(PowerCons_summary)

PowerCons_best_representation= PowerCons_summary[1,]$representation
print("PowerCons best representation")
print(PowerCons_best_representation)

PowerCons_best_dist = PowerCons_summary[1,]$approach
print("PowerCons best dist")
print(PowerCons_best_dist)

PowerCons_best_k = PowerCons_summary[1,]$k
print("PowerCons best k")
print(PowerCons_best_k)

PowerCons_best_acc = PowerCons_summary[1,]$avg_acc
print("PowerCons best CV average accuracy")
print(PowerCons_best_acc)

#accuracy on test data

PowerCons_complete = rbind(PowerCons_test, PowerCons_train)
PowerCons_test_indices = 1:nrow(PowerCons_test)
PowerCons_train_indices = (nrow(PowerCons_test)+1):(nrow(PowerCons_test)+nrow(PowerCons_train))
PowerCons_trainclass = PowerCons_train$V1
PowerCons_testclass = PowerCons_test$V1

###representation raw

###distance  BURASI YAPILMADI

PowerCons_complete_euc = as.matrix(dist(PowerCons_complete[,-1], upper=TRUE))
diag(PowerCons_complete_euc)=large_number

###k
PowerCons_test_classification=test_classification(dist_matrix = PowerCons_complete_euc,
                                                  train_class = PowerCons_trainclass,
                                                  test_indices = PowerCons_test_indices,
                                                  k=PowerCons_best_k )

PowerCons_pred_test = PowerCons_test_classification$prediction$predicted

PowerCons_test_accuracy=sum(PowerCons_testclass==PowerCons_pred_test)/length(PowerCons_testclass)
print("PowerCons test accuracy")
print(PowerCons_test_accuracy)

PowerCons_df = data.table(dataset="PowerCons", 
                          best_repr=PowerCons_best_representation, 
                          best_dist=PowerCons_best_dist, 
                          best_k=PowerCons_best_k, 
                          cv_avg_acc = PowerCons_best_acc,
                          test_acc = PowerCons_test_accuracy)

print(PowerCons_df)

all_datasets_df= rbind(ECG_df, Plane_df, Trace_df, Fish_df, PowerCons_df)
print(all_datasets_df)
