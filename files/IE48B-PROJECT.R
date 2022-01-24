require(data.table)
require(lubridate)
#require(dtw)
require(TunePareto)
require(TSdist)
require (ggplot2)

getwd()
setwd('C:/Users/asus_pinar/Desktop/Files/48bproject')

dat=fread('YAT-01012019-10122021.csv')

setnames(dat,names(dat),c('date','hour','yat_one','yat_two','yat_three'))
dat[,datex:=strptime(date,'%d/%m/%Y')]

dat[,tst:=ymd_hm(paste(datex,hour))]
dat[,date:=date(tst)]
dat[,hour:=hour(tst)]

dat[,yat_one_t:=gsub('\\.','',yat_one)]
dat[,yat_two_t:=gsub('\\.','',yat_two)]
dat[,yat_three_t:=gsub('\\.','',yat_three)]


dat[,yat_one:=as.numeric(gsub(',','.',yat_one_t))]
dat[,yat_two:=as.numeric(gsub(',','.',yat_two_t))]
dat[,yat_three:=as.numeric(gsub(',','.',yat_three_t))]


yat_dat=dat[,list(date,hour,yat_one,yat_two,yat_three)]


dat=fread('YAL-01012019-10122021.csv')

# naming is temporary here, used the same set of codes
setnames(dat,names(dat),c('date','hour','yat_one','yat_two','yat_three'))
dat[,datex:=strptime(date,'%d/%m/%Y')]

dat[,tst:=ymd_hm(paste(datex,hour))]
dat[,date:=date(tst)]
dat[,hour:=hour(tst)]

dat[,yat_one_t:=gsub('\\.','',yat_one)]
dat[,yat_two_t:=gsub('\\.','',yat_two)]
dat[,yat_three_t:=gsub('\\.','',yat_three)]


dat[,yal_one:=as.numeric(gsub(',','.',yat_one_t))]
dat[,yal_two:=as.numeric(gsub(',','.',yat_two_t))]
dat[,yal_three:=as.numeric(gsub(',','.',yat_three_t))]


yal_dat=dat[,list(date,hour,yal_one,yal_two,yal_three)]
############# ABOVE CODE IS THE SAMPLE CODE FROM MOODLE

## YAL - up regulation
yal_dat[,tot_yal:=yal_one+yal_two+yal_three]

## YAT - down regulation
yat_dat[,tot_yat:=yat_one+yat_two+yat_three]

## combining YAL & YAT
comp_dat = data.table()
comp_dat = cbind(yal_dat[,c("date","hour","tot_yal")],yat_dat[,"tot_yat"])

## If total down volume is greater than up volume, the class is positive (energy
##surplus), it is negative otherwise. Y

comp_dat[, net_volume:=tot_yat-tot_yal] 

comp_dat[(net_volume>=(-50)) & (net_volume<=50), sign:=0]
comp_dat[(net_volume<(-50)) , sign:=-1]
comp_dat[(net_volume>50) , sign:=1]
sum(is.na(comp_dat$sign))  #NA yok


length(unique(comp_dat$date)) #1075 gün var
min(comp_dat$date)
max(comp_dat$date)

wide_comp_dat= dcast(comp_dat, formula = hour~date, value.var="sign")


ggplot(comp_dat, aes(x = date, y = sign)) + geom_histogram(aes(color = date)) + facet_wrap(~hour)


ggplot(comp_dat, aes(x = date, y = net_volume)) + geom_line()

ggplot(comp_dat[hour==0,], aes(x = date, y = sign)) + geom_point(aes(color = date))

ggplot(comp_dat[date=="2019-01-01",], aes(x = hour, y = sign)) + geom_line(aes(color = date))

## FUNCTIONS

evaluate_selected_method=function(windows_set,dist_mat, k){
  
  trainclass = windows_set$sign
  nof_rep=5
  n_fold=10
  set.seed(171)
  cv_indices=generateCVRuns(trainclass, 
                            ntimes =nof_rep, 
                            nfold = n_fold,
                            leaveOneOut = FALSE, 
                            stratified = TRUE)
  
  
  result=vector('list',nof_rep*n_fold)
  
  iter=1
  
  for(i in 1:nof_rep){
    this_fold=cv_indices[[i]]
    for(j in 1:n_fold){
      test_indices=this_fold[[j]]
      current_k=k
      current_fold=nn_classify_cv(dist_mat,trainclass,test_indices,k=current_k)
      accuracy=sum(trainclass[test_indices]==current_fold$prediction$predicted)/length(test_indices)
      tmp=data.table(repid=i,foldid=j,
                     k=current_k,acc=accuracy)
      result[[iter]]=tmp
      iter=iter+1
    }
  }
  
  return(result)
}

get_window = function(comp_dat, hr, window_size){
  
  dummy_hour_df = comp_dat[comp_dat$hour==hr,]
  unique_date_list = unique(dummy_hour_df$date) #indices from 1
  
  col_names=rep(0,window_size)
  
  for (i in 1:window_size){
    col_names[i] = paste("day-" ,i, sep="")
  }

  train_for_classification = data.table()

  for (i in unique_date_list){
   repr_for_each_day = data.table()
  
   for( d in 1:window_size){
     repr_for_each_day= cbind(repr_for_each_day,dummy_hour_df[date==as.Date((i-d), origin="1970-01-01"),"sign"])
   }
  
   colnames(repr_for_each_day)= col_names
   repr_for_each_day$sign = dummy_hour_df[date==as.Date((i), origin="1970-01-01"),"sign"]
   train_for_classification = rbind(train_for_classification,repr_for_each_day)
  }
  
  #first w series will have NAs 
  #drop
  train_for_classification = train_for_classification[-c(1:window_size),]
  return(train_for_classification)
} 


#returns euclidean, lcss and edr distances given the dataset

get_distances = function(dataset){ #remove class column
  large_number = 99999
  
  #euclidean distance
  euc_dist = as.matrix(dist(dataset, upper=TRUE))
  diag(euc_dist)=large_number
  
  #LCSS distance
  lcss_dist=TSDatabaseDistances(dataset,distance='lcss',epsilon=0.1)
  lcss_dist=as.matrix(lcss_dist)
  diag(lcss_dist)=large_number
  
  #EDR distance
  edr_dist = TSDatabaseDistances(dataset,distance='edr',epsilon=0.1)
  edr_dist=as.matrix(edr_dist)
  diag(edr_dist)=large_number
  
  return(list(euc_dist, lcss_dist, edr_dist))
}


## nn classification for a given k
nn_classify_cv=function(dist_matrix,train_class,test_indices,k){
  
  dist_matrix = as.data.frame(dist_matrix)

  test_distances_to_train=as.matrix(dist_matrix[test_indices,])
  test_distances_to_train=test_distances_to_train[,-test_indices]
  trainclass=train_class[-test_indices]
  
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
  long_nn_class = as.data.table(long_nn_class)
  
  class_counts=long_nn_class[,.N,list(id,value)]
  
  class_counts[,predicted_prob:=N/k]
  wide_class_prob_predictions=dcast(class_counts,id~value,value.var='predicted_prob')
  wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0
  class_predictions=class_counts[,list(predicted=value[which.max(N)]),by=list(id)]
  
  return(list(prediction=class_predictions,prob_estimates=wide_class_prob_predictions))
}


#classifies the test data
test_classification=function(dist_matrix,train_class,test_indices,k){
  
  dist_matrix = as.data.frame(dist_matrix)
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
  long_nn_class = as.data.table(long_nn_class)
  class_counts=long_nn_class[,.N,list(id,value)]
  class_counts[,predicted_prob:=N/k]
  wide_class_prob_predictions=dcast(class_counts,id~value,value.var='predicted_prob')
  wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0
  class_predictions=class_counts[,list(predicted=value[which.max(N)]),by=list(id)]
  
  
  return(list(prediction=class_predictions,prob_estimates=wide_class_prob_predictions))
}


#returns paa representation
get_paa=function(dataset,segment_length, method){
  
  train_dummy = data.table(dataset)
  setnames(train_dummy,'sign','class')
  train_dummy = train_dummy[order(class)]
  train_dummy[,class:=as.character(class)]
  train_dummy[,id:=1:.N]
  
  
  long_train=melt(train_dummy,id.vars=c('id','class'))
  # need to get numerical part of the variable (represents time)
  # using gsub to set the nonnumerical part to zero length
  long_train = as.data.table(long_train)
  long_train[,time:=as.numeric(gsub("day-", "", variable))]
  long_train=long_train[,list(id,class,time,value)]
  long_train=long_train[order(id,time)]
  
  dt_paa = data.table()
  for (i in 1:nrow(dataset)){
    
    selected_series=i
    data_ts=long_train[id==selected_series]$value
    paa_rep=repr_paa(data_ts, segment_length, method)
    #plot(paa_rep,type='l')
    
    dt_paa=rbind(dt_paa,t(as.integer(paa_rep)))
  }
  
  return(dt_paa)
}


## evaluates euclidean, lcss and edr distances given a representation
#k_levels - k list for k-nn 
#datarepr - without class info
#trainclass 

eval_distances_for_rep=function(trainclass, datarepr, k_levels){
  
  nof_rep=5
  n_fold=10
  set.seed(171)
  cv_indices=generateCVRuns(trainclass, 
                            ntimes =nof_rep, 
                            nfold = n_fold,
                            leaveOneOut = FALSE, 
                            stratified = TRUE)
  
  dist_list = get_distances(datarepr)
  result=vector('list',length(dist_list)*nof_rep*n_fold*length(k_levels))
  
  iter=1
  
  for(m in 1:length(dist_list)){ 
    
    if(m==1)
    {apprch = "euclidean distance"
    print("euclidean distance")}
    if(m==2)
    {apprch = "lcss distance"
    print("lcss distance")}
    if(m==3)
    {apprch = "edr distance"
    print("edr distance")}
    
    
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



complete_paramater_evalutaion=function(dataset, k_levels){

  print("Raw Data")
  raw_data_evalutaion=eval_distances_for_rep(trainclass = dataset$sign, datarepr = dataset[,-"sign"],k_levels = k_levels)
  raw_data_results = rbindlist(raw_data_evalutaion)
  raw_data_summary = raw_data_results[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(approach,k)]
  raw_data_results[,representation:="Raw"]
  
  #print(ggplot(raw_data_results,aes(x=paste0(representation,'+',approach,'+',k), y=acc)) +
   #       geom_boxplot()+
    #      coord_flip()+labs(title="Raw Results", x="Accuracy", y="Approach"))
  
    complete_results = raw_data_results
  
  #ggplot(complete_results,aes(x=paste0(representation,'+',approach,'+',k), y=acc)) +
   # geom_boxplot()+
    #coord_flip()+labs(title="Complete Result Plot of ECG", y="Accuracy", x="Approach")
  
  #complete_summary = complete_results[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(representation,approach,k)]
  #print("Complete Results Summary")
  #print(complete_summary)
  
  #order by avg accuracy
  #complete_summary=complete_summary[order(complete_summary$avg_acc, decreasing = TRUE)]
  #print("Complete Results Summary Ordered")
  #print(complete_summary)
  
  return(complete_results)
}



############# MODEL TRIALS

hour_list = unique(comp_dat$hour)

### hour for starts

window_size_list = c(14)
#window_size_list = c(7)

all_hours_summary=list()

ppp=1
for (hr in c(11,12,13,14)){
  print(hr)
  for (window_size in window_size_list){
 
  col_names=rep(0,window_size)

  for (i in 1:window_size){
    col_names[i] = paste("day-" ,i, sep="")
  }


  dummy_hour_df = comp_dat[comp_dat$hour==hr,]
  unique_date_list = unique(dummy_hour_df$date) #indices from 1

  train_for_classification = data.table()
  
  for (i in unique_date_list){
    
      repr_for_each_day = data.table()
    
      for( d in 1:window_size){
      repr_for_each_day= cbind(repr_for_each_day,dummy_hour_df[date==as.Date((i-d), origin="1970-01-01"),"sign"])
      }

      colnames(repr_for_each_day)= col_names
      repr_for_each_day$sign = dummy_hour_df[date==as.Date((i), origin="1970-01-01"),"sign"]
      train_for_classification = rbind(train_for_classification,repr_for_each_day)
  }   

  #first w series will have NAs 
  #drop
  train_for_classification = train_for_classification[-c(1:window_size),]

  k_list = c(5,10,15,20,25)
  
  result_dummy = complete_paramater_evalutaion(dataset = train_for_classification,k_levels = k_list)
  comp_result = result_dummy
  comp_result = as.data.table(comp_result)
  comp_result[,w_size := window_size]
  comp_result[,hour := hr]
  
  comp_summary = comp_result[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(representation,approach,k,w_size)]
  #order by avg accuracy
  comp_summary=comp_summary[order(comp_summary$avg_acc, decreasing = TRUE)]
}
  all_hours_summary[[ppp]] = comp_summary
  ppp = ppp+1
  
}


all_hours_summary2=list()
ppp=1

for (hr in c(15,16,17,18)){
  print(hr)
  for (window_size in window_size_list){
    
    col_names=rep(0,window_size)
    
    for (i in 1:window_size){
      col_names[i] = paste("day-" ,i, sep="")
    }
    
    
    dummy_hour_df = comp_dat[comp_dat$hour==hr,]
    unique_date_list = unique(dummy_hour_df$date) #indices from 1
    
    train_for_classification = data.table()
    
    for (i in unique_date_list){
      
      repr_for_each_day = data.table()
      
      for( d in 1:window_size){
        repr_for_each_day= cbind(repr_for_each_day,dummy_hour_df[date==as.Date((i-d), origin="1970-01-01"),"sign"])
      }
      
      colnames(repr_for_each_day)= col_names
      repr_for_each_day$sign = dummy_hour_df[date==as.Date((i), origin="1970-01-01"),"sign"]
      train_for_classification = rbind(train_for_classification,repr_for_each_day)
    }   
    
    #first w series will have NAs 
    #drop
    train_for_classification = train_for_classification[-c(1:window_size),]
    
    k_list = c(5,10,15,20,25)
    
    result_dummy = complete_paramater_evalutaion(dataset = train_for_classification,k_levels = k_list)
    comp_result = result_dummy
    comp_result = as.data.table(comp_result)
    comp_result[,w_size := window_size]
    comp_result[,hour := hr]
    
    comp_summary = comp_result[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(representation,approach,k,w_size)]
    #order by avg accuracy
    comp_summary=comp_summary[order(comp_summary$avg_acc, decreasing = TRUE)]
  }
  all_hours_summary2[[ppp]] = comp_summary
  ppp = ppp+1
  
}


all_hours_summary3=list()
ppp=1

for (hr in c(19,20,21,22,23)){
  print(hr)
  for (window_size in window_size_list){
    
    col_names=rep(0,window_size)
    
    for (i in 1:window_size){
      col_names[i] = paste("day-" ,i, sep="")
    }
    
    
    dummy_hour_df = comp_dat[comp_dat$hour==hr,]
    unique_date_list = unique(dummy_hour_df$date) #indices from 1
    
    train_for_classification = data.table()
    
    for (i in unique_date_list){
      
      repr_for_each_day = data.table()
      
      for( d in 1:window_size){
        repr_for_each_day= cbind(repr_for_each_day,dummy_hour_df[date==as.Date((i-d), origin="1970-01-01"),"sign"])
      }
      
      colnames(repr_for_each_day)= col_names
      repr_for_each_day$sign = dummy_hour_df[date==as.Date((i), origin="1970-01-01"),"sign"]
      train_for_classification = rbind(train_for_classification,repr_for_each_day)
    }   
    
    #first w series will have NAs 
    #drop
    train_for_classification = train_for_classification[-c(1:window_size),]
    
    k_list = c(5,10,15,20,25)
    
    result_dummy = complete_paramater_evalutaion(dataset = train_for_classification,k_levels = k_list)
    comp_result = result_dummy
    comp_result = as.data.table(comp_result)
    comp_result[,w_size := window_size]
    comp_result[,hour := hr]
    
    comp_summary = comp_result[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(representation,approach,k,w_size)]
    #order by avg accuracy
    comp_summary=comp_summary[order(comp_summary$avg_acc, decreasing = TRUE)]
  }
  all_hours_summary3[[ppp]] = comp_summary
  ppp = ppp+1
  
}

#all_hours_summary[[1]]  #0
#all_hours_summary[[2]]  #4
#all_hours_summary[[3]]  #5
#all_hours_summary2[[1]] #6
#all_hours_summary2[[2]] #7
#all_hours_summary2[[3]] #8
#all_hours_summary3[[1]] #9
#all_hours_summary3[[2]] #10
#all_hours_summary3[[3]] #11
#all_hours_summary3[[4]] #12

all_hours_summary[[1]]  #11
all_hours_summary[[2]]  #12
all_hours_summary[[3]]  #13
all_hours_summary[[4]]  #14

all_hours_summary2[[1]] #15
all_hours_summary2[[2]] #16
all_hours_summary2[[3]] #17
all_hours_summary2[[4]] #18

all_hours_summary3[[1]] #19
all_hours_summary3[[2]] #20
all_hours_summary3[[3]] #21
all_hours_summary3[[4]] #22
all_hours_summary3[[5]] #23


############# CV PHASE ENDS

##### COMPARISON

#compare with baselines 
base_df = comp_dat
### most frequent sign in train data for hour 

hours=c(0:23)
#hours

freq_table=as.data.table(hours)

freq_table$neg_count = 99999
freq_table$pos_count = 99999
freq_table$balanced_count = 99999

for (i in hours){
  pos_count = sum(base_df[hour==i,sign==1])
  freq_table[hours==i]$pos_count = pos_count
}

for (i in hours){
  neg_count = sum(base_df[hour==i,sign==-1])
  freq_table[hours==i]$neg_count = neg_count
}

for (i in hours){
  bal_count = sum(base_df[hour==i,sign==0])
  freq_table[hours==i]$balanced_count = bal_count
}

freq_table[,dummy := apply(freq_table[,c("neg_count", "pos_count", "balanced_count")], MARGIN = 1, which.max)]

freq_table[dummy==1,net_sign:=-1 ]
freq_table[dummy==2,net_sign:=1 ]
freq_table[dummy==3,net_sign:=0 ]

base_df$base1_pred = rep(freq_table$net_sign,1075)

base_df[,base1_correct := sign == base1_pred]

base1_score = sum(base_df$base1_correct)/nrow(base_df)
base1_score

### Sign of yesterday same hour lag24

base_df[,sign_prevday:=shift(sign, 24)]

base_df[,base2_correct := sign == sign_prevday]

base2_score = sum(base_df$base2_correct, na.rm=TRUE)/(nrow(base_df)-24)
base2_score

### Sign of prev. week same day same hour lag168

base_df[,sign_prevweek:=shift(sign, 168)]

base_df[,base3_correct := sign == sign_prevweek]

base3_score = sum(base_df$base3_correct, na.rm=TRUE)/(nrow(base_df)-168)
base3_score

#all 0 

base_df[,all_zeros:=0]

base_df[,all_zeros_correct := sign == all_zeros]

all_zeros_score = sum(base_df$all_zeros_correct)/(nrow(base_df))
all_zeros_score

#all 1

base_df[,all_ones:=1]

base_df[,all_ones_correct := sign == all_ones]

all_ones_score = sum(base_df$all_ones_correct)/(nrow(base_df))
all_ones_score

#all -1

base_df[,all_mones:=-1]

base_df[,all_mones_correct := sign == all_mones]

all_mones_score = sum(base_df$all_mones_correct)/(nrow(base_df))
all_mones_score

base_hours = data.table(hours)

base1_list = list()
for (h in hours){
  base1_list= append(base1_list,(sum(base_df[hour==h]$base1_correct)/nrow(base_df[hour==h])))
}
base_hours$base_freq = base1_list

base2_list = list()
for (h in hours){
  base2_list= append(base2_list,(sum(base_df[hour==h]$base2_correct, na.rm=TRUE)/(nrow(base_df[hour==h])-1)))
}
base_hours$base_24lag = base2_list

base3_list = list()
for (h in hours){
  base3_list= append(base3_list,(sum(base_df[hour==h]$base3_correct, na.rm=TRUE)/(nrow(base_df[hour==h])-7)))
}
base_hours$base_168lag = base3_list

allzro = list()
for (h in hours){
  allzro= append(allzro,(sum(base_df[hour==h]$all_zeros_correct)/(nrow(base_df[hour==h]))))
}
base_hours$all_zeros = allzro

allo = list()
for (h in hours){
  allo= append(allo,(sum(base_df[hour==h]$all_ones_correct)/nrow(base_df[hour==h])))
}
base_hours$all_ones = allo

allm = list()
for (h in hours){
  allm= append(allm,(sum(base_df[hour==h]$all_mones_correct)/nrow(base_df[hour==h])))
}
base_hours$all_mones = allm

#EVALUTAION RESULTS - BEST MODELS
large_number=99999


#hour0
####euclidean distance k=20 w_size = 7  

hour0_windows = get_window(comp_dat, 0, 7)

#euclidean distance
hour0_dist = as.matrix(dist(hour0_windows[,-c("sign","date")], upper=TRUE))
diag(hour0_dist)=large_number


hour0_eval = evaluate_selected_method(windows_set =hour0_windows,k = 20, dist_mat =hour0_dist )

hour0_eval= rbindlist(hour0_eval)
hour0_eval_summary = hour0_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==0, tuned_method_acc:=hour0_eval_summary$avg_acc]
base_hours[hours==0, tuned_method_sd:=hour0_eval_summary$sdev_acc]


#hour1
####euclidean distance k=20 w_size = 14  

hour1_windows = get_window(comp_dat, 1, 14)

#euclidean distance
hour1_dist = as.matrix(dist(hour1_windows[,-c("sign","date")], upper=TRUE))
diag(hour1_dist)=large_number

hour1_eval = evaluate_selected_method(windows_set =hour1_windows,k = 20, dist_mat = hour1_dist )

hour1_eval= rbindlist(hour1_eval)
hour1_eval_summary = hour1_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==1, tuned_method_acc:=hour1_eval_summary$avg_acc]
base_hours[hours==1, tuned_method_sd:=hour1_eval_summary$sdev_acc]



#hour2
####edr distance k=20 w_size =14

hour2_windows=get_window(comp_dat, 2, 14)


#EDR distance
hour2_dist = TSDatabaseDistances(hour2_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour2_dist=as.matrix(hour2_dist)
diag(hour2_dist)=large_number

hour2_eval = evaluate_selected_method(windows_set =hour2_windows,k = 20, dist_mat =hour2_dist )

hour2_eval= rbindlist(hour2_eval)
hour2_eval_summary = hour2_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==2, tuned_method_acc:=hour2_eval_summary$avg_acc]
base_hours[hours==2, tuned_method_sd:=hour2_eval_summary$sdev_acc]



#hour3
####edr distance k=10 w_size =14

hour3_windows=get_window(comp_dat, 3, 14)

#EDR distance
hour3_dist = TSDatabaseDistances(hour3_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour3_dist=as.matrix(hour3_dist)
diag(hour3_dist)=large_number

hour3_eval = evaluate_selected_method(windows_set =hour3_windows,k = 10, dist_mat =hour3_dist )

hour3_eval= rbindlist(hour3_eval)
hour3_eval_summary = hour3_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==3, tuned_method_acc:=hour3_eval_summary$avg_acc]
base_hours[hours==3, tuned_method_sd:=hour3_eval_summary$sdev_acc]



#hour4
####edr distance k=20 w_size =14 

hour4_windows=get_window(comp_dat, 4, 14)

#EDR distance
hour4_dist = TSDatabaseDistances(hour4_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour4_dist=as.matrix(hour4_dist)
diag(hour4_dist)=large_number

hour4_eval = evaluate_selected_method(windows_set =hour4_windows,k = 20, dist_mat = hour4_dist )

hour4_eval= rbindlist(hour4_eval)
hour4_eval_summary = hour4_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==4, tuned_method_acc:=hour4_eval_summary$avg_acc]
base_hours[hours==4, tuned_method_sd:=hour4_eval_summary$sdev_acc]


#hour5
####edr distance k=20 w_size =14 

hour5_windows=get_window(comp_dat, 5, 14)

#EDR distance
hour5_dist = TSDatabaseDistances(hour5_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour5_dist=as.matrix(hour5_dist)
diag(hour5_dist)=large_number

hour5_eval = evaluate_selected_method(windows_set =hour5_windows,k = 20, dist_mat = hour5_dist )


hour5_eval= rbindlist(hour5_eval)
hour5_eval_summary = hour5_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==5, tuned_method_acc:=hour5_eval_summary$avg_acc]
base_hours[hours==5, tuned_method_sd:=hour5_eval_summary$sdev_acc]


#hour6
####edr distance k=20 w_size = 14

hour6_windows=get_window(comp_dat, 6, 14)

#EDR distance
hour6_dist = TSDatabaseDistances(hour6_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour6_dist=as.matrix(hour6_dist)
diag(hour6_dist)=large_number

hour6_eval = evaluate_selected_method(windows_set =hour6_windows,k = 20, dist_mat = hour6_dist )

hour6_eval= rbindlist(hour6_eval)
hour6_eval_summary = hour6_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==6, tuned_method_acc:=hour6_eval_summary$avg_acc]
base_hours[hours==6, tuned_method_sd:=hour6_eval_summary$sdev_acc]


#hour7
####edr distance k=20 w_size =14

hour7_windows=get_window(comp_dat, 7, 12)

#EDR distance
hour7_dist = TSDatabaseDistances(hour7_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour7_dist=as.matrix(hour7_dist)
diag(hour7_dist)=large_number

hour7_eval = evaluate_selected_method(windows_set =hour7_windows,k = 20, dist_mat = hour7_dist )

hour7_eval= rbindlist(hour7_eval)
hour7_eval_summary = hour7_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==7, tuned_method_acc:=hour7_eval_summary$avg_acc]
base_hours[hours==7, tuned_method_sd:=hour7_eval_summary$sdev_acc]


#hour8
####euclidean distance k=25 w_size =7

hour8_windows=get_window(comp_dat, 8, 7)

#euclidean distance
hour8_dist = as.matrix(dist(hour8_windows[,-c("sign","date")], upper=TRUE))
diag(hour8_dist)=large_number

hour8_eval = evaluate_selected_method(windows_set =hour8_windows,k = 25, dist_mat = hour8_dist )

hour8_eval= rbindlist(hour8_eval)
hour8_eval_summary = hour8_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==8, tuned_method_acc:=hour8_eval_summary$avg_acc]
base_hours[hours==8, tuned_method_sd:=hour8_eval_summary$sdev_acc]


#hour9
####euclidean distance k=15 w_size =14 
hour9_windows=get_window(comp_dat, 9, 14)

#euclidean distance
hour9_dist = as.matrix(dist(hour9_windows[,-c("sign","date")], upper=TRUE))
diag(hour9_dist)=large_number

hour9_eval = evaluate_selected_method(windows_set =hour9_windows,k = 15, dist_mat = hour9_dist )

hour9_eval= rbindlist(hour9_eval)
hour9_eval_summary = hour9_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==9, tuned_method_acc:=hour9_eval_summary$avg_acc]
base_hours[hours==9, tuned_method_sd:=hour9_eval_summary$sdev_acc]


#hour10
####euclidean distance k=20 w_size = 14 

hour10_windows=get_window(comp_dat, 10, 14)

#euclidean distance
hour10_dist = as.matrix(dist(hour10_windows[,-c("sign","date")], upper=TRUE))
diag(hour10_dist)=large_number

hour10_eval = evaluate_selected_method(windows_set =hour10_windows,k = 20, dist_mat = hour10_dist )

hour10_eval= rbindlist(hour10_eval)
hour10_eval_summary = hour10_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==10, tuned_method_acc:=hour10_eval_summary$avg_acc]
base_hours[hours==10, tuned_method_sd:=hour10_eval_summary$sdev_acc]


#hour11
####edr distance k=15 w_size =7

hour11_windows=get_window(comp_dat, 11, 7)


#EDR distance
hour11_dist = TSDatabaseDistances(hour11_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour11_dist=as.matrix(hour11_dist)
diag(hour11_dist)=large_number

hour11_eval = evaluate_selected_method(windows_set =hour11_windows,k = 15, dist_mat = hour11_dist )

hour11_eval= rbindlist(hour11_eval)
hour11_eval_summary = hour11_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==11, tuned_method_acc:=hour11_eval_summary$avg_acc]
base_hours[hours==11, tuned_method_sd:=hour11_eval_summary$sdev_acc]

#hour12
####edr distance k=15 w_size =14 

#hour12_windows=windows_all[[13]] #ilk window - latest 
hour12_windows=get_window(comp_dat, 12, 14)

#EDR distance
hour12_dist = TSDatabaseDistances(hour12_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour12_dist=as.matrix(hour12_dist)
diag(hour12_dist)=large_number

hour12_eval = evaluate_selected_method(windows_set =hour12_windows,k = 15, dist_mat = hour12_dist )

hour12_eval= rbindlist(hour12_eval)
hour12_eval_summary = hour12_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==12, tuned_method_acc:=hour12_eval_summary$avg_acc]
base_hours[hours==12, tuned_method_sd:=hour12_eval_summary$sdev_acc]

#hour13
####edr distance k=20 w_size =7 

hour13_windows=get_window(comp_dat, 13, 7)

#EDR distance
hour13_dist = TSDatabaseDistances(hour13_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour13_dist=as.matrix(hour13_dist)
diag(hour13_dist)=large_number

hour13_eval = evaluate_selected_method(windows_set =hour13_windows,k = 20, dist_mat = hour13_dist )

hour13_eval= rbindlist(hour13_eval)
hour13_eval_summary = hour13_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==13, tuned_method_acc:=hour13_eval_summary$avg_acc]
base_hours[hours==13, tuned_method_sd:=hour13_eval_summary$sdev_acc]


#hour14
####euclidean distance k=20 w_size = 7 

hour14_windows=get_window(comp_dat, 14, 7)

#euclidean distance
hour14_dist = as.matrix(dist(hour14_windows[,-c("sign","date")], upper=TRUE))
diag(hour14_dist)=large_number


hour14_eval = evaluate_selected_method(windows_set =hour14_windows,k = 20, dist_mat = hour14_dist )

hour14_eval= rbindlist(hour14_eval)
hour14_eval_summary = hour14_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==14, tuned_method_acc:=hour14_eval_summary$avg_acc]
base_hours[hours==14, tuned_method_sd:=hour14_eval_summary$sdev_acc]




#hour15
####edr distance k=10 w_size = 14

#hour15_windows=windows_all[[16]] #ilk window - latest 
hour15_windows=get_window(comp_dat, 15, 14)

#EDR distance
hour15_dist = TSDatabaseDistances(hour15_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour15_dist=as.matrix(hour15_dist)
diag(hour15_dist)=large_number

hour15_eval = evaluate_selected_method(windows_set =hour15_windows,k = 10, dist_mat = hour15_dist )

hour15_eval= rbindlist(hour15_eval)
hour15_eval_summary = hour15_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==15, tuned_method_acc:=hour15_eval_summary$avg_acc]
base_hours[hours==15, tuned_method_sd:=hour15_eval_summary$sdev_acc]


#hour16
####euclidean distance k=25 w_size =7

hour16_windows=get_window(comp_dat, 16, 7)

#euclidean distance
hour16_dist = as.matrix(dist(hour16_windows[,-c("sign","date")], upper=TRUE))
diag(hour16_dist)=large_number


hour16_eval = evaluate_selected_method(windows_set =hour16_windows,k = 25, dist_mat = hour16_dist )

hour16_eval= rbindlist(hour16_eval)
hour16_eval_summary = hour16_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==16, tuned_method_acc:=hour16_eval_summary$avg_acc]
base_hours[hours==16, tuned_method_sd:=hour16_eval_summary$sdev_acc]


#hour17

####edr distance k=15 w_size =14 

hour17_windows=get_window(comp_dat, 17, 14)

#EDR distance
hour17_dist = TSDatabaseDistances(hour17_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour17_dist=as.matrix(hour17_dist)
diag(hour17_dist)=large_number

hour17_eval = evaluate_selected_method(windows_set =hour17_windows,k =15, dist_mat = hour17_dist )

hour17_eval= rbindlist(hour17_eval)
hour17_eval_summary = hour17_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==17, tuned_method_acc:=hour17_eval_summary$avg_acc]
base_hours[hours==17, tuned_method_sd:=hour17_eval_summary$sdev_acc]


#hour18
####edr distance k=10 w_size = 14

hour18_windows=get_window(comp_dat, 18, 14)

#EDR distance
hour18_dist = TSDatabaseDistances(hour18_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour18_dist=as.matrix(hour18_dist)
diag(hour18_dist)=large_number

hour18_eval = evaluate_selected_method(windows_set =hour18_windows,k = 10, dist_mat = hour18_dist )

hour18_eval= rbindlist(hour18_eval)
hour18_eval_summary = hour18_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==18, tuned_method_acc:=hour18_eval_summary$avg_acc]
base_hours[hours==18, tuned_method_sd:=hour18_eval_summary$sdev_acc]


#hour19
####euclidean distance k=20 w_size =14

#hour19_windows=windows_all[[20]] #ilk window - latest 
hour19_windows=get_window(comp_dat, 19, 14)


#euclidean distance
hour19_dist = as.matrix(dist(hour19_windows[,-c("sign","date")], upper=TRUE))
diag(hour19_dist)=large_number

hour19_eval = evaluate_selected_method(windows_set =hour19_windows,k =20, dist_mat = hour19_dist )

hour19_eval= rbindlist(hour19_eval)
hour19_eval_summary = hour19_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==19, tuned_method_acc:=hour19_eval_summary$avg_acc]
base_hours[hours==19, tuned_method_sd:=hour19_eval_summary$sdev_acc]


#hour20
####edr distance k=15 w_size =14 

hour20_windows=get_window(comp_dat, 20, 14)

#EDR distance
hour20_dist = TSDatabaseDistances(hour20_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour20_dist=as.matrix(hour20_dist)
diag(hour20_dist)=large_number

hour20_eval = evaluate_selected_method(windows_set =hour20_windows,k =15, dist_mat = hour20_dist )

hour20_eval= rbindlist(hour20_eval)
hour20_eval_summary = hour20_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==20, tuned_method_acc:=hour20_eval_summary$avg_acc]
base_hours[hours==20, tuned_method_sd:=hour20_eval_summary$sdev_acc]

#hour21
####euclidean distance k=25 w_size =14

hour21_windows=get_window(comp_dat, 21, 14)

#euclidean distance
hour21_dist = as.matrix(dist(hour21_windows[,-c("sign","date")], upper=TRUE))
diag(hour21_dist)=large_number

hour21_eval = evaluate_selected_method(windows_set =hour21_windows,k =25, dist_mat = hour21_dist )

hour21_eval= rbindlist(hour21_eval)
hour21_eval_summary = hour21_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==21, tuned_method_acc:=hour21_eval_summary$avg_acc]
base_hours[hours==21, tuned_method_sd:=hour21_eval_summary$sdev_acc]


#hour22
####euclidean distance k=20 w_size = 7  

hour22_windows=get_window(comp_dat, 22, 7)


#euclidean distance
hour22_dist = as.matrix(dist(hour22_windows[,-c("sign","date")], upper=TRUE))
diag(hour22_dist)=large_number

hour22_eval = evaluate_selected_method(windows_set=hour22_windows,k=20, dist_mat=hour22_dist )

hour22_eval= rbindlist(hour22_eval)
hour22_eval_summary = hour22_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==22, tuned_method_acc:=hour22_eval_summary$avg_acc]
base_hours[hours==22, tuned_method_sd:=hour22_eval_summary$sdev_acc]

#hour23

####euclidean distance k = 20 w_size = 14

hour23_windows=get_window(comp_dat, 23, 14)


#euclidean distance
hour23_dist = as.matrix(dist(hour23_windows[,-c("sign","date")], upper=TRUE))
diag(hour23_dist)=large_number

hour23_eval = evaluate_selected_method(windows_set=hour23_windows,k=20, dist_mat=hour23_dist )

hour23_eval= rbindlist(hour23_eval)
hour23_eval_summary = hour23_eval[,list(avg_acc=mean(acc),sdev_acc=sd(acc)),]

base_hours[hours==23, tuned_method_acc:=hour23_eval_summary$avg_acc]
base_hours[hours==23, tuned_method_sd:=hour23_eval_summary$sdev_acc]

#########


### NEW DATE PREDICTION 
require(data.table)
require(lubridate)
require(TunePareto)
require(TSdist)

#getwd()
#setwd('C:/Users/asus_pinar/Desktop/Files/48bproject')


get_window = function(comp_dat, hr, window_size){
  
  dummy_hour_df = comp_dat[comp_dat$hour==hr,]
  unique_date_list = unique(dummy_hour_df$date) #indices from 1
  unique_date_list = rev(unique_date_list)
  
  col_names=rep(0,window_size)
  
  for (i in 1:window_size){
    col_names[i] = paste("day-" ,i, sep="")
  }
  
  train_for_classification = data.table()
  
  for (i in unique_date_list){
    repr_for_each_day = data.table()
    
    for( d in 1:window_size){
      repr_for_each_day= cbind(repr_for_each_day,dummy_hour_df[date==as.Date((i-d), origin="1970-01-01"),"sign"])
    }
    
    colnames(repr_for_each_day)= col_names
    repr_for_each_day$sign = dummy_hour_df[date==as.Date((i), origin="1970-01-01"),"sign"]
    repr_for_each_day$date = dummy_hour_df[date==as.Date((i), origin="1970-01-01"),"date"]
    train_for_classification = rbind(train_for_classification,repr_for_each_day)
  }
  
  #first w series will have NAs 
  #drop
  train_for_classification = train_for_classification[-c(nrow(train_for_classification):(nrow(train_for_classification)-(window_size-1))),]
  
  return(train_for_classification)
} 


#test datasýný classify etmek için
test_classification=function(dist_matrix,train_class,test_indices,k){
  
  
  dist_matrix = as.data.frame(dist_matrix)
  test_distances_to_train=as.matrix(dist_matrix[test_indices,])
  test_distances_to_train=test_distances_to_train[,-test_indices]
  trainclass=train_class
  #trainclass=train_class[-test_indices]
  #print(str(test_distances_to_train))
  
  ordered_indices=order(test_distances_to_train,decreasing = FALSE)
  
  if(k==1){
    nearest_class=(trainclass[as.numeric(ordered_indices[1])])
    nearest_class=data.table(id=test_indices,nearest_class)
  } else {
    nearest_class=trainclass[as.numeric(ordered_indices[1:k])]
    nearest_class=as.data.table(nearest_class)
    nearest_class=data.table(id=test_indices,t(nearest_class))
  }
  
  long_nn_class=melt(nearest_class,'id')
  long_nn_class = as.data.table(long_nn_class)
  class_counts=long_nn_class[,.N,list(id,value)]
  class_counts[,predicted_prob:=N/k]
  wide_class_prob_predictions=dcast(class_counts,id~value,value.var='predicted_prob')
  wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0
  class_predictions=class_counts[,list(predicted=value[which.max(N)]),by=list(id)]
  
  
  return(list(prediction=class_predictions,prob_estimates=wide_class_prob_predictions))
}



#################################################################

## bulk_imabalance.csv downloaded from provided drive link

dat_current = fread('bulk_imbalance.csv')

comp_dat = data.table()
comp_dat = dat_current[,c("date","hour","system_direction")]
comp_dat[system_direction=="Neutral", sign:=0]
comp_dat[system_direction=="Negative", sign:=-1]
comp_dat[system_direction=="Positive", sign:=1]
sum(is.na(comp_dat$sign)) 


#wide_comp_dat= dcast(comp_dat, formula = hour~date, value.var="sign")

new_day_hours = seq(0,23) ##?
new_day_hours = data.table(new_day_hours)
colnames(new_day_hours) = "saat"


large_number=99999


## 0 to 12 do not run
#hour0
####euclidean distance k=20 w_size = 7  

hour0_windows = get_window(comp_dat, 0, 7)

#euclidean distance
hour0_dist = as.matrix(dist(hour0_windows[,-c("sign","date")], upper=TRUE))
diag(hour0_dist)=large_number


ti = 1
tc = hour0_windows$sign[-1] 
pred_hour0 = test_classification(hour0_dist,train_class=tc,test_indices=ti,20)

new_day_hours[saat ==0, prediction:=pred_hour0$prediction$predicted]


print("hour0 comp")

#hour1
####euclidean distance k=20 w_size = 14  

hour1_windows = get_window(comp_dat, 1, 14)

#euclidean distance
hour1_dist = as.matrix(dist(hour1_windows[,-c("sign","date")], upper=TRUE))
diag(hour1_dist)=large_number

ti = 1
tc = hour1_windows$sign[-1] 
pred_hour1 = test_classification(hour1_dist,train_class=tc,test_indices=ti,20)


new_day_hours[saat ==1, prediction:=pred_hour1$prediction$predicted]

print("hour1 comp")

#hour2
####edr distance k=20 w_size =14

hour2_windows=get_window(comp_dat, 2, 14)


#EDR distance
hour2_dist = TSDatabaseDistances(hour2_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour2_dist=as.matrix(hour2_dist)
diag(hour2_dist)=large_number

ti = 1
tc = hour2_windows$sign[-1] 
pred_hour2 = test_classification(hour2_dist,train_class=tc,test_indices=ti,20)


new_day_hours[saat ==2, prediction:=pred_hour2$prediction$predicted]

print("hour2 comp")


#hour3
####edr distance k=10 w_size =14

hour3_windows=get_window(comp_dat, 3, 14)

#EDR distance
hour3_dist = TSDatabaseDistances(hour3_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour3_dist=as.matrix(hour3_dist)
diag(hour3_dist)=large_number

ti = 1
tc = hour3_windows$sign[-1] 
pred_hour3 = test_classification(hour3_dist,train_class=tc,test_indices=ti,10)


new_day_hours[saat ==3, prediction:=pred_hour3$prediction$predicted]

print("hour3 comp")

#hour4
####edr distance k=20 w_size =14 

hour4_windows=get_window(comp_dat, 4, 14)

#EDR distance
hour4_dist = TSDatabaseDistances(hour4_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour4_dist=as.matrix(hour4_dist)
diag(hour4_dist)=large_number

ti = 1
tc = hour4_windows$sign[-1] 
pred_hour4 = test_classification(hour4_dist,train_class=tc,test_indices=ti,20)


new_day_hours[saat ==4, prediction:=pred_hour4$prediction$predicted]


print("hour4 comp")

#hour5
####edr distance k=20 w_size =14 

hour5_windows=get_window(comp_dat, 5, 14)

#EDR distance
hour5_dist = TSDatabaseDistances(hour5_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour5_dist=as.matrix(hour5_dist)
diag(hour5_dist)=large_number

ti = 1
tc = hour5_windows$sign[-1] 
pred_hour5 = test_classification(hour5_dist,train_class=tc,test_indices=ti,20)


new_day_hours[saat ==5, prediction:=pred_hour5$prediction$predicted]

print("hour5 comp")


#hour6
####edr distance k=20 w_size = 14

hour6_windows=get_window(comp_dat, 6, 14)

#EDR distance
hour6_dist = TSDatabaseDistances(hour6_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour6_dist=as.matrix(hour6_dist)
diag(hour6_dist)=large_number

ti = 1
tc = hour6_windows$sign[-1] 
pred_hour6 = test_classification(hour6_dist,train_class=tc,test_indices=ti,20)

new_day_hours[saat ==6, prediction:=pred_hour6$prediction$predicted]


print("hour6 comp")

#hour7
####edr distance k=20 w_size =14

hour7_windows=get_window(comp_dat, 7, 12)

#EDR distance
hour7_dist = TSDatabaseDistances(hour7_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour7_dist=as.matrix(hour7_dist)
diag(hour7_dist)=large_number

ti = 1
tc = hour7_windows$sign[-1] 
pred_hour7 = test_classification(hour7_dist,train_class=tc,test_indices=ti,20)

new_day_hours[saat ==7, prediction:=pred_hour7$prediction$predicted]

print("hour7 comp")

#hour8
####euclidean distance k=25 w_size =7

hour8_windows=get_window(comp_dat, 8, 7)

#euclidean distance
hour8_dist = as.matrix(dist(hour8_windows[,-c("sign","date")], upper=TRUE))
diag(hour8_dist)=large_number

ti = 1
tc = hour8_windows$sign[-1] 
pred_hour8 = test_classification(hour8_dist,train_class=tc,test_indices=ti,25)

new_day_hours[saat ==8, prediction:=pred_hour8$prediction$predicted]

print("hour8 comp")


#hour9
####euclidean distance k=15 w_size =14 
hour9_windows=get_window(comp_dat, 9, 14)

#euclidean distance
hour9_dist = as.matrix(dist(hour9_windows[,-c("sign","date")], upper=TRUE))
diag(hour9_dist)=large_number

ti = 1
tc = hour9_windows$sign[-1] 
pred_hour9 = test_classification(hour9_dist,train_class=tc,test_indices=ti,15)

new_day_hours[saat ==9, prediction:=pred_hour9$prediction$predicted]

print("hour9 comp")


#hour10
####euclidean distance k=20 w_size = 14 

hour10_windows=get_window(comp_dat, 10, 14)

#euclidean distance
hour10_dist = as.matrix(dist(hour10_windows[,-c("sign","date")], upper=TRUE))
diag(hour10_dist)=large_number


ti = 1
tc = hour10_windows$sign[-1] 
pred_hour10 = test_classification(hour10_dist,train_class=tc,test_indices=ti,20)

new_day_hours[saat ==10, prediction:=pred_hour10$prediction$predicted]

print("hour10 comp")


#hour11
####edr distance k=15 w_size =7

hour11_windows=get_window(comp_dat, 11, 7)


#EDR distance
hour11_dist = TSDatabaseDistances(hour11_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour11_dist=as.matrix(hour11_dist)
diag(hour11_dist)=large_number

ti = 1
tc = hour11_windows$sign[-1] 
pred_hour11 = test_classification(hour11_dist,train_class=tc,test_indices=ti,15)

new_day_hours[saat ==11, prediction:=pred_hour11$prediction$predicted]

print("hour11 comp")



### PREDICTIONS START

#hour12
####edr distance k=15 w_size =14 

#hour12_windows=windows_all[[13]] #ilk window - latest 
hour12_windows=get_window(comp_dat, 12, 14)

#EDR distance
hour12_dist = TSDatabaseDistances(hour12_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour12_dist=as.matrix(hour12_dist)
diag(hour12_dist)=large_number

ti = 1
tc = hour12_windows$sign[-1] 
pred_hour12 = test_classification(hour12_dist,train_class=tc,test_indices=ti,15)


new_day_hours[saat ==12, prediction:=pred_hour12$prediction$predicted]

print("hour12 comp")


#hour13
####edr distance k=20 w_size =7 

hour13_windows=get_window(comp_dat, 13, 7)

#EDR distance
hour13_dist = TSDatabaseDistances(hour13_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour13_dist=as.matrix(hour13_dist)
diag(hour13_dist)=large_number


ti = 1
tc = hour13_windows$sign[-1] 
pred_hour13 = test_classification(hour13_dist,train_class=tc,test_indices=ti,20)

new_day_hours[saat ==13, prediction:=pred_hour13$prediction$predicted]

print("hour13 comp")


#hour14
####euclidean distance k=20 w_size = 7 

hour14_windows=get_window(comp_dat, 14, 7)

#euclidean distance
hour14_dist = as.matrix(dist(hour14_windows[,-c("sign","date")], upper=TRUE))
diag(hour14_dist)=large_number

ti = 1
tc = hour14_windows$sign[-1] 
pred_hour14 = test_classification(hour14_dist,train_class=tc,test_indices=ti,20)

new_day_hours[saat ==14, prediction:=pred_hour14$prediction$predicted]

print("hour14 comp")


#hour15
####edr distance k=10 w_size = 14

#hour15_windows=windows_all[[16]] #ilk window - latest 
hour15_windows=get_window(comp_dat, 15, 14)

#EDR distance
hour15_dist = TSDatabaseDistances(hour15_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour15_dist=as.matrix(hour15_dist)
diag(hour15_dist)=large_number

ti = 1
tc = hour15_windows$sign[-1] 
pred_hour15 = test_classification(hour15_dist,train_class=tc,test_indices=ti,10)

new_day_hours[saat ==15, prediction:=pred_hour15$prediction$predicted]

print("hour15 comp")


#hour16
####euclidean distance k=25 w_size =7

hour16_windows=get_window(comp_dat, 16, 7)

#euclidean distance
hour16_dist = as.matrix(dist(hour16_windows[,-c("sign","date")], upper=TRUE))
diag(hour16_dist)=large_number

ti = 1
tc = hour16_windows$sign[-1] 
pred_hour16 = test_classification(hour16_dist,train_class=tc,test_indices=ti,25)

new_day_hours[saat ==16, prediction:=pred_hour16$prediction$predicted]

print("hour16 comp")


#hour17

####edr distance k=15 w_size =14 

hour17_windows=get_window(comp_dat, 17, 14)

#EDR distance
hour17_dist = TSDatabaseDistances(hour17_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour17_dist=as.matrix(hour17_dist)
diag(hour17_dist)=large_number

ti = 1
tc = hour17_windows$sign[-1] 
pred_hour17 = test_classification(hour17_dist,train_class=tc,test_indices=ti,15)

new_day_hours[saat ==17, prediction:=pred_hour17$prediction$predicted]

print("hour17 comp")


#hour18
####edr distance k=10 w_size = 14

hour18_windows=get_window(comp_dat, 18, 14)

#EDR distance
hour18_dist = TSDatabaseDistances(hour18_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour18_dist=as.matrix(hour18_dist)
diag(hour18_dist)=large_number

ti = 1
tc = hour18_windows$sign[-1] 
pred_hour18 = test_classification(hour18_dist,train_class=tc,test_indices=ti,10)

new_day_hours[saat ==18, prediction:=pred_hour18$prediction$predicted]

print("hour18 comp")


#hour19
####euclidean distance k=20 w_size =14

#hour19_windows=windows_all[[20]] #ilk window - latest 
hour19_windows=get_window(comp_dat, 19, 14)


#euclidean distance
hour19_dist = as.matrix(dist(hour19_windows[,-c("sign","date")], upper=TRUE))
diag(hour19_dist)=large_number


ti = 1
tc = hour19_windows$sign[-1] 
pred_hour19 = test_classification(hour19_dist,train_class=tc,test_indices=ti,20)

new_day_hours[saat ==19, prediction:=pred_hour19$prediction$predicted]

print("hour19 comp")



#hour20
####edr distance k=15 w_size =14 

hour20_windows=get_window(comp_dat, 20, 14)

#EDR distance
hour20_dist = TSDatabaseDistances(hour20_windows[,-c("sign","date")],distance='edr',epsilon=0.1)
hour20_dist=as.matrix(hour20_dist)
diag(hour20_dist)=large_number

ti = 1
tc = hour20_windows$sign[-1] 
pred_hour20 = test_classification(hour20_dist,train_class=tc,test_indices=ti,15)

new_day_hours[saat ==20, prediction:=pred_hour20$prediction$predicted]

print("hour20 comp")


#hour21
####euclidean distance k=25 w_size =14

hour21_windows=get_window(comp_dat, 21, 14)

#euclidean distance
hour21_dist = as.matrix(dist(hour21_windows[,-c("sign","date")], upper=TRUE))
diag(hour21_dist)=large_number

ti = 1
tc = hour21_windows$sign[-1] 
pred_hour21 = test_classification(hour21_dist,train_class=tc,test_indices=ti,25)


new_day_hours[saat ==21, prediction:=pred_hour21$prediction$predicted]

print("hour21 comp")


#hour22
####euclidean distance k=20 w_size = 7  

hour22_windows=get_window(comp_dat, 22, 7)


#euclidean distance
hour22_dist = as.matrix(dist(hour22_windows[,-c("sign","date")], upper=TRUE))
diag(hour22_dist)=large_number


ti = 1
tc = hour22_windows$sign[-1] 
pred_hour22 = test_classification(hour22_dist,train_class=tc,test_indices=ti,20)

new_day_hours[saat ==22, prediction:=pred_hour22$prediction$predicted]

print("hour22 comp")


#hour23

####euclidean distance k = 20 w_size = 14

hour23_windows=get_window(comp_dat, 23, 14)


#euclidean distance
hour23_dist = as.matrix(dist(hour23_windows[,-c("sign","date")], upper=TRUE))
diag(hour23_dist)=large_number

ti = 1
tc = hour23_windows$sign[-1] 
pred_hour23 = test_classification(hour23_dist,train_class=tc,test_indices=ti,20)

new_day_hours[saat ==23, prediction:=pred_hour23$prediction$predicted]

print("hour23 comp")
#########


new_day_hours$prediction
new_day_hours[new_day_hours$prediction==1,"label"] = "Positive"
new_day_hours[new_day_hours$prediction==-1,"label"] = "Negative"
new_day_hours[new_day_hours$prediction==0,"label"] = "Neutral"


########## SAVE
pin=paste(new_day_hours[saat>=12]$label, collapse=",")

write.csv(pin,"jan21.csv", row.names = FALSE)



