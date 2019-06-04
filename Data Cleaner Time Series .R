## Data cleaner for time series data 
## For data with no unique groups

TimeSeriesData.Clean<- function(data, Method = "Remove"){
  data_Output <- data
 
  
  ## check is group variables are misssing data then force method to Remove 
  ## grab all columns wi
  if(Method == "Remove"){
    data_Output <- na.omit(data) ## because I was too lazy to change to approp name data_Ouput
  }else if(Method == "Mean"){
    for(i in 1:ncol(data)){
      data_Output[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
    }
    
  } else if (Method == "Median"){
    for (i in 1:ncol(data)) {
      data_Output[is.na(data[,i]), i] <- median(data[,i], na.rm = TRUE) 
    }
    
  } else if(Method == "Max" ){
    for(i in 1:ncol(data)){
      data_Output[is.na(data[,i]), i] <- max(data[,i], na.rm = TRUE)
    }
    
  } else if(Method == "Min" ){
    for(i in 1:ncol(data)){
      data_Output[is.na(data[,i]), i] <- min(data[,i], na.rm = TRUE)
    }
    
  } else if(Method == "All" ){
    
    for(i in 1:ncol(data)){
      
      data_Output[is.na(data[,i]), i] <- mean( c(min(data[,i], na.rm = TRUE) , 
                                                max(data[,i], na.rm = TRUE) , 
                                                median(data[,i], na.rm = TRUE), 
                                                mean(data[,i], na.rm = TRUE) ) )
      
    }
    
  }

 return(data_Output) 
}

data_test <- matrix( sample(c(rnorm(1,-1,1), NA, NA), size = 400, prob = c(0.5,0.25,0.25), replace =  T), ncol = 4)
head(data_test)
data_check <- TimeSeriesData.Clean(data_test, 'All')
head(data_check)

data_Output<- data_test
for(i in 1:ncol(data_test)){
  data_Output[is.na(data_test[,i]), i] <- mean(data_test[,i], na.rm = TRUE)
}

data_Output[is.na(data_test[,1 ]), 1] <- mean(data_test[,1], na.rm = TRUE)

mu_vect<- colMeans((data_test[,]), na.rm = T)


### Option to removes outliers 
dist <- 4

##get distance from individual means in columns 
## get all deviations and see how minimum deviation works
## if minim is not within mu +/- 4*std then drop that point
means_vect <- colMeans(data_Output, na.rm = T)
cov_mat <- 
  for(i in 1:length(means_vect)){
    for(j in 1:ncol(data_Output)){
      
    }
  }


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### Now for data cleaner taking group means into account 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
## Assume group data is first column
ts_Cleaner.W_Group <- function(data, Method = "Remove"){
  data_Out <- data
  g_all <- data[,1] 
  
  ## check if any group is has missing value 
  g_check <- unique(data[,1])
  if(any(is.na(data[,1])) & (Method != "Remove") ){
    stop("A group point is missing. Must use remove method for integrity of test")
  } 
  #method must be remove if group data is missing to preserve test
  ## can make alternate algo later with bayesian inf to fill out group missing given values
  
  if(Method == "Remove"){
    data_Out <- na.omit(data)
    
  } else if(Method == "Mean") {
    ## get groups in orginal order 
    #g_all <- data[,1] 
    for(g in g_all){
      for (i in 2:ncol(data)) {
        data_Out[is.na(data[,i]) & (g_all == g), i] <- mean(data[g_all == g,i], na.rm = TRUE) 
      }
    }
    
  } else if(Method == "Median"){
    
    #g_all <- data[,1] 
    for(g in g_all){
      for (i in 2:ncol(data)) {
        data_Out[is.na(data[,i]) & (g_all == g), i] <- median(data[g_all == g,i], na.rm = TRUE) 
      }
    }
  } else if(Method == "Max"){
    #g_all <- data[,1] 
    for(g in g_all){
      for (i in 2:ncol(data)) {
        data_Out[is.na(data[,i]) & (g_all == g), i] <- max(data[g_all == g,i], na.rm = TRUE) 
      }
    }
  } else if(Method == "Min"){
    
    #g_all <- data[,1] 
    for(g in g_all){
      for (i in 2:ncol(data)) {
        data_Out[is.na(data[,i]) & (g_all == g), i] <- min(data[g_all == g,i], na.rm = TRUE) 
      }
    }
  } else if(Method == "All"){
    
    #g_all <- data[,1] 
    for(g in g_all){
      for (i in 2:ncol(data)) {
        data_Out[is.na(data[,i]) & (g_all == g), i] <- mean( c(min(data[g_all == g,i], na.rm = TRUE) , 
                                                               max(data[g_all == g,i], na.rm = TRUE) , 
                                                               median(data[g_all == g,i], na.rm = TRUE), 
                                                               mean(data[g_all == g,i], na.rm = TRUE) ) )
      }
    }
  }
  
  
  return(data_Out)
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

## testing
n = 25
#data_test <- matrix( sample(c(rnorm(1,-1,1), NA, NA), size = 100, prob = c(0.5,0.25,0.25), replace =  T ), ncol = 3)
g_roups <- sample(c('a','b','c', 'd'), prob = c(0.25,0.25,0.25, 0.25), size = 100, replace = T)
g_roups <- as.factor(g_roups)

group_abcd <- rep(NA, 100)
group_abcd[g_roups == 'a'] <- rnorm(sum(g_roups == 'a'), 0,1)
group_abcd[g_roups == 'b'] <- rnorm(sum(g_roups == 'b'), 50, 1)
group_abcd[g_roups == 'c'] <- rnorm(sum(g_roups == 'c'), 10, 1)
group_abcd[g_roups == 'd'] <- rnorm(sum(g_roups == 'd'), 100,1)

group_abcd1 <- rep(NA, 100)
group_abcd1[g_roups == 'a'] <- rnorm(sum(g_roups == 'a'), 0,1)
group_abcd1[g_roups == 'b'] <- rnorm(sum(g_roups == 'b'), 50, 1)
group_abcd1[g_roups == 'c'] <- rnorm(sum(g_roups == 'c'), 10, 1)
group_abcd1[g_roups == 'd'] <- rnorm(sum(g_roups == 'd'), 100,1)

group_abcd2 <- rep(NA, 100)
group_abcd2[g_roups == 'a'] <- rnorm(sum(g_roups == 'a'), 0,1)
group_abcd2[g_roups == 'b'] <- rnorm(sum(g_roups == 'b'), 50, 1)
group_abcd2[g_roups == 'c'] <- rnorm(sum(g_roups == 'c'), 10, 1)
group_abcd2[g_roups == 'd'] <- rnorm(sum(g_roups == 'd'), 100,1)

data_w_group <- data.frame((g_roups), group_abcd, group_abcd1, group_abcd2)
str(data_w_group)

Na_ids <- sample(c(1:100), size = 30)
data_w_group[Na_ids, -1] <- NA
  

#test_data_1 <- cbind(g_roups, data_test)
#colnames(test_data_1) <- c('groups','var1', 'var2', 'var3')
#head(test_data_1)
#colnames(test_data_1)[2:4]
#str(test_data_1)
#data <- as.data.frame(test_data_1)
#data$groups <- as.factor(data$groups)
#str(data)

ts_Cleaner.W_Group(data_w_group, Method = "All" )

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
##### Now with option to clean outliers 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

#Simplier case first, simply removing the row of outlying data for group data

Data.Clean.w.Out_Handle<- function(data, Method = "Remove", OutlierHandle = "Remove", std.away = 4, out.wrt.group = "No"){
  
  data_Out <- data
  g_all <- data[,1] 
  
  ## check if any group is has missing value 
  g_check <- unique(data[,1])
  if(any(is.na(data[,1])) & (Method != "Remove") ){
    stop("A group point is missing. Must use remove method for integrity of test")
  } 
  #method must be remove if group data is missing to preserve test
  ## can make alternate algo later with bayesian inf to fill out group missing given values
  
  if(Method == "Remove"){
    data_Out <- na.omit(data)
    
  } else if(Method == "Mean") {
    ## get groups in orginal order 
    #g_all <- data[,1] 
    for(g in g_all){
      for (i in 2:ncol(data)) {
        data_Out[is.na(data[,i]) & (g_all == g), i] <- mean(data[g_all == g,i], na.rm = TRUE) 
      }
    }
    
  } else if(Method == "Median"){
    
    #g_all <- data[,1] 
    for(g in g_all){
      for (i in 2:ncol(data)) {
        data_Out[is.na(data[,i]) & (g_all == g), i] <- median(data[g_all == g,i], na.rm = TRUE) 
      }
    }
  } else if(Method == "Max"){
    #g_all <- data[,1] 
    for(g in g_all){
      for (i in 2:ncol(data)) {
        data_Out[is.na(data[,i]) & (g_all == g), i] <- max(data[g_all == g,i], na.rm = TRUE) 
      }
    }
  } else if(Method == "Min"){
    
    #g_all <- data[,1] 
    for(g in g_all){
      for (i in 2:ncol(data)) {
        data_Out[is.na(data[,i]) & (g_all == g), i] <- min(data[g_all == g,i], na.rm = TRUE) 
      }
    }
  } else if(Method == "All"){
    
    #g_all <- data[,1] 
    for(g in g_all){
      for (i in 2:ncol(data)) {
        data_Out[is.na(data[,i]) & (g_all == g), i] <- mean( c(min(data[g_all == g,i], na.rm = TRUE) , 
                                                               max(data[g_all == g,i], na.rm = TRUE) , 
                                                               median(data[g_all == g,i], na.rm = TRUE), 
                                                               mean(data[g_all == g,i], na.rm = TRUE) ) )
      }
    }
  }
  
  ## now removing outlier
  row_collect <- NULL
  if(OutlierHandle == "NONE"){
    data_Out <- data_Out
  } else if(OutlierHandle == "Remove"){
    if(out.wrt.group == "No"){
      
      for(j in 2:ncol(data_Out)){
        meanj <- mean(data_Out[,j])
        stdj <- sd(data_Out[,j])
        intj <- abs(meanj - std.away*stdj) ### outlying therhod
        
        for(i in 1:length(data_Out[,j])){ ## now to remove outlying rows
        if(abs(data_Out[i,j]) > intj){ row_collect <- c(row_collect, i) } 
        }
      }
    }
  }
  
  row_collect <- unique(row_collect)
  data_Out[row_collect,] <- NA
  data_Out <- na.omit(data_Out)
  
  
  return(data_Out)
}

################################################
#### Check for independent outlier function 
## test indep outlier function 
################################################
Outlier.remove <- function(data, std.away = 4){
  data_Out <- data
  row_collect <- NULL
      for(j in 2:ncol(data_Out)){
        meanj <- mean(data_Out[,j])
        stdj <- sd(data_Out[,j])
        intj <- abs(meanj - std.away*stdj) ### outlying therhod
        
        for(i in 1:length(data_Out[,j])){ ## now to remove outlying rows
          if(abs(data_Out[i,j]) > intj){ row_collect<- c(row_collect, i)} 
        }
      }

  row_collect <- unique(row_collect)
  
  #data_Out[row_collect, ] <- NULL
  
  return(list(data_result = data_Out, row_ind = row_collect))
}

row_rem <- Outlier.remove(data_w_group, std.away = 3)$row_ind
data_w_group[row_rem,] <- NA
data_w_group <- na.omit(data_w_group)

#######################################
#Test main function 
####################################
Data.Clean.w.Out_Handle(data = data_w_group)

##### check overall cleaner function 


### not, these variables may be correlated, so an outlying in one single dimension...
##.. may not be an outlying in the overal multidimensional dimesional view 
# what do we call an outlying in this case 


