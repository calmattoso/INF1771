# ARFF parser
require(foreign)
require(RWeka)

# Split dataset into training and testing sets
# Modified from: http://gettinggeneticsdone.blogspot.com.br/2011/02/split-data-frame-into-testing-and.html
splitdf <- function(dataframe, seed=NULL, ratio=0.5) 
{
  if (!is.null(seed)) set.seed(seed)
  if (is.nan(ratio)) ratio = 0.5
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index) * ratio))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

# Mode
mode <- function(x)
{
  as.numeric(names(table(x))[which.max(table(x))])
}

# Euclidean distance
euclidean_dist <- function( v1, v2 )
{
  if(length(v1) != length(v2)) return -1
  sqrt(sum((v1 - v2)^2))
}

# Manhattan distance
manhattan_dist <- function( v1, v2 )
{  
  sum(abs(v1 - v2))
}

######################################################################################

####  ####    ####    ####   ####    ####
#### ####     #####   ####   #####   ####
########      ######  ####   ######  ####
########      ############   ############
#### ####     ############   ############
####  ####    ####  ######   ####  ######
####   ####   ####    ####   ####    ####  

knn <- function(train.set, test.set, train.class, k=1, dist=euclidean_dist)
{
  if (is.nan(k)){ k = 1 }
  if(k > nrow(train.set)){ k = nrow(train.set) }
  
  
  test.n_row = nrow(test.set)
  train.n_row = nrow(train.set)
  
  # Array of class predictions for the test set
  test.pred = rep(0, test.n_row)
  
  # Use this array to store the results of distance functions
  #   No need to reinitialize it, as it gets reset for each 
  #   value of i
  d = rep(0, train.n_row) 
  
  # For each test example
  for(i in 1:test.n_row)
  {
    #print(i)
    
    test.row = as.numeric( test.set[i,] )     
    
    # For each training example
    for(j in 1:train.n_row)
    {
      
      train.row = as.numeric( train.set[j,] )  
      
      # Get the distance form test.row vector to train.row vector
      #  and save it for later
      d[j] = dist(test.row , train.row)
    }
    
    # Pick, out of the classes of the **k** nearest neighbors, that which is most common.
    #   In case there's a tie, the first one is chosen. To reduce ocurrence of 
    #   ties a larger value for k coupled with more training data would help
    test.pred[i] = mode( train.class[ order(d)[1:k] ] )
    
  }
  
  #print(test.pred)
  
  test.pred
}

####  ####    ####    ####   ####    ####
#### ####     #####   ####   #####   ####
########      ######  ####   ######  ####
########      ############   ############
#### ####     ############   ############
####  ####    ####  ######   ####  ######
####   ####   ####    ####   ####    ####  

##############################################################################################

classify <- function(dataset, n_runs = 1, k=1, dist=euclidean_dist)
{
  n_folds = n_runs
  
  if (is.nan(n_folds)) n_folds = 1
  if (is.nan(k)) k = 1
  
  best.pred = factor(c(), levels(dataset$class))
  
  # Correctly classified percentage
  correct = 0.0
  
  # Remember this for verification
  best.test.set = c()  
  
  # Sum of prediction percentages
  preds_sum = 0.0
  
  for(i in 1:n_folds)
  {
    print(paste("Run ", i))
    
    # Split dataset intro training and test sets
    split = splitdf(dataset, ratio=0.7)
    train.set = split$trainset
    test.set = split$testset
    
    # Remove classes from both sets
    train.set.no.class = train.set[, -ncol(train.set)]     
    test.set.no.class  = test.set[, -ncol(test.set)]

    #print(train.set$class)
    #print(test.set$class)    
    
    # Predict with simple KNN
    pred = knn(train.set.no.class, test.set.no.class, train.set$class, k, dist)
    #print(pred)
    
    pred = factor( pred , levels(dataset$class))
    #print(pred)
    #print(train.set$class)
    
    # Check for improvement
    new_correct = mean(pred == test.set$class)
    preds_sum = preds_sum + new_correct
    
    # Improve the best found solution
    if( correct < new_correct )
    {
      correct = new_correct
      
      best.pred = pred  
      best.test.set = test.set
      
      # Correct classification rate
      print("Correct classification rate")
      print(mean( best.pred == best.test.set$class ) * 100.0)
      
      # Confusion matrix
      print("Confusion Matrix")
      print(table( best.pred , best.test.set$class ))
    }
  }
  
  # Present the mean percentage os correct classifications
  print(paste("Mean Prediction Percentage: ", preds_sum/n_folds))
  
  best.pred
}


# First load the input
# This file should be opened in weka and resaved
setwd("C:\\Users\\Carlos\\SkyDrive\\PUC\\INF1771 - AI\\INF1771\\T3\\logs")
arff = read.arff("dataset_basic_10.arff")
#head(arff)

# Normalize the dataset using Weka's built-in normalization algorithms
dataset = Normalize( ~., data=arff )

# Display some information about the data
#head( dataset )
#summary( dataset )

# Start testing knn
knn_pred = classify(dataset, n_runs=10, dist=manhattan_dist)







