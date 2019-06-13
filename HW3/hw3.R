#GT account name: xtao41

#0a partition data into train and test data 
setwd('C://Users//xin_t//OneDriveGIT//OneDrive - Georgia Institute of Technology//Data Visualization//HW3')
train <- read.csv("mnist_train.csv", header = FALSE)
train<-t(train)
train01<-train[(train[,785]==1 | train[,785]==0),]
train01X<-train01[,c(0:(ncol(train01)-1))]
train01Y<-train01[,ncol(train01)]
train35<-train[(train[,785]==3 | train[,785]==5),]
train35X<-train35[,c(0:(ncol(train35)-1))]
train35Y<-train35[,ncol(train35)]

test<- read.csv("mnist_test.csv", header = FALSE)
test<-t(test)
test01<-test[(test[,785]==1 | test[,785]==0),]
test01X<-test01[,c(0:(ncol(test01)-1))]
test01Y<-test01[,ncol(test01)]
test35<-test[(test[,785]==3 | test[,785]==5),]
test35X<-test35[,c(0:(ncol(test35)-1))]
test35Y<-test35[,ncol(test35)]


train01Y[train01Y==0]<--1
test01Y[test01Y==0]<--1

train35Y[train35Y==3]<--1
test35Y[test35Y==3]<--1
train35Y[train35Y==5]<-1
test35Y[test35Y==5]<-1

#b sample image from each class
train0<-train01X[which.max(train01Y==0),]
sample0<-matrix(train0,nrow=28,ncol=28)
image(sample0,col=gray.colors(256))

train1<-train01X[which.max(train01Y==1),]
sample1<-matrix(train1,nrow=28,ncol=28)
image(sample1,col=gray.colors(256))

train3<-train35X[which.max(train35Y==3),]
sample3<-matrix(train3,nrow=28,ncol=28)
image(sample3,col=gray.colors(256))

train5<-train35X[which.max(train35Y==5),]
sample5<-matrix(train5,nrow=28,ncol=28)
image(sample5,col=gray.colors(256))

#2 Implementation
# https://www.youtube.com/watch?v=qSTHZvN8hzs&feature=youtu.be
# X space d-dimensional space (x1,x2,...xd), add a constant x0 serve as the offset, so X->(x0,x1,x2,....xd). 
# Similarly theta has a constant theta0-> (theta0,theta1,..., theta d)
#trainX and trainY are matrices, trainX has first col all 1
LRGD=function(trainX, trainY){
  X=trainX
  Y=trainY
  # initialize theta with random values between 0 and 1, the constant set to 0
  theta_init <- round(runif(ncol(X), 0.0, 1.0), digits=2)
  theta<-as.matrix(theta_init)
  # set learning parameter and threshold
  alpha<- 0.001  #http://pingax.com/linear-regression-with-r-step-by-step-implementation-part-2/
  TH<-10/sqrt(nrow(X))   
  
  #vectorization: the formula for computing theta can be vectorized, i.e. use matrix algebra. 
  # define I=<theta, X>, n*1 matrix; LF=Y/(1+exp(-Y*Z)), n*1 matrix; 
  # then the update theta<-theta-alpha*t(X) dot LF
  I<-X %*% theta
  LF<-Y/(1+exp(-Y*I))
  G<-alpha*t(X)%*%LF
  while (any(abs(G) > TH)){   #keep update until the updates is two small- meaning the changes is neglectable
    I<- X %*% theta
    LF<-Y/(1+exp(-Y*I))
    G<-alpha*t(X)%*%LF
    theta<-theta-G
  }
  return(theta)
}


#3a Train and test on 0_1 and 3_5 dataset


#Predict fucntion
predict=function(data, theta){
  return(-sign(data %*% theta))
}

#  accuracy fucntion
get_accuracy=function (trainX,trainY,testX,testY){
  trainX<-as.matrix(trainX)
  testX<- as.matrix(testX)
  
  # add constant to vector X 
  trainC<-cbind(matrix(1,nrow=nrow(trainX)),trainX)
  testC<-cbind(matrix(1,nrow=nrow(testX)),testX)
  trainY<-as.matrix(trainY)
  testY<-as.matrix(testY)
  
  theta<-LRGD(trainC,trainY)
  
  predict_train<- predict(trainC, theta)
  predict_test<- predict(testC,theta)
  
  #training accuracy
  train_accuracy<- mean(predict_train == trainY)
  test_accuracy<-mean(predict_test == testY)
  
  return(list(train_accuracy,test_accuracy))
}


# get 0_1 accuracy label 0 as -1, label 1 as 1

train01_accuracy<-get_accuracy(train01X,train01Y,test01X,test01Y)[1]
test01_accuracy<-get_accuracy(train01X,train01Y,test01X,test01Y)[2]

sprintf("accuracy of train_0_1 is  %s", train01_accuracy)
sprintf("accuracy of test_0_1 is  %s", test01_accuracy)


train35_accuracy<-get_accuracy(train35X,train35Y,test35X,test35Y)[1]
test35_accuracy<-get_accuracy(train35X,train35Y,test35X,test35Y)[2]

sprintf("accuracy of train_3_5 is  %s", train35_accuracy)
sprintf("accuracy of test_3_5 is  %s", test35_accuracy)


#3b. average over 10 times: Used BATCH GRADIENT DESCENT. Train on 10 random 80% divisions of training data and take average.

get_avg_accuracy=function(Xtrain,Ytrain,testX,testY,subset){
  sum_train=0.0
  sum_test=0.0
  data_train<-cbind(Xtrain,Ytrain)
  
  for(i in 1:10){
    #shuffle data
    train <- data_train[sample(1:nrow(data_train),round(subset*nrow(data_train))),]
    trainX<-train[,c(0:(ncol(train)-1))]
    trainY<-train[,ncol(train)]


    accuracy<-get_accuracy(trainX,trainY,testX,testY)
    train_accuracy<-accuracy[1]
    test_accuracy<-accuracy[2]
    #print(train_accuracy)
    #print(test_accuracy)

    sum_train<-sum_train+as.numeric(train_accuracy)
    sum_test<-sum_test+as.numeric(test_accuracy)
 
  }
  
  avg_accuracy_train= sum_train/10
  avg_accuracy_test= sum_test/10
  return(list(avg_accuracy_train,avg_accuracy_test))

}
result01=get_avg_accuracy(train01X,train01Y,test01X,test01Y,0.8)
result35=get_avg_accuracy(train35X,train35Y,test35X,test35Y,0.8)
avg01_accuracy_train<-result01[1]
avg01_accuracy_test<-result01[2]
avg35_accuracy_train<-result35[1]
avg35_accuracy_test<-result35[2]

sprintf("average accuracy of train_0_1 is %s", avg01_accuracy_train)
sprintf("average accuracy of test_0_1 is %s", avg01_accuracy_test)
sprintf("average accuracy of train_3_5 is %s", avg35_accuracy_train)
sprintf("average accuracy of test_3_5 is %s", avg35_accuracy_test)



# 4a Experiment with different initializations of theta.Theta init changed to each of the commented line
LRGD=function(trainX, trainY){
  X=trainX
  Y=trainY
  # initialize theta with random values between 0 and 1, the constant set to 0
  theta_init <- round(runif(ncol(X), 0.0, 1.0), digits=2)
  #theta_init <- round(runif(ncol(X), -1.0, 0), digits=2)
  #theta_init <- round(runif(ncol(X), 5, 10), digits=2)
  #theta_init <- round(runif(ncol(X), -10.0, -5.0), digits=2)
  #theta_init <- matrix(0,nrow=ncol(X))
  theta<-as.matrix(theta_init)
  # set learning parameter and threshold
  alpha<- 0.001  #http://pingax.com/linear-regression-with-r-step-by-step-implementation-part-2/
  TH<-10/sqrt(nrow(X))   
  #TH<-1/sqrt(nrow(X)) 
  #TH<-100/sqrt(nrow(X))
  #vectorization: the formula for computing theta can be vectorized, i.e. use matrix algebra. 
  # define I=<theta, X>, n*1 matrix; LF=Y/(1+exp(-Y*Z)), n*1 matrix; 
  # then the update theta<-theta-alpha*t(X) dot LF
  I<-X %*% theta
  LF<-Y/(1+exp(-Y*I))
  G<-alpha*t(X)%*%LF
  while (any(abs(G) > TH)){   #keep update until the updates is two small- meaning the changes is neglectable
    I<- X %*% theta
    LF<-Y/(1+exp(-Y*I))
    G<-alpha*t(X)%*%LF
    theta<-theta-G
  }
  return(theta)
}
result=get_avg_accuracy(train35X,train35Y,test35X,test35Y,0.8)
avg35_accuracy_train<-result[1]
avg35_accuracy_test<-result[2]  
sprintf("average accuracy of train_3_5 is %s", avg35_accuracy_train)
sprintf("average accuracy of test_3_5 is %s", avg35_accuracy_test)

#4b,  fix iternation counts. Here change N from 10 to 100 and see the difference 

LRGD=function(trainX, trainY){
  X=trainX
  Y=trainY
  # initialize theta with random values between 0 and 1, the constant set to 0
  theta_init <- round(runif(ncol(X), 0.0, 1.0), digits=2)
  theta<-as.matrix(theta_init)
  # set learning parameter and threshold
  alpha<- 0.001  #http://pingax.com/linear-regression-with-r-step-by-step-implementation-part-2/
  TH<-10/sqrt(nrow(X))   
  #vectorization: the formula for computing theta can be vectorized, i.e. use matrix algebra. 
  # define I=<theta, X>, n*1 matrix; LF=Y/(1+exp(-Y*Z)), n*1 matrix; 
  # then the update theta<-theta-alpha*t(X) dot LF
  I<-X %*% theta
  LF<-Y/(1+exp(-Y*I))
  G<-alpha*t(X)%*%LF
  N=0
  while (N<100){   #keep update until the updates is two small- meaning the changes is neglectable
    I<- X %*% theta
    LF<-Y/(1+exp(-Y*I))
    G<-alpha*t(X)%*%LF
    theta<-theta-G
    N=N+1
  }
  return(theta)
}

#5. Learning curve
size<-seq(0.05,1.0, length.out=20)

#a. accuracy vs trainign set size
accuracy01_train<-vector(length=20,mode="numeric")
accuracy01_test<-vector(length=20,mode="numeric")
accuracy35_train<-vector(length=20,mode="numeric")
accuracy35_test<-vector(length=20,mode="numeric")

for (i in 1:20){
  result01=get_avg_accuracy(train01X,train01Y,test01X,test01Y,size[i])
  result35=get_avg_accuracy(train35X,train35Y,test35X,test35Y,size[i])
  accuracy01_train[i]<-result01[1]
  accuracy01_test[i]<-result01[2]
  accuracy35_train[i]<-result35[1]
  accuracy35_test[i]<-result35[2]
}

#plot 0_1 data set
matplot(size,cbind(accuracy01_train, accuracy01_test), type="b",pch=19, col=c("red", "blue"), xlab="training set size", ylab="Accuracy")
legend("bottomright", legend=c("train", "test"), pch=19, col=c("red", "blue"))
title ("0_1 Train and Test Accuracies Over different Training Set size")
# plot 3_5 data set
matplot(size,cbind(accuracy35_train, accuracy35_test), type="b",pch=19, col=c("red", "blue"), xlab="training set size", ylab="Accuracy")
legend("bottomright", legend=c("train", "test"), pch=19, col=c("red", "blue"))
title ("3_5 Train and Test Accuracies Over different Training Set size")


#b. negative log likelihood vs training set size
Neg_log_f=function(dataX, dataY, theta){
  I<-dataX %*% theta
  Neg_log<-log(1+exp(dataY *I))
  return (sum(Neg_log))
}

get_avg_loss=function(Xtrain,Ytrain,testX,testY,subset){
  sum_train=0.0
  sum_test=0.0
  data_train<-cbind(Xtrain,Ytrain)
  
  for(i in 1:10){
    #shuffle data
    train <- data_train[sample(1:nrow(data_train),round(subset*nrow(data_train))),]
    trainX<-train[,c(0:(ncol(train)-1))]
    trainY<-train[,ncol(train)]
    
    trainX<-as.matrix(trainX)
    testX<- as.matrix(testX)
    
    # add constant to vector X 
    trainC<-cbind(matrix(1,nrow=nrow(trainX)),trainX)
    testC<-cbind(matrix(1,nrow=nrow(testX)),testX)
    trainY<-as.matrix(trainY)
    testY<-as.matrix(testY)
    theta<-LRGD(trainC, trainY)
    loss_train<-Neg_log_f(trainC,trainY,theta)
    loss_test<-Neg_log_f(testC,testY,theta)
    print(loss_train)
    print(loss_test)
    
    sum_train<-sum_train+as.numeric(loss_train)
    sum_test<-sum_test+as.numeric(loss_test)
    
  }
  
  avg_loss_train= sum_train/10
  avg_loss_test= sum_test/10
  return(list(avg_loss_train,avg_loss_test))
  
}

  

loss01_train<-vector(length=20,mode="numeric")
loss01_test<-vector(length=20,mode="numeric")
loss35_train<-vector(length=20,mode="numeric")
loss35_test<-vector(length=20,mode="numeric")

for (i in 1:20){
  result01=get_avg_loss(train01X,train01Y,test01X,test01Y,size[i])
  result35=get_avg_loss(train35X,train35Y,test35X,test35Y,size[i])
  loss01_train[i]<-result01[1]
  loss01_test[i]<-result01[2]
  loss35_train[i]<-result35[1]
  loss35_test[i]<-result35[2]
  
}


#plot 0_1 data set
matplot(size,cbind(loss01_train, loss01_test), type="b",pch=19, col=c("red", "blue"), xlab="training set size", ylab="Negative log likelihood")
legend("topright", legend=c("train", "test"), pch=19, col=c("red", "blue"))
title ("0_1 Train and Test Negative log likelihood Over different Training Set size")
# plot 3_5 data set
matplot(size,cbind(loss35_train, loss35_test), type="b",pch=19, col=c("red", "blue"), xlab="training set size", ylab="Negative log likelihood",ylim=c(0,10000))
legend("topright", legend=c("train", "test"), pch=19, col=c("red", "blue"))
title ("3_5 Train and Test Negative log likelihood Over different Training Set size")


