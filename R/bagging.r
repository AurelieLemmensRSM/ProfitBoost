#===============#
# Bagging Trees #
#===============#

bagging = function(data.train, data.test, stop = "automatic", iterations = 1000, seed.value = 1534)
{
  library(tree)
  likelihood <-function(y,yprob)  #computes the lik based on the values and predicted probabilities
  {
    meanloglik<-mean(log(as.matrix((y*yprob)+(1-y)*(1-yprob))))
    meanlik<-mean((as.matrix((y*yprob)+(1-y)*(1-yprob))))
  }
  
  y=data.train[,ncol(data.train)]              #{0,1} dependent variable
  ytest=data.test[,ncol(data.test)]
  ntrain=dim(data.train)[1]
  ntest=dim(data.test)[1]
  ypredtrain=ypredtest=c() 
  lik=array(NA,c(1,iterations))
  
  for (b in 1:iterations)
  {
    print(b)
    I.D.b <-sample(seq(1,ntrain),replace=TRUE)
    data.bagging <-data.train[I.D.b,]
    knodetree<- tree(factor(y)~.,as.data.frame(data.bagging))#,
                     #control = tree.control(ntrain, minsize = 10,mindev = 0.005))
    
    #Limit the number of terminal nodes to maximum 8
    if (length(levels(factor(knodetree$where)))>5) knodetree=prune.tree(knodetree,best=5)
    
    ypredtrain = cbind(ypredtrain,predict(knodetree,as.data.frame(data.train),'vector')[,2])
    ypredtest  = cbind(ypredtest ,predict(knodetree,as.data.frame(data.test), 'vector')[,2])
    
    ypredtrain.average = apply(ypredtrain,1,mean)
    ypredtest.average  = apply(ypredtest,1,mean)
    
    lik[b]=likelihood(y=y,yprob=ypredtrain.average) 
      
    if (stop == "automatic") 
      {if (b > 1) 
        { if (lik[b]-lik[b-1]<.0000001) break}
      }
  }

  list(predictions.train = ypredtrain.average, predictions.test = ypredtest.average,b,lik)
}
  
