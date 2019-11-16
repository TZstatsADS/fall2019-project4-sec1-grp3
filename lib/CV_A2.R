cv.function <- function(data,dat_train, K,epochs,lrate, f, lambda_p, lambda_q){
  ### Input:
  ### - train data frame
  ### - K: a number stands for K-fold CV
  ### - tuning parameters 
  
  n <- dim(dat_train)[1]
  n.fold <- round(n/K, 0)
  set.seed(0)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  train_rmse <- matrix(NA, ncol = 10,nrow = K)
  test_rmse <- matrix(NA, ncol = 10, nrow = K)
  
  for (i in 1:K){
    train.data <- dat_train[s != i,]
    test.data <- dat_train[s == i,]
    
    result <- gradesc(f = f, lambda_p =lambda_p,lambda_q=lambda_q,
                      lrate = lrate, epochs = epochs, stopping.deriv = 0.01, data=data,
                      dat_train = dat_train, train = train.data, test = test.data)
    
    train_rmse[i,] <-  result$RMSE[,1]
    test_rmse[i,] <-   result$RMSE[,2]
    
  }		
  return(list(mean_best_train_rmse = mean(apply(train_rmse, 1, min)), mean_best_test_rmse = mean(apply(test_rmse, 1, min)),
              sd_best_train_rmse = sd(apply(train_rmse, 1, min)), sd_best_test_rmse = sd(apply(test_rmse, 1, min))))
}