#Define a function to calculate RMSE
RMSE <- function(rating, est_rating){
  sqr_err <- function(obs){
    sqr_error <- (obs[3] - est_rating[as.character(obs[2]), as.character(obs[1])])^2
    return(sqr_error)
  }
  return(sqrt(mean(apply(rating, 1, sqr_err))))  
}

gradesc <- function(f = 10, 
                    lrate = 0.01, lambda_p, lambda_q, epochs, stopping.deriv = 0.01,
                    data, train, test){
  set.seed(0)
  #random assign value to matrix p and q
  U <- length(unique(data$userId))
  I <- length(unique(data$movieId))
  
  p <- matrix(runif(f*U, -1, 1), ncol = U) 
  colnames(p) <- as.character(1:U)
  
  q <- matrix(runif(f*I, -1, 1), ncol = I)
  colnames(q) <- levels(as.factor(data$movieId))
  
  train_RMSE <- c()
  test_RMSE <- c()
  
  for(l in 1:epochs){
    
    grad_p <- matrix(0, ncol=U, nrow=f)
    colnames(grad_p) <- as.character(1:U)
    grad_q <- matrix(0, ncol=I, nrow=f)
    colnames(grad_q) <- levels(as.factor(data$movieId))
    RMSE <- matrix(0, nrow=epochs, ncol=2)
    est_rating <- vector(mode="list")
    
    for (i in 1:nrow(data)){
      user <- as.character(train[i,1])
      movie <- as.character(train[i,2])
      error <- train[i,3]-t(p[,user])%*%q[,movie]
      grad_p[,user] <- grad_p[,user]+error*q[,movie]
      grad_q[,movie] <- grad_q[,movie]+error*p[,user]
    }
    
    for(i in 1:U){
      grad_p[,as.character(i)] <- grad_p[,as.character(i)]-lambda_p*p[,as.character(i)]
    }
    for(i in 1:I){
      grad_q[,as.character(i)] <- grad_q[,as.character(i)]-lambda_q*q[,as.character(i)]
    }
    
    p <- p + lrate*grad_p
    q <- q + lrate*grad_q
    
    # calculate trainng and testng rmse after each epoch
    est_rating[[l]] <- t(q) %*% p
    rownames(est_rating) <- levels(as.factor(data$movieId))
    RMSE[l,1] <- RMSE(train, est_rating[[l]])
    RMSE[l,2] <- RMSE(test, est_rating[[l]])  
  
    # automatically change learning rate to avoid diverge
    
    best_RMSE <- min(RMSE)
   
  }
  
  return(list(p = p, q = q, RMSE=RMSE, est_rating=est_rating))
}