#Define a function to calculate RMSE

#Summation from 1 to n of 
#(sqrt((observe_i-estimated_i)^2/n)

RMSE <- function(rating, est_rating){
  #(square error = (observe - estimated)^2)
  sqr_err <- function(obs){
    sqr_error <- (obs[3] - est_rating[as.character(obs[2]), as.character(obs[1])])^2
    return(sqr_error)
  }
  # to all the ratings by row, run the function sqr_err
  # take the mean then sqrt the result
  return(sqrt(mean(apply(rating, 1, sqr_err))))  
}

#Stochastic Gradient Descent
# a function returns a list containing factorized matrices p and q, training and testing RMSEs.
gradesc <- function(f = 10, 
                    lambda = 0.3,lrate = 0.01, max.iter, stopping.deriv = 0.01,
                    data, train, test){
  set.seed(0)
  
  #random assign value to matrix p and q
  #p is a matrix that generates f*U (f*Number of Users) numbers that are between -1 and 1 with U columns
  #p has U columns and f rows (column names are numbers)
  p <- matrix(runif(f*U, -1, 1), ncol = U) 
  colnames(p) <- as.character(1:U)
  
  #q is a matrix that generates f*I (f*Number of Movies) numbers that are between -1 and 1 with I columns
  #q has I columns and f rows (column names are movieids?)
  q <- matrix(runif(f*I, -1, 1), ncol = I)
  colnames(q) <- levels(as.factor(data$movieId))
  
  #Make matrix of u x i fill in 1 if user u did rate movie i otherwise put 0
  I_ui
  
  train_RMSE <- c()
  test_RMSE <- c()
  
  
  for(l in 1:max.iter){
    sample_idx <- sample(1:nrow(train), nrow(train))
    #loop through each training case and perform update
    

    for(all movies m1){
      
      i <- as.character(train[s,2])
      
      for(all users u1){
        #Get the users, movie ids, and the ratings
        u <- as.character(train[s,1])
        r_ui <- train[s,3]
        
        # rating - Transpose column i of q maxtrix multipled by column u of p
        # rating(1x1) - the multiplication of a user and some movie (should be 1xf * fx1 matrix results in 1x1 matrix)
        # e_ui = 1x1
        
        e_ui <- r_ui - t(q[,i]) %*% p[,u]
        
        # Ans_Ie disappears when looping, how are we supposed to use this data?
        Ans_Ie =I_ui[u,m] * e_ui^2 


      }
       
    }
    
    #What is the first standard deviation using
    std_q = stdev(, na.rm, unbiased)/2*stdev(q, na.rm, unbiased)
    std_p = stdev(, na.rm, unbiased)/2*stdev(q, na.rm, unbiased)
    
    #WHAT IS ||q_i||^2? 
    #we are using sum(q^2) for now since we saw examples online that use that.
    E = Ans_Ie + std_q*sum(q^2)+ std_p*sum(p^2)
    
    
    #print the values of training and testing RMSE
    if (l %% 10 == 0){
      cat("epoch:", l, "\t")
      est_rating <- t(q) %*% p
      rownames(est_rating) <- levels(as.factor(data$movieId))
      
      train_RMSE_cur <- RMSE(train, est_rating)
      cat("training RMSE:", train_RMSE_cur, "\t")
      train_RMSE <- c(train_RMSE, train_RMSE_cur)
      
      test_RMSE_cur <- RMSE(test, est_rating)
      cat("test RMSE:",test_RMSE_cur, "\n")
      test_RMSE <- c(test_RMSE, test_RMSE_cur)
    } 
  }
  
  return(list(p = p, q = q, train_RMSE = train_RMSE, test_RMSE = test_RMSE))
}
