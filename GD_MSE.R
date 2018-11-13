#function for gradient descent for mean squared error
#X -> predictor
#Y -> response
gradientDescentMSE <- function(X,Y, n, learning_rate=0.01, epsilon=0.00001, iterations=10000)
{
  #column 1 is added for intercept
  X <- cbind(1, X) # 50*2
  
  #creating a weights matrix(theta, weights etc), in this case theta0 for intercept and theta1 for X predictor
  theta <- matrix(0, nrow=2, ncol=1) #2*1
  
  i <- 1
  mse_loss <- 0
  
  #test_mtrix to hold gradient value to see how it is decreasing
  test_mtrix <<- matrix(0, nrow = 10000, ncol = 2)
  while( i < iterations)
  {
    
    #calculating the mse loss sum of (y-y_cap)^2
    mse_new_loss <- sum((Y - X%*%theta)^2)
    
    #gradient is calculated be derivative of squared error
    gradientDescent <- -(2/n)*t(X)%*%(Y - X%*%theta)
    
    #new theta value
    theta <- theta - learning_rate*gradientDescent
    if(abs(mse_new_loss - mse_loss) < epsilon)
    {
      break
    }
    mse_loss <- mse_new_loss
    test_mtrix[i,] <<- gradientDescent
    i <- i+1
  }
  return(theta)
}

#randomly generated 50 values between -2 and 2
x<-runif(50, min = -2, max = 2)
x<- matrix(x)

#normally distributed errors with mean 0 and sd=2
error <- rnorm(50, 0, 2)
error<- matrix(error)

#our linear equation. We will be predicting 2 and 3
#first find the original y values
y <- 2+3*x + error

#call gradient function to estimate y values started from theta=0
thetas <- gradientDescentMSE(x,y,50)
print(thetas)

#to plot gradient for both theta0 and theta1
grad_mat <- cbind(test_mtrix, c(1:10000))
grad_mat <- as.data.frame(grad_mat)
grad_mat <- grad_mat[grad_mat$V1 != 0,]
ggplot(grad_mat, aes(x=V3, y=V1)) + geom_line()
ggplot(grad_mat, aes(x=V3, y=V2)) + geom_line()
