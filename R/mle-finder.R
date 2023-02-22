MLE_peudo.inverse <- function(design,outcome){
  return(solve(t(design)%*%design,t(design)%*%outcome))
}

linear.loglik <- function(MLE,design,outcome,noise_var = 1){
  loglik = -sum((outcome - design%*%MLE)^2)/2/noise_var
  loglik
}

linear.gradient <- function(MLE,design,outcome,noise_var = 1){
  gradient = -(t(design) %*% design %*% MLE - t(design)%*% outcome)/noise_var
  gradient
}

linear.BFGS<-function(design,outcome,noise_var = 1){
  initial = rep(0,ncol(design))
  MLE = stats::optim(initial,linear.loglik,linear.gradient,
                     design=design,outcome=outcome,noise_var = 1,
                     control = list(fnscale=-1),
                     method = 'BFGS')
  return(MLE)
}

approx_grad <- function(func, x, design,outcome,dx = .Machine$double.eps^(1/3)) {
  numerical_grad <- rep(0, length(x))
  for (i in 1:length(x)) {
    add_x = x
    add_x[i] = add_x[i]+dx
    minus_x = x
    minus_x[i] = minus_x[i]-dx
    add_term = func(add_x,design,outcome)
    minus_term = func(minus_x,design,outcome)
    numerical_grad[i] = (add_term-minus_term)/2/dx
  }
  return(numerical_grad)
}
