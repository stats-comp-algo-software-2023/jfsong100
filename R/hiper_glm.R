#' @export
hiper_glm <- function(design, outcome, model = "linear", option=list()){
  supported_model <- c("linear", "logit")
  if(!(model %in% supported_model)){
    stop(sprintf("Model is not available"))
  }
  # TODO: find MLE.
  if(model == "linear"){
    if(is.null(option$mle_solver)==TRUE){
      MLE = MLE_peudo.inverse(design,outcome)
    }
    else if(option$mle_solver=='BFGS'){
      MLE = linear.BFGS(design,outcome,noise_var = 1)$par
    }
  }

  hglm_out <- list(coef=MLE)
  class(hglm_out) <- "hglm"
  return(hglm_out)
}
