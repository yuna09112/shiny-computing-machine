# Problem1 - (b)
#' @title Predict Method for KRR Model
#' @description Computes KRR predictions for new data using the fitted model object.
#' Prediction is f(x*) = K_new * alpha
#' @param model An object of class "krr", returned by krr(). [cite: 448]
#' @param X_new A matrix of new features for prediction. [cite: 448]
#' @param ... Additional arguments (not used by this method).
#' @return A vector of predicted response values. [cite: 451]
#' @export
predict.krr <- function(model, X_new) {
  # Calculate Kernel Matrix between new data and training data
  K_new <- gaussian_kernel(X_new, model$X, model$rho) [cite: 449]
  
  # Calculate prediction: K_new %*% alpha
  y_pred <- K_new %*% model$alpha [cite: 450]
  return(y_pred) [cite: 451]
}
