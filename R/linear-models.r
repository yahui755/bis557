
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats lm
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  form = all.vars(formula)
  mat = model.matrix(formula,data = data)
  res = data[,form[1]]
  fit_lm_model =list()
  fit_lm_model$coefficients =qr.coef(qr(mat),res)
  class(fit_lm)="lm"
  return(fit_lm)
}
