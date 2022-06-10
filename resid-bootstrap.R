residual_boot <- function(B, y) {
  n <- length(y)
  original_mod <- cv.glmnet(x, y, relax = TRUE, nfolds = n, gamma = 0)
  p <- predict(original_mod, newx = x, s = "lambda.min")
  resids <- y - p
  c0 <- drop(coef(original_mod, s = "lambda.min"))
  samps <- Matrix(0, nrow = length(c0), ncol = B, sparse = TRUE)
  for (i in seq_len(B)) {
    ny <- p + sample(resids, n, TRUE)
    new_mod <- cv.glmnet(x, ny, relax = TRUE, nfolds = n, gamma = 0)
    samps[,i] <- coef(new_mod, s = "lambda.min")
  }
  return(list(c0 = c0, samps = samps))
}
