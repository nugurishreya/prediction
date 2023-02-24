#' Trans fat IR prediction
#'
#' @description This function returns the plsr model obtained using IR data (5500) for trans fat
#' @return calibrated plsr model
#'
#' @export
#'
#' @examples
transfat_model <- function() {
  data <- data4500
  # i <- c(2:ncol(data))
  # data[,i] <- apply(data[,i], 2, function(x) as.numeric(as.character(x)))
  s = data[2:1774]
  y = data[1775]
  data_1 <- data.frame(TF = data[,1775], MIR = I(data.matrix(s)))
  #pre-processing
  new <- apply(data_1$MIR, 1, FUN=sgolayfilt, p=2, n=3, m=2, ts=1)
  newspectra <- apply(t(new), 1, FUN=sgolayfilt, p=2, n=3, m=0, ts=1)
  data_sd <- data.frame(TFS= data$trans, MIR = I(t(newspectra[1545:1715,])))
  model <- plsr(TFS~MIR, ncomp = 4, data = data_sd, validation ='LOO')
  model
}
