european <- function(numsim=1000,Spot =  100,Strike = 95,
                        t = 365,r = 0.06,sigma = 0.2,silent = F, precison = 3) {
  GBM <- matrix(NA, nrow = (t), ncol = numsim)
  p <- progress_estimated(numsim)
  for(i in 1:numsim){
    GBM[,i] <-  exp( (r  - 0.5 * sigma^2) * (1/(m)) + (sigma * (sqrt(1/(t))) * rnorm(t, mean = 0, sd = 1)))
    if(silent == F) print(p$tick())
  }
  if(silent == F) print(p)

  GBM[1,] <- Spot
  GBM <- apply(GBM,2,cumprod)
  valsput  <- ifelse(GBM[t,] < Strike,Strike - GBM[t,],0)
  valscall <- ifelse(GBM[t,] > Strike,GBM[t,]-Strike,0)
  priceput <- mean(valsput*(1/(1+r))) %>% round(., precison)
  pricecall <- mean(valscall*(1/(1+r)))%>% round(., precison)
  cat("\n")
  if(silent == T){
  data.frame(
  "put" = priceput,
  "call" = pricecall, row.names = "")
  }else{
    data.frame(
    "put" = priceput,
    "call" = pricecall,
    "numsim"=numsim,
    "Spot" =  Spot,
    "Strike" = Strike,
    "r" = r,
    "sigma" = sigma, row.names = "")
  }
}



