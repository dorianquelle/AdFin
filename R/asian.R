
asian <- function(numsim=10000,Spot =  100,Strike = 95,t = 365,r = 0.06,sigma = 0.2,silent = F) {
  GBM <- matrix(NA, nrow = (t), ncol = numsim)
  p <- dplyr::progress_estimated(numsim)
  for(i in 1:numsim){
    GBM[,i] <-  exp( (r  - 0.5 * sigma^2) * (1/(t)) + (sigma * (sqrt(1/(t))) * rnorm((t), mean = 0, sd = 1)))
    print(p$tick())
  }
  print(p)

  GBM[1,] <- Spot
  GBM <- apply(GBM,2,cumprod)

  pr <- colSums(GBM[,])/((t))
  price <- exp(-r) * mean(pmax(pr-Strike,0))
  putprice <- exp(-r) * mean(pmax(Strike-pr,0))

  geopr <- GBM %>% geoprice()
  putgeopr <- exp(-r) * mean(pmax(Strike-geopr,0))
  geopr <- exp(-r) * mean(pmax(geopr-Strike,0))

  cat("\n")
  if(silent == T){
    data.frame("arith_Call" = price,
               "geom_Call" = geopr,
               "arith_Put" = putprice,
               "geom_Put" = putgeopr,
               row.names = ""
               ) %>% return()
  }else{
    data.frame("arith_Call" = price,
               "geom_Call" = geopr,
               "arith_Put" = putprice,
               "geom Put" = putgeopr,
               "numsim"=numsim,
               "Spot" =  Spot,
               "Strike" = Strike,
               "t" = t,
               "r" = r,
               "sigma" = sigma,
               row.names = ""
    ) %>% return()
  }
}



