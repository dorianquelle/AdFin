

LSMput <- function (Spot = 100, Strike = 110,
                    sigma = 0.2,numsim = 1000,
                    t = 365,r = 0.06, silent = F) {
  geometricbrownian <- matrix(NA, nrow = (t), ncol =numsim)
  if(silent == F) cat("constructing gbm \n")
  p <- progress_estimated(numsim)
  for(i in 1:numsim){
    geometricbrownian[,i] <-  exp( (r  - 0.5 * sigma^2) * (1/(t)) +
                                     (sigma * (sqrt(1/(t))) * rnorm((t), mean = 0, sd = 1)))
    print(p$tick())
  }
  print(p)
  geometricbrownian[1,] <- geometricbrownian[1,]*Spot
  geometricbrownian <- t(apply(geometricbrownian,2,cumprod))

  cashflow <- ifelse(Strike - geometricbrownian > 0, Strike - geometricbrownian, 0)
  X <- ifelse(geometricbrownian < Strike, geometricbrownian, NA)
  X2 <- X^2
  discashflow <- cashflow * exp(-r * (1/t))
  Y <- cbind((matrix(NA, nrow = numsim, ncol = t - 1)), discashflow[,t])

  predmatrix <- matrix(NA, nrow = numsim, ncol = t - 1)
  if(silent == F) cat("\n\nCalculating expecctations\n")
  p <- progress_estimated((t - 1))

  try(
    for (i in (t - 1):1){
      if(silent == F) p$tick()$print()
      reg1 <- lm(Y[, i + 1] ~ X[, i] + X2[, i])
      predmatrix[, i] <-  reg1$coefficients[1] + reg1$coefficients[2] * X[,i] + reg1$coefficients[3] * X2[,i]
      predmatrix[is.na(predmatrix[,i]),i] <- 0
      Y[, i] <- ifelse(cashflow[, i] > predmatrix[, i], discashflow[, i], Y[,i + 1] * exp(-r * (1/t)))
    },
    silent = TRUE)

  predmatrix[is.na(predmatrix)] <- 0
  predmatrix <- cbind(predmatrix, 0)

  payoffpred <- ifelse(predmatrix > cashflow, 0, cashflow)

  for(i in 1:nrow(payoffpred)){
    j  <- which(payoffpred[i,] > 0)[1]
    payoffpred[i,-j] <- 0
  }


  PRICE <- payoffpred %*% as.matrix(exp(-1 * (1:t)/t * r)) %>% mean %>% round(.,3)
  if(silent == F){
    cat("\n\n\n")
    return(data.frame("Price" = PRICE, "Spot" = Spot, "sigma" = sigma, "#Sim" =numsim, "t" = t, "Strike" = Strike,
                      "r" = r, row.names = ""))
  }else{
    return(PRICE)
  }
}


