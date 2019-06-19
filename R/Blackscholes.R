BSCall <- function(Spot = 120, Strike = 100, r = 0.06, t = 1, sigma = 0.20) {
  d1  <-  (log(Spot/Strike) + (r + sigma^2/2)*t) / (sigma*sqrt(t))
  d2  <-  d1 - sigma*sqrt(t)
  (Spot * pnorm(d1)  - Strike*exp(-r*t)*pnorm(d2)) %>% return()
}

BSPut <- function(Spot = 120, Strike = 100, r = 0.06, t = 1, sigma = 0.20) {
  d1  <-  (log(Spot/Strike) + (r + sigma^2/2)*t) / (sigma*sqrt(t))
  d2  <-  d1 - sigma*sqrt(t)
  (-Spot * pnorm(-d1) + Strike*exp(-r*t)*pnorm(-d2)) %>% return()
}
