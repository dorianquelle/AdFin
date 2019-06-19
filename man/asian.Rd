\name{asian}
\alias{asian}
\title{asian Monte Carlo}
\usage{
asian(numsim=10000,Spot =  100,Strike = 95,t = 365,r = 0.06,sigma = 0.2,silent = F)
}
\description{
Calculates asian option prices.
Put and Call options can be calculated arithmetically or geometrically.
Output is returned in a data.frame structure.
}
\examples{
asian(numsim=10000,Spot =  100,Strike = 95,t = 365,r = 0.06,sigma = 0.2,silent = F)
}
