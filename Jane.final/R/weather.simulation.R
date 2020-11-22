#'weather simulation function
#'
#'This function simulate the weather forecast in Richmond,If a day is sunny, the probability that the next day is sunny is 0.85. If a day is rainy, the probability that the next day is rainy is 0.35. If a day is rainy, the amount of rainfall accumulation in the city is governed by an Exponential(λ=2) distribution, where the value from that distribution is the rainfall in inches. If a day is sunny, there can be no rain
#' @param  initial: weather condition on the initial day
#' @keywords weather.simulation
#' @return the number of projected sunny days in the next 10 days, as well as the projected rainfall accumulation
#' @import LaplacesDemon
#' @export
#' @examples weather.simulation("sunny")
#'
weather.simulation<-function(initial){
  day<-c()
  prop.rain.rain<-LaplacesDemon::rbern(1,0.35)
  prop.sun.rain<-LaplacesDemon::rbern(1,0.15)

  if(initial=="rainy"){day[1]<-prop.rain.rain}
  if(initial=="sunny"){day[1]<-prop.sun.rain}
  for(i in 2:10){
    day[i]<-ifelse(day[i-1]==1,prop.rain.rain,prop.sun.rain)
  }
  rainfall<-day*rexp(10,2)
  simu<-c(sum(day==0),sum(rainfall))
  return(simu)
}
