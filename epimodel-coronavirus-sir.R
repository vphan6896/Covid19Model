#Using epimodel
#Using data from worldometer
library(EpiModel)
library(tidyverse)
cor <- read_csv("coronavirus_worldometer.csv")

#Check column names
names(cor)

#Drop the total row and store it
tot <- cor %>% 
  filter(Country %in% c("Total:"))

'%not_in%' <- Negate('%in%')
#Could just do filter(!Country %not_in% c("Total:"))

g_cor <- cor %>% 
  filter(Country %not_in% c("Total:"))

tot

#death rate of coronavirus
dr <- tot$`Total Deaths` / tot$`Total Cases`

rr <- tot$`Total Recovered` / tot$`Total Cases`

#birth rate overall of the world
br <- 18.5/1000 #ourworldindata.org

#death rate overall of the world
dr_norm <- 7.7/1000 #indexmundi.com

#infection rate in world
tot_pop <- 7.53 * (10^9)
ir <- tot$`Total Cases` / tot_pop


#dr.rate multiplied to be smaller than di rate to indicate
#disease-induced mortality
#act rate based on news articles say 2-4 people get the virus
param <- param.dcm(inf.prob = .3, act.rate = 2, rec.rate = rr,
                   a.rate=br, ds.rate = dr_norm, di.rate = dr,
                   dr.rate = dr_norm * .9)
#s.num based on Wuhan population
init <- init.dcm(s.num=10 ^ 6, i.num=1, r.num=tot$`Total Recovered`)
control <- control.dcm(type="SIR", nsteps=250, dt=.5)
mod <- dcm(param, init, control)

par(mar = c(3.2, 3, 2, 3), mgp = c(2, 1, 0), mfrow = c(1, 2))
plot(mod, popfrac = FALSE, alpha = 0.5,
     lwd = 4, main = "Compartment Sizes")
plot(mod, y = "si.flow", lwd = 4, col = "firebrick",
     main = "Disease Incidence", legend = "n")

par(mfrow = c(1, 1))
comp_plot(mod, at = 50, digits = 1)
