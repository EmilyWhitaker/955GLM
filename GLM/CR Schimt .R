devtools::install_github("USGS-R/glmtools", ref = 'ggplot_overhaul')
devtools::install_github("GLEON/GLM3r")
devtools::install_github("GLEON/GLMr")
library(glmtools)
library(GLM3r)
library(tidyverse)



#wtemp <- read.delim('wtemp.txt', sep = '\t')
#wtemp <- read.delim('CR_wtemp_ex8.txt', sep = '\t')
wtemp <- read_delim('CR_wtemp_ex8.txt', delim = '\t')

head(wtemp)

#hypso <- read.delim('hypso.txt', sep = '\t')
hypso <- read.delim('CR_lake_hypsometry.txt', sep = '\t')
head(hypso)

#V <- 479823720
V <- 3900000 #volume of CR, ensure its cubic meters
wtempLong = wtemp %>% gather(Date,Temp, -Depth)

#as.Date(wtempLong$Date, "%m/%d/%Y")

#wtempLong$Date format(wtempLong) = --- change into date format (%mm %dd %yyyy)


#ggplot(wtempLong) + geom_path(aes(x = Temp, y = Depth)) + 
#  scale_y_reverse() +
#  facet_grid(cols = vars(Date))

wtempLong$Temp <- as.double(wtempLong$Temp)
# Density function, where the argument is water temperature in deg C
dens <- function(wtemp) {
  density = 999.842594+6.793952e-2*wtemp-9.09529e-3*wtemp^2+1.001685e-4*wtemp^3-1.120083e-6*wtemp^4+6.536332e-9*wtemp^5
  return(density)
}

# Create a new column in your wtempLong dataframe for water density
wtempLong = wtempLong %>% mutate(Density = dens(Temp))
maxarea= 376358        #square meters
hypso = hypso %>% mutate(R = hp_factor*maxarea)

#hypso = hypso %>% mutate(P = hp_factor*area)

# The volume centroid 
##zg <- 1/V * sum(hypso$Az)
zg =hypso$R %*% hypso$Depth / sum(hypso$R)
zgg= zg
pi = 1000

ss.df = wtempLong %>% left_join(hypso, by = 'Depth') 
# Add new row for $$ sum{(z_g - z) * A_z * (p_i - \rho_z)}$$
#ss.df = ss.df %>% mutate(s = (t) * As * (D))

t= as.double(zgg) - ss.df$Depth
As= ss.df$R
D= 1000 - ss.df$Density

ss.df = ss.df %>% mutate(s = (t) * As * (D))

schmidtStability = ss.df %>% group_by(Date) %>% 
  summarise(s = sum(s,na.rm = T)) %>% 
  mutate(schmidtStability =  9.81/As[2] * s)

ggplot(schmidtStability) + geom_path(aes(x = Date, y = schmidtStability))

+
  facet_grid(cols = vars(year))


plot(schmidtStability$schmidtStability)


Cr = ggplot(schmidtStability) + geom_path(aes(x= as.Date(Date),y=schmidtStability))
Cr + labs(title = "Crystal Lake Schmidt Stability",
         x = "Year",
         y = "Schmidt Stability (J/m^2)") +
  theme_bw()


library(lubridate)
ss= schmidtStability %>% 
  mutate(year = ifelse(month(Date)==12, year(Date)-1, year(Date)),
         Date = as.Date(Date))

year(ss$Date) = 2019

ss %>% 
  ggplot(aes(x = Date, y = schmidtStability, col= year, group = year))+
  geom_line()

ss %>% 
  mutate(lag.s = lag(schmidtStability),
         lead.s= lead(schmidtStability),
         mixend = ifelse(lead.s==0&schmidtStability>0, 1, 0),
         mixstart = ifelse(lag.s==0&schmidtStability>0,1,0)) %>% 
  select(Date, mixend, mixstart) %>% 
  gather(mix, ind, -Date) 







schmidtStability_withdates <-
  schmidtStability %>%
  mutate(Date, c("year", "month", "day"), sep= "-", extra = "merge")
schmidtStability_withdates 

Cr1 = ggplot(schmidtStability_withdates) + geom_point(aes(x= month,y=schmidtStability, ))
Cr1 + labs(title = "Crystal Lake Schmidt Stability",
          x = "month",
          y = "Schmidt Stability (J/m^2)") +
  theme_bw()






##color based on year 
#stat tree?
??aes
  

#change index to date
#break into year and facet grid
#greatest slope
#max SS
#80s, 90s, and 2000s graph?
