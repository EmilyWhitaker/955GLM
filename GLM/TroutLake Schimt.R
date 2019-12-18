devtools::install_github("USGS-R/glmtools", ref = 'ggplot_overhaul')
devtools::install_github("GLEON/GLM3r")
library(glmtools)
library(GLM3r)
library(tidyverse)



#wtemp <- read.delim('wtemp.txt', sep = '\t')
#wtemp <- read.delim('CR_wtemp_ex8.txt', sep = '\t')
#wtemp <- read_delim('troutalltemps.txt', delim = '\t')
wtemp <- read_delim('trout89alltemps.txt', delim = '\t')

head(wtemp)

#hypso <- read.delim('hypso.txt', sep = '\t')
hypso <- read.delim('TL_lake_hypsometry.txt', sep = '\t')
head(hypso)

#V <- 479823720
V <- 228504600 #volume of TL, ensure its cubic meters
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
maxarea= 15651000        #square meters
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

#ggplot(schmidtStability) + geom_path(aes(x = Date, y = schmidtStability))

+
#  facet_grid(cols = vars(year))

#xrange <- range(schmidtStability$Date) 
#yrange <- range(schmidtStability$schmidtStability)


#plot(xrange, yrange, xlab="Years",
#     ylab="Schimt Stability (Joules/square meter)" ) 
#colors <- rainbow(ntrees) 
#linetype <- c(1:ntrees) 




#plot(
#  x= as.Date(schmidtStability$Date),
#  y= schmidtStability$schmidtStability,
#  main="Trout Lake Schimt Stability from 1983-2014",
#  ylab="Joule per square meter",
#  xlab = "Year")
  

#  lines(x, y, type=opts[c])
  
  

#  aes(x = Date, y = schmidtStability)

#tt= ggplot(schmidtStability) + geom_path(aes(x= as.Date(Date), 
 #                           y=schmidtStability))+
#  xlab= "Year" +
#  ylab= "Schmidt Stability (J/m^2)"
#plot(tt)+theme_bw()

#change index to date

p = ggplot(schmidtStability) + geom_path(aes(x= as.Date(sampleDate),y=schmidtStability))
p + labs(title = "Trout Lake Schmidt Stability in 1989",
         x = "Month",
         y = "Schmidt Stability (J/m^2)") +
  theme_bw()



schmidtStability_date <- 
  schmidtStability %>%
  separate(sampledate, c("year", "month", "day"), sep= "-", extra = "merge")







schmidtStability_dates %>% schmidtStability
    dplyr::mutate(year = lubridate::year(Date), 
                  month = lubridate::month(Date), 
                  day = lubridate::day(Date))


    
    
    
    
    
        
dfdates <- data.frame(date = schmidtStability$Date,
                     year = as.numeric(format(schmidtStability$Date, format = "%Y")),
                     month = as.numeric(format(schmidtStability$Date, format = "%m")),
                     day = as.numeric(format(schmidtStability$Date, format = "%d")))    
    
#break into year and facet grid
#greatest slope
#max SS
#80s, 90s, and 2000s graph?