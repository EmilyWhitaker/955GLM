devtools::install_github("USGS-R/glmtools", ref = 'ggplot_overhaul')
devtools::install_github("GLEON/GLM3r")
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
  scale_y_reverse()

+
  facet_grid(cols = vars(year))


plot(schmidtStability$schmidtStability)
  #change index to date
  #break 


######################################

interp.z <- seq(0,20,1)
densmat <- matrix(NA, nrow = length(interp.z),
                  ncol = length(unique(ss.df$sampledate)))
for (ii in 1:length(unique(ss.df$sampledate))){
  t1 <- unique(ss.df$sampledate)[ii]
  data <- ss.df$Density[which(ss.df$sampledate == t1)]
  z1 <- ss.df$depth[which(ss.df$sampledate == t1)]
  densmat[,ii]  <- approx(z1,data,interp.z)$y
  #densmatistheerror
}
emilydata = list('time' = unique(ss.df$sampledate),
                 'depth' =interp.z,
                 "sim" = densmat)

schmidt.work <- function(x, bthD, bthA){
  rho = 998.2 # km/m3
  g <- 9.81 # m/s2
  dep <- x$depth
  data <-  (x$sim)
  
  numD = nrow(data)
  if(max(bthD) > max(dep)){
    dep[numD+1] = max(bthD)
  }else if(max(bthD) < max(dep)) {
    bthD = c(bthD, max(dep))
    bthA = c(bthA, 0)
  }
  if(min(bthD) < dep[1]) {
    dep = c(min(bthD), dep)
  }
  A <- stats::approx(bthD, bthA, dep)$y
  
  zv <- dep %*% A / sum(A)
  schmidt <- c()
  for (ii in 1:length(x$time)){
    took <- !is.na(data[,ii])
    if (min(x$depth[took]) > min(dep)){
      appr.data <- c(data[took,ii][1], data[,ii])
      appr.dep <- c(min(dep), x$depth)
    } else {
      appr.data <- data[,ii]
      appr.dep <- x$depth
    }
    if (max(x$depth[took]) < max(dep)){
      appr.data <- c(appr.data, appr.data[length(na.omit(appr.data))])
      appr.dep <- c(appr.dep, max(dep))
    }
    took2 <- !is.na(appr.data)
    inter.data <- stats::approx(appr.dep[took2], appr.data[took2], dep)
    schmidt <- append(schmidt, g/A[1] * pracma::trapz(dep,A *(1000- inter.data$y) * (c(zv) - dep)))#
  }
  return(data.frame('time' = x$time, 'St' = schmidt))
}

E.schmidt <- schmidt.work(x = emilydata, bthD = abs(rev(hypso$depth)-max(hypso$depth)), bthA = rev(hypso$Az))
ggplot(E.schmidt, aes(time, St)) +
  geom_line()






