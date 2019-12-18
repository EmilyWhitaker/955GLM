#can we schimdt with outputdata?

library(glmtools)
library(GLM3r)
library(tidyverse)
#library(lubridate)

depth= seq(1,25,1)

get_var(file = 'outputs/output-CR.nc', reference = 'surface',var_name = 'temp', z_out = seq(1,25,1))

CR2wtemp <- get_var(file = 'outputs/output-CR.nc', reference = 'surface',var_name = 'temp', z_out = depth)
head(CR2wtemp)


CRhypso <- read.delim('../CR_lake_hypsometry.txt', sep = '\t')
head(CRhypso)

V <- 3900000 #volume of CR, ensure its cubic meters

# dummy = matrix(NA, nrow = length(rep(CR2wtemp$DateTime, each = length(depth))), ncol = 1)
dummy = c()
dummy_data <- CR2wtemp[,-1]
for (ii in 1:length(CR2wtemp$DateTime)){
  dummy <- append(dummy, dummy_data[ii,])
}

model_df <- data.frame('DateTime' = rep(CR2wtemp$DateTime, each = length(depth)), "Depth" = rep(depth, times =
                                                                                  length(unique(CR2wtemp$DateTime))))
model_df$Temp <- as.double(dummy)

###model_df!!!!!!!!!!! its your thing!

#model_df_long = model_df %>% gather(DateTime,Temp, -Depth)

dens <- function(wtemp) {
  density = 999.842594+6.793952e-2*wtemp-9.09529e-3*wtemp^2+1.001685e-4*wtemp^3-1.120083e-6*wtemp^4+6.536332e-9*wtemp^5
  return(density)
}

wtempLong = model_df %>% mutate(Density = dens(Temp))
maxarea= 376358        #square meters
CRhypso = CRhypso %>% mutate(R = hp_factor*maxarea)

zg =CRhypso$R %*% CRhypso$Depth / sum(CRhypso$R)
zgg= zg
pi = 1000

ss.df = wtempLong %>% left_join(CRhypso, by = 'Depth') 
# Add new row for $$ sum{(z_g - z) * A_z * (p_i - \rho_z)}$$
#ss.df = ss.df %>% mutate(s = (t) * As * (D))

t= as.double(zgg) - ss.df$Depth
As= ss.df$R
D= 1000 - ss.df$Density

ss.df = ss.df %>% mutate(s = (t) * As * (D))

schmidtStability = ss.df %>% group_by(DateTime) %>% 
  summarise(s = sum(s,na.rm = T)) %>% 
  mutate(schmidtStability =  9.81/As[2] * s)

Cr = ggplot(schmidtStability) + geom_path(aes(x= as.Date(DateTime),y=schmidtStability))
Cr + labs(title = "Crystal Lake Schmidt Stability",
          x = "Year",
          y = "Schmidt Stability (J/m^2)") +
  theme_bw()

