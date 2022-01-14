library(tidyverse)

df <- read.csv(file='MechaCar_mpg.csv')
head(df, n=10)

lm(formula =  mpg ~ vehicle_weight + vehicle_length + spoiler_angle + ground_clearance + AWD, data = df) #generate multiple linear regression model

summary(lm(formula =  mpg ~ vehicle_weight + vehicle_length + spoiler_angle + ground_clearance + AWD, data = df))

df_coils <- read.csv(file='Suspension_Coil.csv')
head(df, n=10)

total_summary <- df_coils %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))
lot_summary <- df_coils %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))

t.test(df_coils$PSI, mu=1500)
