# Core libraries
install.packages("tidyverse")
install.packages("readx1")
install.packages("psych")
install.packages("corrplot")
install.packages("MASS")
install.packages("ggplot2")
install.packages("pscl")

library(tidyverse)
library(readxl)      # If your file is in Excel format
library(psych)       # For descriptive stats
library(corrplot)    # For correlation matrix
library(MASS)        # For negative binomial regression if needed
library(ggplot2)     # For all plotting
library(pscl)        # For testing overdispersion

##Importing datasets

clinical_dt<-read.csv("C:/Users/AHN/OneDrive - Africa Healthcare Network/Desktop/Hildas Docs and Projects/Membrane vs Outcome Project/Clinical_dataset.csv", sep=",", header = TRUE)
membrane_dt<-read.csv("C:/Users/AHN/OneDrive - Africa Healthcare Network/Desktop/Hildas Docs and Projects/Membrane vs Outcome Project/Membrane_dataset.csv", sep=",", header = TRUE)

#Preview data

glimpse(clinical_dt)
glimpse(membrane_dt)


#Merge

combined_dt<- clinical_dt %>%
  left_join(membrane_dt, by = "Center")

glimpse(combined_dt)

#Descriptive summary

describe(combined_dt[, c("Average.HB","Average.Albumin","Infections_p100","Admissions_p100","Mortalities_p100", "Raw.TDS","Conductivity","Membrane.Changes", "Membrane.age")])

clinic_summary <- combined_dt %>%
  group_by(Center) %>%
  summarise(
    mean_HB = mean(Average.HB, na.rm = TRUE),
    mean_Alb = mean(Average.Albumin, na.rm= TRUE),
    mean_infections = mean(Infections_p100, na.rm = TRUE),
    mean_admissions = mean(Admissions_p100, na.rm=TRUE),
    mean_mortalities = mean(Mortalities_p100, na.rm= TRUE)
  ) %>% 
  left_join(membrane_dt, by = "Center")

glimpse(clinic_summary)

# Correlation

corr_vars <- corr.test(clinic_summary[, c("Membrane.Changes", "Membrane.age","Raw.TDS", "Conductivity",
                                        "mean_HB", "mean_Alb", "mean_infections", "mean_admissions", "mean_mortalities")], use = "complete")

corrplot(corr_vars$r, method = "circle", type = "lower", bg = "skyblue", tl.cex = 0.8,tl.col = "black")

##Poison Regression Model
## 1. Infections model 

#With Raw TDS and Conductivity

infection_model <-glm(Infections_p100 ~ Membrane.Changes+ Raw.TDS + Conductivity + Average.HB + Average.Albumin + Membrane.age, family = poisson(), data = combined_dt)
summary(infection_model)

exp(infection_model_2$coefficients)

## 2. Admissions model

#with conductivity
admissions_model <-glm(Admissions_p100 ~ Membrane.Changes+ Raw.TDS + Conductivity + Average.HB + Average.Albumin + Membrane.age, family = poisson(), data = combined_dt)
summary(admissions_model)


#3. Mortality model

mort_model <-glm(Mortalities_p100 ~ Membrane.Changes+ Raw.TDS + Conductivity + Average.HB + Average.Albumin + Membrane.age, family = poisson(), data = combined_dt)
summary(mort_model)


##Group by membrane quality
combined_dt <- combined_dt %>%
  mutate(freq_group = ifelse(Membrane.Changes >= 2,"High","Low"))

ggplot(combined_dt, aes(x=freq_group, y = Mortalities)) +
  geom_boxplot() +
  labs(title = "Mortalities by Membrane Change Frequency", y = "Mortalities")



