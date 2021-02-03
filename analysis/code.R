# Set up environment -------------------------------------
# Load Libraries
library(tidyverse)
library(here)
library(magrittr)
library(readxl)
library(MASS)
library(car)
library(robust)
library(GGally)
library(rcompanion)
library(hermite)
library(gee)
library(stats)
 

# Load Data - Updated 12-8!
covid_counts <- read_xlsx("../data/biweekly-counts-rates-by-geography-dec-8.xlsx",
                          col_types = c("text", "date", "date", 
                                        "numeric", "text", "numeric", "text",
                                        "numeric", "text", "numeric", "text",
                                        "numeric", "numeric"))
covid_demos <- read_xlsx("../data/total-counts-by-date-city-demography-dec-8.xlsx")
 
# Clean Data --------------------------------------------
covid_counts <- covid_counts[, c("City", 
                                 "Week_Start", 
                                 "People_Tested_Rate", 
                                 "Positives", 
                                 "Deaths")]
covid_counts_clean <- covid_counts %>%
  mutate(City = factor(City),
         People_Tested_Rate = as.numeric(People_Tested_Rate)/100000) 

covid_demos <- covid_demos[, c("Age_Group", 
                               "City", 
                               "Population")]
covid_demos_clean <- covid_demos %>%
  filter(City != "All King County") %>%
  mutate(City = factor(City),
         Age_Group = factor(Age_Group))

covid <- covid_counts_clean %>%
  left_join(covid_demos_clean, 
            by = "City")
 
# Visualize Data ----------------------------------------
# Scatterplot
ggplot(covid) + 
  geom_point(aes(Week_Start, 
                 Positives), 
             alpha=0.01) +
  labs(title= "Biweekly Counts of Positive COVID-19 Tests",
       subtitle = "Cities in King County from January 29, 2020 to December 8, 2020",
       caption= "Source: Washington State Department of Health",
       x="",
       y="Biweekly Positives\n")+
  theme_light()+
  theme(legend.position = "none")

# Histogram Code
ggplot(covid) + 
  geom_boxplot(aes(Positives)) +
  labs(y="",
       x="Biweekly Positives")+
  theme_light()+
  theme(legend.position = "none")


# Poisson Model -----------------------------------------
pois_mod <- glm(Positives~.-City, 
                data=covid, 
                family="poisson")

summary(pois_mod)

Anova(pois_mod,
      type="II",
      test="LR")

# Poisson Model Residuals Analysis
glm.evaluate=pois_mod

rp=resid(glm.evaluate, "pearson")
rd=resid(glm.evaluate, "deviance")
rw=resid(glm.evaluate, "working")
rs=rstudent(glm.evaluate)
ra=3*(glm.evaluate$y^{2/3}-glm.evaluate$fit^{2/3})/glm.evaluate$fit^{1/6}/2

par(mfrow=c(2,3))
plot(glm.evaluate$fit,rp, main="Pearson")
plot(glm.evaluate$fit,rd, main = "deviance")
plot(glm.evaluate$fit,rw, main="working")
plot(glm.evaluate$fit,rs, main="student")
plot(glm.evaluate$fit,ra, main="ra")
plot(hatvalues(glm.evaluate), type="h", main="hat")
 

# Quasi-Poisson Model ------------------------------------
qp_mod = glm(Positives~.-City, 
             data=covid,
             family=quasipoisson(link = "log"))
summary(qp_mod)

Anova(qp_mod,
      type="II",
      test="LR")

# Quasi-Poisson Residuals Analysis
glm.evaluate=qp_mod

rp=resid(glm.evaluate, "pearson")
rd=resid(glm.evaluate, "deviance")
rw=resid(glm.evaluate, "working")
rs=rstudent(glm.evaluate)
ra=3*(glm.evaluate$y^{2/3}-glm.evaluate$fit^{2/3})/glm.evaluate$fit^{1/6}/2

par(mfrow=c(2,3))
plot(glm.evaluate$fit,rp, main="Pearson")
plot(glm.evaluate$fit,rd, main = "deviance")
plot(glm.evaluate$fit,rw, main="working")
plot(glm.evaluate$fit,rs, main="student")
plot(glm.evaluate$fit,ra, main="ra")
plot(hatvalues(glm.evaluate), type="h", main="hat")


# Negative Binomial Model --------------------------------
nb_mod <- glm.nb(Positives~.-City, 
                 data = covid, 
                 control = glm.control(maxit=75))
summary(nb_mod)

Anova(nb_mod,
      type="II",
      test="LR")

# Negative Binomial Residuals Analysis
glm.evaluate=nb_mod

rp=resid(glm.evaluate, "pearson")
rd=resid(glm.evaluate, "deviance")
rw=resid(glm.evaluate, "working")
rs=rstudent(glm.evaluate)
ra=3*(glm.evaluate$y^{2/3}-glm.evaluate$fit^{2/3})/glm.evaluate$fit^{1/6}/2

par(mfrow=c(2,3))
plot(glm.evaluate$fit,rp, main="Pearson")
plot(glm.evaluate$fit,rd, main = "deviance")
plot(glm.evaluate$fit,rw, main="working")
plot(glm.evaluate$fit,rs, main="student")
plot(glm.evaluate$fit,ra, main="ra")
plot(hatvalues(glm.evaluate), type="h", main="hat")
 

# QQ plots -----------------------------------------------
par(mfrow=c(1,3))
qqnorm(pois_mod$fitted.values, 
       main="Poisson")
qqline(pois_mod$fitted.values)

qqnorm(qp_mod$fitted.values, 
       main="Quasi-Poisson")
qqline(qp_mod$fitted.values)

qqnorm(nb_mod$fitted.values, 
       main="Negative Binomial")
qqline(nb_mod$fitted.values)
 
# Another check for outliers -----------------------------
outlierTest(pois_mod)
outlierTest(qp_mod)
outlierTest(nb_mod)

# Anova tables -------------------------------------------
Anova(pois_mod,
      type="II",
      test="LR")

anova(qp_mod,
      test="F")

Anova(nb_mod,
      type="II",
      test="LR")


