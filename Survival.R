library(tidyverse)
library(survival)
library(survminer)

setwd("/Users/lilianchanty/Library/CloudStorage/OneDrive-UniversityofToronto/Documents/UofT/EEB398+498")

data <- read.csv("2021 Data Lilian.csv")
data$Site <-as.factor(data$Site)
data$Notch <- as.factor(data$Notch)
data$Sex <- as.factor(data$Sex)

summary(data)

# Male ----
## Select turtles with known birth year (first capture <= 4 y.o.)
m_data <- data %>% 
  filter(Sex == "Male") 

m_known_age <- m_data %>% 
  group_by(Notch) %>% 
  filter(Year == min(Year)) %>% 
  #filter(Known.age == T)  # 44 males with known.age = T
  filter(Age <= 4) 

n_distinct(m_known_age$Notch) #There are 37 male turtles with known birth year

# Obtain males with known age at maturity
m_mat <- read.csv("model predicted age_size maturity (13Feb).csv")
m_mat <- m_mat[, c(2,4)]

m_mat <- m_mat %>% 
  filter(!is.na(Age_mat)) # 35 males with known age at maturity


#We want to add zero into individual names s.t. the length is 4 char
#e.g. 987 -> 0987
numbers_only <- function(x) {!grepl("\\D", x)}

for (i in 1:nrow(m_mat)){
  ind <- m_mat[i,1]
  if (numbers_only(ind)) {
    m_mat[i,1] <- str_pad(ind, 4, side = "left", pad = "0")
  }
}

# Sort by Notch
m_mat <- m_mat[order(m_mat[,1]),]

#Add birth year to the m_mat dataset
m_mat <- left_join(m_mat, unique(data[, c("Notch","Year.of.birth")]), 
                   by = c("Individual" = "Notch"))

colnames(m_mat) <- c("Notch", "Age", "Year.of.birth")

# Add survival data
# Status 1 = matured, status 2 = censored/ not yet matured/die 
m_mat$status <- rep(1, nrow(m_mat))

# Censored data
m_censor <- m_data %>% 
  filter(Notch %in% m_known_age$Notch & !(Notch %in% m_mat$Notch))

n_distinct(m_censor$Notch) # The 2 turtles that did not matured: 0426, 1324


## Fit survival models ----
### Cox Proportional Hazard Model ----
m.cox.md <- coxph(Surv(Age, status) ~ Year.of.birth, data = m_mat)
summary(m.cox.md)

# Survival curves
## Create the new data  
m_cox_pred <- data.frame(Year.of.birth = c(1987, 2011))

# Estimated probability of maturation
m_fit <- survfit(m.cox.md, newdata = m_cox_pred)
m_fit_mean <- survfit(m.cox.md, data = m_mat)
m_fit_list <- list(m_fit, m_fit_mean)

# Median survival time
m_fit_mean
summary(m_fit_mean)

m.cox.md %>% 
  tbl_regression(exp = TRUE) 


## Plot
# add method to grid.draw
grid.draw.ggsurvplot <- function(x){
  survminer:::print.ggsurvplot(x, newpage = FALSE)
}

m_cox_plot <- ggsurvplot_combine(list(m_fit_mean, m_fit),
                                 conf.int = T, 
                                 fun = "event",
                                 palette = c("grey20",
                                             "#E7B800",
                                             "#2E9FDF"),
                                 ylab = "Probability of Maturation",
                                 xlab = "Age",
                                 title = "(a)",
                                 legend = "bottom",
                                 legend.title = "Birth Year",
                                 legend.labs = c("Mean", "1987", "2011"),
                                 ggtheme = theme(panel.grid.minor = element_blank(),
                                                 panel.grid.major = element_blank(),
                                                 panel.border = element_rect(colour = "black", 
                                                                             fill = NA),
                                                 panel.background = element_blank()))

m_cox_plot

# ggsave("m_cox_plot.png", m_cox_plot)


# Female ----
f_data <- data %>% 
  filter(Sex == "Female") 

# Mature turtles
f_mat <- read.csv("f_mat.csv")

f_mat <- f_mat[, c(2,5,3)]

# We want to add zero into individual names s.t. the length is 4 char
for (i in 1:nrow(f_mat)){
  ind <- f_mat[i,1]
  if (numbers_only(ind)) {
    f_mat[i,1] <- str_pad(ind, 4, side = "left", pad = "0")
  }
}

f_mat$status <- rep(1, nrow(f_mat))

# Censored data (turtles that did not mature)
f_censor <- read.csv("f_profile Jul 10.csv")

f_censor <- f_censor %>% 
  filter(!(Notch %in% f_mat$Notch)) %>% 
  filter(is.na(Age_mat))

n_distinct(f_censor$Notch) # 38 censored females (still a juvenile or died before reaching maturity)

f_censor$Notch

f_censor <- f_data %>% 
  filter(Notch %in% f_censor$Notch) %>% #select those 38 females
  group_by(Notch) %>% 
  filter(Year == max(Year)) %>% 
  dplyr::select(Notch, Age, Year.of.birth)

f_censor$status <- rep(0, nrow(f_censor))

f_survival <- rbind(f_mat, f_censor)

# Remove censored females that are likely matured
# f_sus <- c("0794", "0841", "1048", "1104", "1672")
# f_survival <- subset(f_survival, !(Notch %in% f_sus))


## Fit survival models ----
### Cox Proportional Hazard Model ----
f.cox.md <- coxph(Surv(Age, status)~Year.of.birth, data = f_survival)
summary(f.cox.md)

# Survival curves
## Create the new data  
f_cox_pred <- data.frame(Year.of.birth = c(1986, 2003))

# Estimated probability of maturation
f_fit <- survfit(f.cox.md, newdata = f_cox_pred)
f_fit_mean <- survfit(f.cox.md, data = f_survival)
f_fit_list <- list(f_fit, f_fit_mean)

# Median survival time
f_fit_mean
summary(f_fit_mean)

f.cox.md %>% 
  tbl_regression(exp = TRUE) 

## Plot

# add method to grid.draw
grid.draw.ggsurvplot <- function(x){
  survminer:::print.ggsurvplot(x, newpage = FALSE)
}

f_cox_plot <- ggsurvplot_combine(list(f_fit_mean, f_fit),
                   conf.int = TRUE, 
                   fun = "event",
                   palette = c("grey20", "#E7B800", "#2E9FDF"),
                   ylab = "Probability of Maturation",
                   xlab = "Age",
                   title = "(b)",
                   legend = "bottom",
                   legend.title = "Birth Year",
                   legend.labs = c("Mean", "1986", "2003"),
                   ggtheme = theme(axis.title.y = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   panel.grid.major = element_blank(),
                                   panel.border = element_rect(colour = "black", 
                                                               fill = NA),
                                   panel.background = element_blank()))

                                                 
f_cox_plot

survival <- arrange_ggsurvplots(list(m_cox_plot, f_cox_plot), ncol = 2, nrow = 1)
survival
#ggsave("survival.png", survival)



# Check Assumptions ----

### Males ----

#### Linearity ----
# Check linearity using Martingale's residuals
ggcoxdiagnostics(m.cox.md, type = "martingale", ox.scale = "linear.predictions")

# Check linearity using DEVIANCE residuals
ggcoxdiagnostics(m.cox.md, type = "deviance", ox.scale = "linear.predictions")


#### Proportional hazard assumption ----

# Hazards should be proportional for cox proportional hazard model

# Test for proportional hazard using Schoenfeld test for PH
# H0: hazards are proportional
# HA: hazards are not proportional

# Will return test for each X and overall model
mtest <- cox.zph(m.cox.md) # p-value = 0.39. Fail to reject H0
mtest

# plot(ftest)
# abline(h = 0, col = 2) # red line indicates no change in HR
# red line falling within 95% confidence band most of the time

# # If the plot is reasonably flat, the PH assumption holds
ggcoxzph(mtest)


### Females ----
#### Linearity ----
# Check linearity using Martingale's residuals
ggcoxdiagnostics(f.cox.md, type = "martingale", ox.scale = "linear.predictions")


# Check linearity using DEVIANCE residuals
ggcoxdiagnostics(f.cox.md, type = "deviance", ox.scale = "linear.predictions")


#### Proportional hazard assumption ----
# Hazards should be proportional for cox proportional hazard model

# Test for proportional hazard using Schoenfeld test for PH
# H0: hazards are proportional
# HA: hazards are not proportional

# Will return test for each X and overall model
ftest <- cox.zph(f.cox.md) # p-value = 0.056. Fail to reject H0
ftest

# plot(ftest)
# abline(h = 0, col = 2) # red line indicates no change in HR
# red line falling within 95% confidence band most of the time except early in time

# If the plot is reasonably flat, the PH assumption holds
ggcoxzph(ftest)
