library(tidyverse)
library(marked)


setwd("/Users/lilianchanty/OneDrive - University of Toronto/Documents/University of Toronto/EEB398+498")


# --------------------------PART 1: Filter the dataset--------------------------

data <- read.csv("2021 Data Lilian.csv")
data$Site <-as.factor(data$Site)
data$Notch <- as.factor(data$Notch)
data$Sex <- as.character(data$Sex)

summary(data)

# Sex can only be Female, Male, or Unknown
data$Sex <- ifelse(data$Sex == "Female", data$Sex, 
                   ifelse(data$Sex == "Male", data$Sex, "Unknown"))

table(data$Sex)


#Only turtles in WHP(e), WestRose, BB are included
table(data$Site)

start.year = 1990

filter.data <- data %>% 
  #Only include turtles from WHP and WHPe
  filter(Site == "Wolf Howl Pond" | Site == "West Rose" |
           Site == "Wolf Howl Pond E." | Site == "Wolf Howl Pond e."|
           Site == "Blanding's Bog" | Site == "Blanding's pond") %>% 
  filter(date != "" | Year != "") %>%    # Remove no-date data
  filter(Year >= start.year & Year <= 2021) %>% 
  select(Notch, Year, Sex) %>% 
  mutate(seen = 1)

#order dataset by year and Notch
filter.data <- filter.data[order(filter.data$Year, filter.data$Notch),]

head(filter.data)

# ----------------------------- PART 2: Covariates ----------------------------

## ---- Sampling effort 1 - Number of sampling days ----
sampling.days <- data %>% 
  #Only include turtles from Arowhons
  filter(Site == "Wolf Howl Pond" | Site == "West Rose" |
           Site == "Wolf Howl Pond E." | Site == "Wolf Howl Pond e."|
           Site == "Blanding's Bog" | Site == "Blanding's pond") %>% 
  # Remove no-date data
  filter(date != "" & Year != "") %>%  
  #Change the format of the date
  mutate("Date" = as.Date(date, "%d-%b-%y")) %>% 
  select(Year, Date)

#Remove turtles captured on Jan 1 (i.e. no date associated) or before end of April (likely mistakes)
sampling.days$month <- month(sampling.days$Date)

sampling.days <- sampling.days %>% 
  filter(month >= 4) %>% 
  group_by(Year) %>% 
  summarise(sampling.start.date = min(Date), # First day of sampling
            sampling.end.date = max(Date)) %>% # Last day of sampling
  arrange(Year)

sampling.days <- sampling.days[-c(1,34),] # Remove 1978 and 2022 data

# Calculate # of sampling days
sampling.days <- sampling.days %>% 
  mutate(num.days = (as.numeric(sampling.end.date - sampling.start.date + 1)))

# Rename Year column to "time" for merge_design.covariates function PART 4
colnames(sampling.days)[1] <- "time"


##----Sampling effort 2 - Number of researchers involved in turtle sampling----
annual.data <- read.csv("Annual Data.csv")  

researchers <- annual.data %>% 
  filter(Site == "Wolf Howl Pond" | Site == "West Rose" |
           Site == "Wolf Howl Pond E." | Site == "Wolf Howl Pond e."|
           Site == "Blanding's Bog" | Site == "Blanding's pond") %>% 
  group_by(Year) %>% 
  #distinct(Processor) %>% arrange(Year) %>%  # Look at the name of the researchers
  summarise(num.researcher = n_distinct(Processor))

# Remove 1984, 1985, 1989, 2022
researchers <- researchers %>% 
  filter(Year >= 1990 & Year <= 2021)

colnames(researchers)[1] <- "time"

## -----------------------Home site-----------------------
# Define a function to find home site of a turtle 
# i.e. the lake that is mostly likely to find the turtle

# Function to find the mode
mode <- function(x) {
  tab <- as.data.frame(table(x))
  tab[tab$Freq == max(tab$Freq),1]
}

# Check if this function works
test.data <- data[data$Notch == "1135",] %>% select(Notch, Year, Site)
test.data2 <- data[data$Notch == "1252",] %>% select(Notch, Year, Site)
test.data3 <- data[data$Notch == "2214",] %>% select(Notch, Year, Site)
test.data <- rbind(test.data, test.data2, test.data3)
test.data #1135: WHP; 1252: WHPe; 2214: West Rose

test.data %>%
  group_by(Notch) %>% 
  summarise(home.site = mode(Site))

# Find the home sites of all arowhon turtles
home.sites <- data %>% 
  filter(Site == "Wolf Howl Pond" | Site == "West Rose" |
           Site == "Wolf Howl Pond E." | Site == "Wolf Howl Pond e."|
           Site == "Blanding's Bog" | Site == "Blanding's pond") %>% 
  group_by(Notch) %>% 
  summarise(home.site = mode(Site))

table(home.sites$home.site)

# Site can only be WHP, WHPe, WRose or BB
home.sites[home.sites$home.site == "Wolf Howl Pond e.",2] <- "Wolf Howl Pond E."
home.sites[home.sites$home.site == "Blanding's pond",2] <- "Blanding's Bog"

table(home.sites$home.site)

# Check if each turtle has only 1 home site
multi.home.site <- home.sites %>% group_by(Notch) %>% tally() %>% filter(n > 1)
nrow(multi.home.site) # There are 27 turtles with more than 1 home sites

#view(home.sites[home.sites$Notch %in% multi.home.site$Notch,]) # Check their respective home sites

# Add home site into filter.data
filter.data <- left_join(filter.data, home.sites,
           by = c("Notch" = "Notch"))

filter.data <- filter.data %>% relocate(home.site, .before = seen)

# -----------------------PART 3: Create encounter history-----------------------

#Create encounter history of each turtle
JS.data <- filter.data %>%
  pivot_wider(names_from = Year, values_from = seen, values_fill = 0) %>% 
  unite("ch", 4:tail(names(.),1), sep = "") %>% #put all encounter history into 1 column
  relocate(ch, .before = Sex)


JS.data <- as.data.frame(JS.data[, -1] ) #Remove Notch column

head(JS.data)
str_length(JS.data$ch[1]) #There are 32 sampling years since 1990


# ----------------------------- PART 4: JS Models -----------------------------

# The following code was adapted from 
# a) Jolly-Seber models to estimate population size in R: https://jamesepaterson.github.io/jamespatersonblog/2020-07-26_jolly_seber_models.html
# b) Lab 08 - Analyses in RMark: https://www.montana.edu/rotella/documents/502/lab08RMark.html

JS.data$Sex <- as.factor(JS.data$Sex)

# Process data
JS.processed <- marked::process.data(JS.data, model = "JS", 
                                     groups = c("Sex","home.site"),
                                     begin.time = 1990)
JS.processed

# Create default design data
JS.ddl = marked::make.design.data(JS.processed)
JS.ddl

# Add covariates to design data
sampling.days <- sampling.days[, c(1,4)]
JS.ddl$p = merge_design.covariates(JS.ddl$p, sampling.days)
JS.ddl$p = merge_design.covariates(JS.ddl$p, researchers)

fit.js.models <- function(){
  #Formulae for model
  ## Define models for Phi (survival probability)
  Phi.dot <- list(formula=~1)
  Phi.time <- list(formula=~time)
  Phi.time <- list(formula=~Sex)
  Phi.timeplussex <- list(formula=~time+Sex)
  
  ## Define models for p (probability of capture)
  p.dot <- list(formula=~1)
  p.time <- list(formula=~time)
  p.sex <- list(formula=~Sex)
  # p.site <- list(formula=~home.site)
  p.days <- list(formula=~num.days)
  p.ppl <- list(formula=~num.researcher)

  p.timeplussex <- list(formula=~time+Sex)
  # p.timeplussite <- list(formula=~time+home.site)
  p.timeplusdays <- list(formula=~time+num.days)
  p.timeplusppl <- list(formula=~time+num.researcher)
  # p.sexplussite <- list(formula=~Sex+home.site)
  p.sexplusdays <- list(formula=~Sex+num.days)
  p.sexplusppl <- list(formula=~Sex+num.researcher)
  # p.siteplusdays <- list(formula=~home.site+num.days)
  # p.siteplusppl <- list(formula=~home.site+num.researcher)
  
  # p.timeplussexplussite <- list(formula=~time+Sex+home.site)
  p.timeplussexplusdays <- list(formula=~time+Sex+num.days)
  p.timeplussexplusppl <- list(formula=~time+Sex+num.researcher)
  # p.sexplussiteplusdays <- list(formula=~Sex+home.site+num.days)
  # p.sexplussiteplusppl <- list(formula=~Sex+home.site+num.researcher)
  
  # p.timeplussexplussiteplusdays <- list(formula=~time+Sex+home.site+num.days)
  # p.timeplussexplussiteplusppl <- list(formula=~time+Sex+home.site+num.researcher)
  
  ## Define models for pent (probability of entry)
  # pent estimates MUST SUM to 1 (for each group). This is constained using a Multinomial Logit link
  pent.dot <- list(formula=~1)
  pent.time <- list(formula=~time)
  pent.sex <- list(formula=~Sex)
  pent.timeplussex <- list(formula=~time+Sex)
  
  ## Define model for N (super-population), NOT predicted population size
  N.dot <- list(formula=~1)
  N.sex <- list(formula=~Sex)
  
  cml <- create.model.list(c("Phi","p", "pent", "N"))
  results <- crm.wrapper(cml, data = JS.processed, ddl = JS.ddl,
                         external = FALSE, accumulate = FALSE, hessian = TRUE)
  
  return(results)
}
  

# Run function
js.models <- fit.js.models()

# Display model table
js.models

# ------------------------ PART 5: Population Analysis ------------------------
detach("package:marked", unload=TRUE)
library(RMark)

# Extract population density
# Process data
js.proc <- RMark::process.data(JS.data, model = "POPAN", 
                               groups = c("Sex", "home.site"),
                               begin.time = 1990)

# Formulae for model
## CHANGE TO PARAMETERS OF THE MODEL WITH LOWEST AICc!!
# Phi.dot <- list(formula=~1)
# p.dot <- list(formula=~1)
# pent.dot <- list(formula=~1)
# N.dot <- list(formula=~1)

best.model <- mark(JS.data, model = "POPAN", 
              model.parameters = list(Phi = Phi.dot, p= p.dot, 
                                      pent = pent.dot, N = N.dot),
              realvcv = TRUE, hessian = TRUE)

derived.best.model <- popan.derived(js.proc, best.model)$N
derived.best.model


# pop.den <- as.data.frame(derived.best.model %>% 
#   mutate(Year = Occasion + 1989) %>% 
#   group_by(Year) %>% 
#   summarise(N.total = sum(N)))
# 
# pop.den


#Plot results

# ggplot(data = pop.den, aes(x = Year, y = N.total)) + 
#   #geom_point() +
#   geom_line() +
#   labs(x = "Year", y = "Population density") +
#   theme_classic()

ggplot(data = derived.best.model) + 
  #geom_point() +
  geom_line(aes(x = (Occasion + 1989), y = N, group = Sex)) +
  labs(x = "Year", y = "Population density") +
  theme_classic()


# Compare to the number of turtles captured in Arowhons each year
filter.data %>% 
  group_by(Year) %>% 
  tally() %>% 
  ggplot(aes(x = Year, y = n)) +
  geom_line() +
  theme_classic()

# -------------------------- PART 6: Goodness-of-test --------------------------
# See https://jamesepaterson.github.io/jamespatersonblog/2020-05-20_gof_for_CJS

library(R2ucare)

#Create matrices with a column for each capture event and a row for each individuals

# Full data
gof <- JS.data$ch %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(JS.data))

head(gof)

# Females only
female.gof <- JS.data$ch[JS.data$Sex == "Female"] %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(JS.data[JS.data$Sex == "Female",]))

head(female.gof)


# Males only
male.gof <- JS.data$ch[JS.data$Sex == "Male"] %>%
  strsplit('') %>%
  sapply(`[`) %>%
  t() %>%
  unlist() %>%
  as.numeric %>%
  matrix(nrow = nrow(JS.data[JS.data$Sex == "Male",]))

head(male.gof)

##----Test 1 (Overall test): is there evidence that animals have equal capture----
#probabilities and equal survival? 

#female
overall_CJS(female.gof, rep(1,nrow(JS.data[JS.data$Sex == "Female",])))
# p-value < 0.05, we reject the null hypothesis. There is evidence for lack-to-fit

#Male
overall_CJS(male.gof, rep(1,nrow(JS.data[JS.data$Sex == "Male",])))


#----Test 2 (equal catchability assumption): Does recapture depend on when an animal----
#was first marked?

## a) Test 2.CT: test whether there is a difference in p at t+1 between those 
##    captured and not captured at t when animals are known to be alive because 
##    they are recaptured later in the study

### Female
test2ct_fem <- test2ct(female.gof, rep(1,nrow(JS.data[JS.data$Sex == "Female",]))) 
test2ct_fem

### Male
test2ct_male <- test2ct(male.gof, rep(1,nrow(JS.data[JS.data$Sex == "Male",]))) 
test2ct_male

## b) Test 2.CL: tests if there is a difference in the expected time of next 
##    recapture between individuals captured and not captured at t when animals 
##    are known to be alive

### Female
test2cl_fem <- test2cl(female.gof, rep(1,nrow(JS.data[JS.data$Sex == "Female",]))) 
test2cl_fem

### Male
test2cl_male <- test2cl(male.gof, rep(1,nrow(JS.data[JS.data$Sex == "Male",]))) 
test2cl_male


#----Test 3 (equal survival assumption): Does marking affect survival?----

## a) Test 3.SR: Do individuals with previous marks have different survival 
##    rates than first-time captures?

### Female
test3sr_fem <- test3sr(female.gof, rep(1,nrow(JS.data[JS.data$Sex == "Female",])))
test3sr_fem

### Male
test3sr_male <- test3sr(male.gof, rep(1,nrow(JS.data[JS.data$Sex == "Male",]))) 
test3sr_male


## b) Test 3.SM: For animals seen again, does when they are recaptured depend on
##    whether they were marked on or before time t?

### Female
test3sm_fem <- test3sm(female.gof, rep(1,nrow(JS.data[JS.data$Sex == "Female",])))
test3sm_fem

### Male
test3sm_male <- test3m(male.gof, rep(1,nrow(JS.data[JS.data$Sex == "Male",]))) 
test3sm_male



