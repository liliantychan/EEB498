library(tidyverse)
library(matrixStats)
library(lme4)
library(lmerTest)
library(ggpubr)
library(effects)
# Not sure if we need the following packages
library(splines)
library(gratia)
library(gamm4)
library(readxl)
library(lubridate)
library(MuMIn)
library(merTools)

setwd("/Users/lilianchanty/Library/CloudStorage/OneDrive-UniversityofToronto/Documents/UofT/EEB398+498")

data <- read.csv("2021 Data Lilian.csv")
data$Site <-as.factor(data$Site)
data$Notch <- as.factor(data$Notch)
data$Sex <- as.factor(data$Sex)

summary(data)

# PART 1: Male Data ----
## Select males ----
m_data <- data %>% 
  filter(Sex == "Male") %>% 
  arrange(Notch)

n_distinct(m_data$Notch) # 202 male turtles in the dataset

# Claw measurement: use the longer claw length if both L and Rclaw are measured
m_data$Claw <- rowMaxs(as.matrix(m_data[,25:26]), na.rm = T)
m_data$Claw[m_data$Claw == -Inf] <- NA #Replace -Inf with NA

colnames(m_data)

# Sort by Notch and then Year
m_data <- m_data[order(m_data[,1], m_data[,5]), ]


# Only include males with 1st PL measurement <10 cm, so they likely mature during the mark-recap process
juv <- m_data %>% 
  group_by(Notch) %>% 
  filter(!is.na(midPL..cm.)) %>% #filter out NAs since min(NA,1,2) = NA
  filter(midPL..cm. == min(midPL..cm.)) %>%
  filter(midPL..cm. < 10)

# Only include males with 2 PL measurements
plcount <- m_data %>% 
  filter(!is.na(midPL..cm.))

plcount <- data.frame(table(plcount$Notch))

plx2 <- plcount %>% 
  filter(Freq > 1)

# Only include males with 2 claw measurements
clawcount <- m_data %>% 
  filter(!is.na(Claw))
clawcount <- data.frame(table(clawcount$Notch))

clawx2 <- clawcount %>% 
  filter(Freq > 1)

# Combine all the criteria
m_data <- m_data[m_data$Notch %in% juv$Notch, ]
m_data <- m_data[m_data$Notch %in% plx2[,1], ]
m_data <- m_data[m_data$Notch %in% clawx2[,1], ]

n_distinct(m_data$Notch) # There are 58 turtles available for claw growth model
# write_csv(m_data, "m_data.csv")

# Remove 1409 since it matured at age 13 from REP results
# Run growth model in another R script

## Extract age and size at maturity ----
m_mat <- read.csv("model predicted age_size maturity (13Feb).csv")

m_mat <- m_mat[, c(2,3,4)] # Remove first column

# We want to add zero into individual names s.t. the length is 4 char
#e.g. 987 -> 0987

#This functions identifies whether a string is numbers only
numbers_only <- function(x) {!grepl("\\D", x)}

for (i in 1:nrow(m_mat)){
  ind <- m_mat[i,1]
  if (numbers_only(ind)) {
    m_mat[i,1] <- str_pad(ind, 4, side = "left", pad = "0")
  }
}

# Sort by Notch
m_mat <- m_mat[order(m_mat[,1]),]

# Add birth year to the m_mat dataset
m_mat <- left_join(m_mat, unique(data[, c("Notch","Year.of.birth")]), 
                   by = c("Individual" = "Notch"))

# Change birth year to NA if age at maturity is also NA (since no age at maturity = no known birth year)
for (i in 1:nrow(m_mat)) {
  if (is.na(m_mat[i, "Age_mat"])) {
    m_mat[i, "Year.of.birth"] <- NA
  }
}


# Remove turtles without known birth year from the dataset
m_mat <- m_mat %>% filter(!is.na(Year.of.birth))

nrow(m_mat) # 35 males for analysis
# write_csv(m_mat, "m_mat.csv") # Save results as a csv file

# PART 2: Female Data----

## Select females ----
female <- data %>%
  filter(Sex == "Female") %>%
  arrange(Notch)

n_distinct(female$Notch) # Total: 587 females in the dataset

# Only include females with known birth year
f_juv <- female %>% 
  group_by(Notch) %>% 
  filter(Year == min(Year)) %>% 
  filter(Age <= 4)

# Create a dataframe with Notch, tag, known age, Year of 1st nesting behaviour (+which behaviour) & 1st nesting
f_data <- f_juv %>% 
  select(Notch, Turtle.ID, Known.age, Year.of.birth) %>% 
  distinct(Notch, Turtle.ID, Known.age, Year.of.birth)

nrow(f_data) # 109 female turtles with known birth year

## Old tag/Notch ----
# Since tag and Notch may change over time, we included old tag/Notch in the dataframe as well so that it would be easier when we reviewed the field notebooks
ind_data <- read_csv("Individual Data.csv")

ind_data <- ind_data %>%
  select(`Turtle ID`, Notch, `Old Turtle ID`, `Old Notch`, `PIT Tag`)

f_data <- left_join(f_data, ind_data,
                        by = c("Notch" = "Notch", "Turtle.ID" = "Turtle ID"))

## Nest Data----
# Add the year of first nest to the dataframe
nest_data <- read.csv("First Nest Data.csv")

nest_data <- nest_data %>%
  group_by(Notch) %>%
  filter(Year == min(Year)) %>% # Year of first nest of all turtles
  select(Turtle.ID, Notch, Year, Nest.date, Notes)

nest_data <-  nest_data[nest_data$Notch %in% f_data$Notch, ] #Only turtles with known birth year

f_data <- left_join(f_data, nest,
          by = c("Notch" = "Notch", "Turtle.ID" = "Turtle.ID"))

# Rename Year column
names(f_data)[names(f_data) == 'Year'] <- 'Year 1st nest'

## Gravid Data----
# Add the first year that the each turtle was found gravid to the dataframe
gravid_data <- read.csv("Annual Data.csv")

gravid <- gravid_data %>%
  filter(Gravid == T) %>%
  group_by(Notch) %>%
  filter(Year == min(Year)) %>% 
  select(Turtle.ID, Notch, Year)

gravid <- gravid[gravid$Notch %in% f_data$Notch, ]

f_data <- left_join(f_data, gravid,
                        by = c("Notch" = "Notch", "Turtle.ID" = "Turtle.ID"))

# Rename Year column
names(f_data)[names(f_data) == 'Year'] <- 'Year 1st gravid'

# write_csv(f_data, "f_profile.csv")

# Then we reviewed the field books and include the exact nesting behaviour and year to the dataframe. 
# This step was done in Excel

## Extract age and size at maturity ----
f_profile <- read.csv("f_profile Jul 10.csv") # Read the finalized dataframe

f_profile <- f_profile %>% 
  filter(!is.na(Year_mat) & Regular.nesting == T) %>% 
  select(Notch, Year_mat)

n_distinct(f_profile$Notch) #62 females with regular nesting after maturity


# We want turtles that are captured at least twice in the three years before maturity
# e.g. if matured in 2000, need to be captured >= 2 times in 1997-99

#From the female dataset, extract those 62 with regular nesting after maturity
f_mat <- inner_join(f_profile, female,
                    by = c("Notch" = "Notch"))

f_filter <- f_mat %>% 
  mutate(Yr_diff = Year_mat - Year) %>% # Calculate the year difference from the year of maturity
  select(Notch, Turtle.ID, Year_mat, Year, Known.age, Year.of.birth, Age, Site, 
         midPL..cm., Yr_diff)

# Keep capture records in the three years preceding their estimated year of maturity (i.e. year difference = 1-3)
f_filter <- f_filter %>% 
  filter(Yr_diff <= 3 & Yr_diff >= 1)

# Find out which turtles have regular capture before maturity
## Count the number of times turtles were captured in the three years preceding their estimated year of maturity
fcapx2 <- data.frame(table(f_filter$Notch)) 

fcapx2 <- fcapx2 %>% 
  filter(Freq >= 2) # Only include those captured at least twice in the three years preceding their estimated year of maturity. 

f_mat <- f_mat[f_mat$Notch %in% fcapx2[,1], ]
n_distinct(f_mat$Notch) # There are 54 females for the analysis

# Sort by Notch and then Year
f_mat <- f_mat[order(f_mat[,1], f_mat[,6]), ]

# Extract records during the year of maturity
f_mat <- f_mat %>% 
  filter(Year == Year_mat) %>% 
  filter(!is.na(Age)) 

# From our estimation, one female matured at age 22, which is way too old and is likely a mistake, so we remove it from the analysis
f_mat <- f_mat %>% 
  filter(Age < 20)

# To ensure all turtles in the same cohort have already matured, and the oldest maturing female (exclude the one with age 22) is 18,
#we only included turtles born after 2021-18=2023
f_mat <- f_mat %>% 
  filter(Year.of.birth <= 2003) %>% 
  select(Notch, Year.of.birth, Year, Age, midPL..cm.)

# write.csv(f_mat, "f_mat.csv") # save the final dataset as a csv file

n_distinct(f_mat$Notch) #52 Females for analysis


# PART 3: Size At Maturity (Linear Mixed Models)----
## PART 3a: Male ----

msizemod <- lmer(PL_mat ~ Year.of.birth + (1|Year.of.birth), data = m_mat)
summary(msizemod)
r.squaredGLMM(msizemod)
hist(resid(msizemod))
confint <- confint(fsizemod, oldNames = F)

# Predict the size at maturity for each birth year from the model
m.size.pred <- as.data.frame(effect("Year.of.birth", msizemod, 
                                    xlevels = list(Year.of.birth = seq(from = 1987, to = 2011, by = 1))))


## PART 3b: female ----
fsizemod <- lmer(midPL..cm. ~ Year.of.birth + (1|Year.of.birth), data = f_mat)
summary(fsizemod)
r.squaredGLMM(fsizemod)
hist(resid(fsizemod))
confint <- confint(fsizemod, oldNames = F)

# Predict the size at maturity for each birth year from the model
f.size.pred <- as.data.frame(effect("Year.of.birth", fsizemod, 
                                    xlevels = list(Year.of.birth = seq(from = 1986, to = 2003, by = 1))))



## Plot ----

m_size <- ggplot() +
  geom_point(data = m_mat, aes(x = Year.of.birth, y = PL_mat), size = 0.75) + 
  geom_line(data = m.size.pred, aes(x = Year.of.birth, y = fit), colour = "#3366FF", linewidth = 1.5) +
  geom_ribbon(data = m.size.pred, aes(x = Year.of.birth, ymin = lower,ymax = upper), 
              color = "#D8D8D8", size = 0, alpha = 0.15) + 
  labs(x = "Birth Year", y = "midPL (cm)", title = "(a)") +
  xlim(c(1987,2011)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

m_size

f_size <- ggplot() +
  geom_point(data = f_mat, aes(x = Year.of.birth, y = midPL..cm.), size = 0.75) + 
  geom_line(data = f.size.pred, aes(x = Year.of.birth, y = fit), colour = "#3366FF", linewidth = 1.5) +
  geom_ribbon(data = f.size.pred, aes(x = Year.of.birth, ymin = lower,ymax = upper), 
              color = "#D8D8D8", size = 0, alpha = 0.15) + 
  labs(x = "Birth Year", y = "midPL (cm)", title = "(b)") +
  #scale_y_continuous(breaks= seq(11, 15, by = 0.75)) +
  xlim(c(1985,2003)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.title.y = element_blank())

f_size

# Combine the two plots into the same graph
size_at_mat <- ggarrange(m_size, f_size, ncol = 2, nrow = 1, common.legend = T)
size_at_mat
#ggsave("size at mat.png", size_at_mat)

# PART 4: Age at maturity (Survival Analysis) ----
# In another R script (Survival.R)

# PART 5: Air Temperature----
# Air Temperature trends during the turtle growth season
temp <- read.csv("Air Temperature (1965-2020).csv")

str(temp)

temp <- temp %>% 
  filter(!is.na(Year)) %>% 
  filter(Year >= 1978) %>%# Start of the long-term study
  filter(Month >= 5 & Month <= 8) %>% # Turtle growth season
  select(Date.Time,Year,Month,Day,Max.Temp.C,Mean.Temp.C,Min.Temp.C)

# Calculate the mean daily maximum, mean and minimum temperature for each year
temp.summary <- temp %>% 
  group_by(Year) %>% 
  summarise(Max.temp = mean(Max.Temp.C, na.rm = T), 
            Mean.temp = mean(Mean.Temp.C, na.rm = T), 
            Min.temp = mean(Min.Temp.C, na.rm = T))

## Linear regression ----
lm.max <- lm(Max.temp ~ Year, data = temp.summary)
summary(lm.max)

lm.mean <- lm(Mean.temp ~ Year, data = temp.summary)
summary(lm.mean)

lm.min <- lm(Min.temp ~ Year, data = temp.summary)
summary(lm.min)

## Plot ----
temp.plot <- temp.summary %>% 
  ggplot() +
  geom_point(aes(x = Year, y = Max.temp), shape = 24) +
  geom_point(aes(x = Year, y = Mean.temp), shape = 21, fill = "black") +
  geom_point(aes(x = Year, y = Min.temp), shape = 22) +
  stat_smooth(mapping = aes(x = Year, y = Max.temp), method = lm, colour = "gray23", size = 2) +
  stat_smooth(mapping = aes(x = Year, y = Mean.temp), method = lm, colour = "gray23", size = 2) +
  stat_smooth(mapping = aes(x = Year, y = Min.temp), method = lm, colour = "gray23", size = 2) +
  labs(y = expression("Temperature " ( degree~C))) +
  xlim(1975, 2024) +
  ylim(NA, 28) +
  scale_y_continuous(breaks= seq(5,25,5)) +
  # annotate(geom = "text", x = 2018, y = 27.5,
  #          label = "y = 0.0197x - 17.1") +
  # annotate(geom = "text", x = 2018, y = 26,
  #          label = "paste(italic(R) ^ 2, \" = 0.056\")", parse = T) +
  # annotate(geom = "text", x = 2018, y = 19,
  #          label = "y = 0.0406x - 66.4") +
  # annotate(geom = "text", x = 2018, y = 17.5,
  #          label = "paste(italic(R) ^ 2, \" = 0.30\")", parse = T) +
  # annotate(geom = "text", x = 2018, y = 11.5,
  #          label = "y = 0.0660x - 127") +
  # annotate(geom = "text", x = 2018, y = 10,
#          label = "paste(italic(R) ^ 2, \" = 0.53\")", parse = T) +
theme_classic() +
  theme(
    # axis.title.y = element_blank(),
    #     axis.title.x = element_blank(),
    axis.text.x = element_text(angle=30, hjust=1, 
                               # size=14
    ),
    # axis.text.y = element_text(size=14),
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color=NA))


temp.plot
# ggsave("temp.horizontal.png", temp.plot)