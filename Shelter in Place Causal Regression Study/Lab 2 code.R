library(tidyverse)
library(lubridate)
library(sandwich)
library(lmtest)
library(stargazer)
library(gridExtra)

# Set working directory and read in data files
setwd("~/Lab 2")
stay_at_home <- read.csv("stay_at_home_20210402.csv")
unemployment <- read.csv("unemployment_data_20210402.csv")
closures <- read.csv("closures_and_reopening_20210402.csv")

# Rename important columns to be more useful
stay_at_home <- stay_at_home %>%
  rename(state = State,
         sah_sip_start = Stay.at.home.shelter.in.place,
         religious_exempt = Religious.Gatherings.Exempt.Without.Clear.Social.Distance.Mandate.,
         sah_no_mvmt_restriction = Stay.at.home.order.issued.but.did.not.specifically.restrict.movement.of.the.general.public,
         sah_sip_end = End.stay.at.home.shelter.in.place)
unemployment <- unemployment %>% 
  rename(state = State,
         week_end = Filed.week.ended,
         unemp_percent = Insured.Unemployment.Rate,
         claims_new = Initial.Claims,
         claims_cont = Continued.Claims)
closures <- closures %>% 
  rename(state = State)

# Filter out unnecessary columns and values
stay_at_home <- stay_at_home %>% 
  select(state, sah_sip_start, religious_exempt,
         sah_no_mvmt_restriction, sah_sip_end) %>% 
  filter(state != "Total")
unemployment <- unemployment %>% 
  select(state, week_end, unemp_percent, claims_cont, claims_new) %>% 
  filter(state != "Virgin Islands", state != "Puerto Rico")
closures <- closures %>% filter(state != "Total")

# Wrangle columns into the correct formats
stay_at_home <- stay_at_home %>% 
  mutate(sah_sip_start = mdy(sah_sip_start),
         sah_sip_end = mdy(sah_sip_end),
         has_sah_sip = !is.na(sah_sip_start),
         religious_exempt = as.logical(religious_exempt))
unemployment <- unemployment %>% 
  mutate(week_end = mdy(week_end),
         claims_cont = as.numeric(gsub(",", "", claims_cont)),
         claims_new = as.numeric(gsub(",", "", claims_new)))

# Wrangle unemployment data prior to merging
baseline_unemp <- unemployment %>% 
  filter(week_end >= mdy("02/01/2020"), week_end <= mdy("02/29/2020")) %>% 
  group_by(state) %>% 
  summarise(unemp_feb = mean(unemp_percent))
later_unemp <- unemployment %>% 
  filter(week_end >= mdy("04/25/2020"), week_end <= mdy("06/13/2020")) %>% 
  group_by(state) %>% 
  summarise(unemp_mayjune = mean(unemp_percent))
max_unemp_date <- unemployment %>% 
  group_by(state) %>% 
  arrange(desc(unemp_percent)) %>% 
  summarise(max_unemp_date = first(week_end))
unemployment <- left_join(baseline_unemp, later_unemp, by = "state") %>%
  left_join(., max_unemp_date, by = "state") %>% 
  mutate(delta_unemp_perc = (unemp_mayjune - unemp_feb) / unemp_feb * 100)

closures <- closures %>%
  mutate(schools_1 = ifelse(closures$Closed.K.12.public.schools != "0", 1, 0), 
         daycare_1 = ifelse(closures$Closed.day.cares != "0", 1, 0),
         nursing_1 = ifelse(closures$Banned.visitors.to.nursing.homes != "0", 1, 0),
         nonessential_1 = ifelse(closures$Closed.other.non.essential.businesses != "0", 1, 0),
         gyms_1 = ifelse(closures$Closed.gyms != "0", 1, 0),
         restaurants_1 = ifelse(closures$Closed.restaurants != "0", 1, 0),
         movies_1 = ifelse(closures$Closed.movie.theaters != "0", 1, 0),
         bars_1 = ifelse(closures$Closed.bars != "0", 1, 0),
         casinos_1 = ifelse(closures$Closed.casinos != "0", 1, 0),
         overnight_1 = ifelse(closures$Closed.businesses.overnight != "0", 1, 0),
         gyms_2 = ifelse(closures$Closed.gyms.x2 != "0", 1, 0),
         restaurants_2 = ifelse(closures$Closed.restaurants.x2 != "0", 1, 0),
         movies_2 = ifelse(closures$Closed.movie.theaters.x2 != "0", 1, 0),
         bars_2 = ifelse(closures$Closed.bars.x2 != "0", 1, 0),
         casinos_2 = ifelse(closures$Closed.casinos.x2 != "0", 1, 0),
         hair_2 = ifelse(closures$Closed.hair.salons.barber.shops.x2 != "0", 1, 0),
         restaurants_3 = ifelse(closures$Closed.restaurants.x3 != "0", 1, 0),
         bars_3 = ifelse(closures$Closed.bars.x3 != "0", 1, 0))

closures_summarized <- data.frame(
  state = closures$state,
  closures_one_tot = rowSums(closures %>% select(ends_with("_1"))),
  closures_two_tot = rowSums(closures %>% select(ends_with("_2"))),
  closures_three_tot = rowSums(closures %>% select(ends_with("_3")))
)

data <- left_join(stay_at_home, unemployment, by = "state") %>% 
  left_join(., closures_summarized, by = "state")


# Start creating more variables of interest
data <- data %>% 
  mutate(
    sah_no_mvmt_restriction = ifelse(
      sah_no_mvmt_restriction == "0", FALSE, TRUE))

# Create models
model_1 <- lm(delta_unemp_perc ~ has_sah_sip,
              data=data)
model_2 <- lm(delta_unemp_perc ~ has_sah_sip + sah_no_mvmt_restriction +
                religious_exempt,
              data=data)
model_3 <- lm(delta_unemp_perc ~ has_sah_sip + sah_no_mvmt_restriction +
                religious_exempt + closures_one_tot + closures_two_tot + 
                closures_three_tot,
              data=data)

# Regression table
se.model1 = coeftest(model_1, vcov = vcovHC)[ , "Std. Error"]
se.model2 = coeftest(model_2, vcov = vcovHC)[ , "Std. Error"]
se.model3 = coeftest(model_3, vcov = vcovHC)[ , "Std. Error"]

stargazer(model_1, model_2, model_3,
          title = "Regression Table",
          align = TRUE,
          dep.var.labels = c("Change in Unemployment Rate"), 
          covariate.labels = c(
            "SAH/SIP Order? (Y/N)",
            "SAH/SIP Movement Restrictions? (Y/N)",
            "SAH/SIP Religious Exemptions? (Y/N)",
            "Closures: # Sectors in Phase 1",
            "Closures: # Sectors in Phase 2",
            "Closures: # Sectors in Phase 3"), 
          omit.stat = c("LL", "ser", "f"),
          se=list(se.model1, se.model2, se.model3),
          no.space=TRUE,
          type="latex")

# Add residuals and predictions to `data`
data <- data %>% 
  mutate(
    model_1_preds = predict(model_1), 
    model_1_resids = resid(model_1),
    model_2_preds = predict(model_2),
    model_2_resids = resid(model_2),
    model_3_preds = predict(model_3),
    model_3_resids = resid(model_3)
  )

# Breusch-Pagan test
bptest(model_2)

# Shapiro-Wilks test
shapiro.test(data$model_2_resids)

# Plots for paper and presentation
# Dot plot of SAH/SIP and max unemployment dates
data %>%
  select(state, sah_sip_start, max_unemp_date) %>% 
  gather(metric, value, -state) %>% 
  ggplot(aes(x = value, y = fct_rev(state), color = metric)) +
  geom_point(size = 2) +
  theme_bw() +
  labs(x = "2020 Date", y = "State",
       title = "SAH/SIP Start Dates and Maximum Unemployment Dates by State") +
  scale_color_discrete(labels = c("Date of Maximum\nUnemployment", "SAH/SIP Start Date")) +
  theme(legend.title = element_blank())

# Histogram of change in unemployment rate
resp1 <- data %>% 
  ggplot(aes(x = delta_unemp_perc)) +
  geom_histogram() +
  labs(x = "Change in Unemployment Rate (%)", y = "Count",
       title = "Histogram of Changes in Unemployment Rate") +
  theme_bw()
# Q-Q plot of change in unemployment rate
resp2 <- data %>% 
  ggplot(aes(sample = delta_unemp_perc)) +
  stat_qq(size = 2) +
  stat_qq_line() +
  theme_bw() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
       title = "Normal Q-Q Plot of Changes in Unemployment Rate")
grid.arrange(resp1, resp2, nrow = 2, ncol = 1)

# Residual-predictions plot for main model
data %>% 
  ggplot(aes(model_2_preds, model_2_resids)) + 
  geom_point() + 
  stat_smooth() + 
  theme_bw() +
  labs(x = "Model 2 Predictions", y = "Model 2 Residuals",
       title = "Residuals vs. Predictions Plot for Model 2")

# Residual histogram for main model
data %>% 
  ggplot(aes(model_2_resids)) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Model 2 Residuals", y = "Count",
       title = "Histogram of Residuals for Model 2")
