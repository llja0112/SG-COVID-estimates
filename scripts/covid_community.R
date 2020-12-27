library(EpiNow2)
library(dplyr)

Covid2020 <- read.csv('../data/COVID_SG.csv')

reporting_delay <- estimate_delay(rlnorm(1000,  log(3), 1),
                                  max_value = 15, bootstraps = 1)

generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

Covid2020$New_community_cases <- Covid2020$New_community_surveillance + Covid2020$New_community_isolation
reported_cases <- Covid2020 %>% select(Date_reported, New_community_cases) %>% rename(date=Date_reported, confirm=New_community_cases)

reported_cases$date <- as.Date(reported_cases$date)

estimates <- epinow(reported_cases = reported_cases, 
                    generation_time = generation_time,
                    delays = delay_opts(incubation_period, reporting_delay),
                    rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
                    stan = stan_opts(cores = 4))

names(estimates)

knitr::kable(summary(estimates))

head(summary(estimates, type = "parameters", params = "R"))
