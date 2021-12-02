# Analyzing Infections in BC based on Current Vaccination Rates

This model is modified from [*Vaccine Rollout Strategies: The Case for Vaccinating Essential Workers Early*](https://github.com/nmulberry/essential-workers-vaccine) (Mulberry et al., 2021). The code is modified to have each age group have the option of 0,1 or 2 doses of the vaccine. Rather than vaccinating the population over time, the population is assumed to already be fully vaccinated (minus heistant people). The output will be infections per day for each (age group, vaccination status).

## How to Run
The files from the original model are still included. There are new files (appended with "1dose.R") that serve the purpose of analyzing infections per day. The main file to run is "reopening_strategies1dose.R". Within it, you can update the parameters such as vaccine efficacy (for 1 and 2 dose), hesitancy (those with no vaccine), the covid-19 R value, days to run model, and propotion of population vaccinated (for 1 and 2 dose). 
