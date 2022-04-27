# Seasonal patterns of SARS-CoV-2 transmission in secondary schools: a modelling study


This repository contains the data and code for the manuscript "Seasonal patterns of SARS-CoV-2 transmission in secondary schools: a modelling study" by Thi Mui Pham,  Ilse Westerhof, Martin Bootsma, Mirjam Kretzschmar, Ganna Rozhnova, Patricia Bruijning-Verhagen. 

Ganna Rozhnova and Patricia Bruijning-Verhagen contributed equally to thius work and are co-senior authors. 

## Correspondence
Dr. Thi Mui Pham (thi.mui.pham@posteo.de)

## Abstract

**Background:** The Omicron variant has caused a new wave of SARS-CoV-2 infections worldwide. We explore crucial epidemiological parameters driving seasonal patterns of SARS-CoV-2 transmission in secondary schools and assess various infection control interventions over a 2.5-year time frame. 

**Methods:** We developed an agent-based model parameterised with data from secondary schools in the Netherlands. We modelled the circulation of Omicron assuming a stable introduction rate of infections and accounted for uncertainty in epidemiological parameters describing virus transmissibility, susceptibility to reinfection, vaccine immune escape, and waning of sterilising immunity. We quantified the SARS-CoV-2 health burden defined as number of symptomatic student days. We further evaluated the cost-benefit (number of prevented infected students per absent student) for reactive quarantine interventions, regular screening using antigen tests, and annual booster vaccinations.  

**Findings:** Durability of sterilising immunity is a key parameter that governs temporal SARS-CoV-2 transmission patterns in secondary schools. Our model predicts pronounced within-school seasonal patterns with dominant autumn outbreaks and smaller winter outbreaks and a maximum prevalence of 2.9% (95% CI: 0.7%-6.6%) symptomatic students during infection peaks. Regular screening and annual booster vaccination may reduce the health burden up to 15% (95% CI: 1.5%-27.8%) and have a higher cost-benefit ratio than reactive quarantine interventions (reduction: 4.3%; 95% CI: -10.1% to 17.6%). 

**Interpretation:** Immunity waning will determine the intensity and pattern of SARS-CoV-2 transmission in secondary schools in the medium-term future. If mitigation strategies are needed, screening and annual booster vaccination have the highest cost-benefit by reducing viral transmission with little educational disruption.

## Data
Data from the pilot project is publicly available here. 

## Code
### Model code
The agent-based model was implemented in R (version 4.0.1). The implementation code can be found [here](https://github.com/tm-pham/sarscov2_secondary_school_transmission/tree/master/model_code). 

The code is divided into various parts. A brief explanation of the most important ones can be found below. 

**covid19school_vars_baseline.R**
Parameter values such as time steps, study period, vacation times, reproduction number, vaccine efficacy, test sensitivity, waning probability, class sizes, etc. are set in this file. 

**covid19school_epidemic.R**
Main function that executes all events that take place in each simulation and at each time step (see appendix for a more detailed explanation). 

**covid19school_functions_diseaseChar.R**
Functions related to disease characteristics, i.e. incubation period, proportion of symptomatic infections, relative infectiousness of asymptomatic to symptomatic infections, relative susceptbility and infectiivity of students compared to teachers, realtive susceptibility of vaccinated vs unvaccinated, time-varying infectivity, and probability of transmission per contact. 

**covid19school_init_vars_function.R**
Set up data frames for students and teachers. Needs to be initiliazed at the beginning of each simulation run. 

**covid19school_functions.R**
Auxiliary functions. 

**covid19school_transmission_function.R**
Functions that executes the transmission between infected and susceptible individuals. It accounts for transmission from students, teachers, vaccinated, unvaccinated, direct and aerosol (indirect) transmission. 

**covid19school_quarantine_isolation_function.R**
Functions for isolation of symptomatically infected individuals, as well as quarantine of close contacts and classmates. It also contains the function that entails testing after on day five after the start of quarantine to potentially shorten the quarantine.  

**covid19school_screening_function.R**
Function that entails screening of individuals accounting for adherence and imperfect test sensitivity. 

### Plotting code
The results of the model were analyzed in R (version 4.0.1). The respective code can be found [here](https://github.com/tm-pham/sarscov2_secondary_school_transmission/tree/master/plotting_code)

## Results
Results of the model can be found [here](https://github.com/tm-pham/sarscov2_secondary_school_transmission/tree/master/model_results).







