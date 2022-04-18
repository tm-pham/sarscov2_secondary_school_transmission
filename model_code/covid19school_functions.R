source("covid19school_functions_diseaseChar.R")
source("covid19school_risk_testing_function.R")
source("covid19school_screening_function.R")
source("covid19school_quarantine_isolation_function.R")
source("covid19school_transmission_function.R")
source("covid19school_external_foi_function.R")
source("../plotting_code/covid19school_plotting_functions.R")

# ============================================================================ #
# Packages necessary
# Automatically check whether packages are installed and load them
# ============================================================================ #
check.and.install.pkgs <- function(pkgs, lib_path=NULL){
  new.packages <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if(length(new.packages)) install.packages(new.packages, dependencies = TRUE, lib=lib_path)
  suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
}
# ============================================================================ #
# Transform for plotting
# ---------------------------------------------------------------------------- #
# Input:
# time_steps = Number of time steps in one simulation
# iter =  Number of iterations/simulations
# nn = List with number of S, IA, PS, IS, R, IH, Q 
# Output: 
# Data frame with mean, 95% quantile 
transform.df <- function(time_steps, 
                         iter,       
                         nn, 
                         tol=1e-5){  
  names(nn) <- c("Susceptible", "Presymptomatic", "Symptomatic", "Asymptomatic", 
                 "Recovered", "Isolated", "Quarantined")
  df_mean <- df_cl <- df_cu <- as.data.frame(matrix(time_steps, ncol=1))
  colnames(df_mean) <- colnames(df_cl) <- colnames(df_cu) <- "time"
  for(i in 1:length(nn)){
    df <- as.data.frame(cbind(time_steps,nn[[i]]))
    colnames(df) <- c("time",1:iter)
    df_mean <- cbind(df_mean, sapply(time_steps, function(x) mean(unlist(df[abs(df$time-x)<=tol,-1]))))
    df_cl <- cbind(df_cl, sapply(time_steps, function(x) quantile(unlist(df[abs(df$time-x)<=tol,-1]), probs=0.025)))
    df_cu <- cbind(df_cu, sapply(time_steps, function(x) quantile(unlist(df[abs(df$time-x)<=tol,-1]), probs=0.975)))
  }
  colnames(df_mean) <- colnames(df_cl) <- colnames(df_cu) <- c("time", names(nn))
  
  data_mean <- reshape::melt(df_mean, id="time")
  colnames(data_mean)[3] <- "mean"
  data_cl <- reshape::melt(df_cl, id="time")
  colnames(data_cl)[3] <- "ci_lower"
  data_cu <- reshape::melt(df_cu, id="time")
  colnames(data_cu)[3] <- "ci_upper"
  
  data_ci <- merge(data_cl, data_cu)
  data <- merge(data_mean, data_ci)
  
  return(data)
}

# ============================================================================ #
# Generate day of the week 
# ---------------------------------------------------------------------------- #
# Input:
# time_steps = Numeric vector with time steps of study period wrt 1 day
# day_names = String vector with names of days to be used
# ---------------------------------------------------------------------------- #
# Output: 
# time_names = String vector with name
# ============================================================================ #
day.of.the.week <- function(time_steps, day_names=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")){
  time_names <- NULL
  fl_time_steps <- floor(time_steps)
  unique_fl_time <- unique(fl_time_steps)
  for(t in unique_fl_time){
    n_rep <- sum(fl_time_steps==t)
    time_names <- c(time_names, rep(day_names[ifelse(t%%7>0, t%%7, 7)], times=n_rep))
  }
  return(time_names)
}

# ============================================================================ #
# Functions to create when contacts will take place
# (based on the time steps)
contacts.in.school <- function(time_steps, time_names, t_school=8/24){
  cont_in_school <- sapply(1:length(time_steps), function(i) ifelse(round(time_steps[i]%%1,2)<round(t_school,1) & !time_names[i]%in%c("Saturday","Sunday"), 1, 0))
  return(cont_in_school)
}
contacts.leisure <- function(time_steps, time_names, t_school=8/24, t_out_school=16/24){
  cont_1 <- sapply(1:length(time_steps), function(i) ifelse(time_steps[i]%%1>round(t_school,1) & round(time_steps[i],1)%%1<round(t_out_school,2) & !time_names[i]%in%c("Saturday","Sunday"), 1, 0))
  cont_2 <- sapply(1:length(time_steps), function(i) ifelse(round(time_steps[i]%%1,1)<=round(t_out_school,2) & time_names[i]%in%c("Saturday","Sunday"), 1, 0))
  cont_out_school <- as.numeric(sapply(1:length(cont_1), function(x) cont_1[x]| cont_2[x]))
  return(cont_out_school)
}
no.contacts <- function(time_steps, t_out_school=16/24){
  no_contacts <- sapply(1:length(time_steps), function(i) ifelse(round(time_steps[i]%%1,1)>t_out_school,1,0))
  return(no_contacts)
}


# ============================================================================ #
# Assign teachers to subjects
assign.teachers <- function(df_teacher, grades, subjects, id=NULL, initial=F){
  if(initial) id <- 1
  ind <- which(df_teacher$grade%in%grades & df_teacher$subject%in%subjects)
  remaining <- 1:nrow(df_teacher[ind,])
  while(length(remaining)>0){
    temp <- sample(remaining, size = length(subjects))
    remaining <- setdiff(remaining, temp)
    df_teacher[ind[temp], "id"] <- id
    id <- id + 1
  }
  return(list(df_teacher=df_teacher, 
              id=id))
}

# ============================================================================ #
# Update susceptible contacts
# ---------------------------------------------------------------------------- #
# Input: 
# t_i = current time
# df_history = data frame with information per day
# susc_list = list of susceptible contacts per individual
# Output:
# Updated susc_list
# ============================================================================ #
update.susc.contacts <- function(t_i, df_history, susc_list, tol=1e-5){
  susc_ids <- df_history[abs(df_history$time-t_i)<=tol & df_history$state=="S" & df_history$pres==1, "id"]
  return(lapply(susc_list, function(x) intersect(unlist(x), susc_ids)))
}


# ============================================================================ #
# Assign screening adherence
# ============================================================================ #
assign.screening.adherence <- function(df_agent, screening_adherence=0.3, vacc_scr_corr=1, only.eligible=T){
  vaccinated2_ids <- df_agent$id[df_agent$vaccinated_2==1]   # Fully vaccinated 
  unvaccinated2_ids <- df_agent$id[df_agent$vaccinated_2==0] # Not fully vaccinated (partially or unvaccinated)
  if(only.eligible){
    # Only eligible population is willing to test
    # Always negative correlation, i.e., only partially and unvaccinated population is willing to test
    n_test_adherent <- ceiling(screening_adherence*length(unvaccinated2_ids))
    df_agent$adherence[df_agent$id%in%unvaccinated2_ids] <- sample(c(0,1), size=length(unvaccinated2_ids), prob=c(1-screening_adherence, screening_adherence), replace=T)
    df_agent$adherence[!df_agent$id%in%unvaccinated2_ids] <- 0  
  }else{
    n_test_adherent <- ceiling(screening_adherence*length(df_agent$id))
    if(vacc_scr_corr==1){ # Vaccinated individuals are more likely to participate in self-testing
      if(length(vaccinated2_ids) < n_test_adherent){
        df_agent$adherence[df_agent$id%in%vaccinated2_ids] <- 1
        n_remaining <- n_test_adherent - length(vaccinated2_ids)
        prop_remaining <- n_remaining/length(unvaccinated2_ids)
        df_agent$adherence[!df_agent$id%in%vaccinated2_ids] <- sample(c(0,1), size=length(unvaccinated2_ids), prob=c(1-prop_remaining, prop_remaining), replace=T)
      }else{
        prop_screening <- n_test_adherent/length(vaccinated2_ids)
        df_agent$adherence[df_agent$id%in%vaccinated2_ids] <- sample(c(0,1), size=length(vaccinated2_ids), prob=c(1-prop_screening, prop_screening), replace=T)
        df_agent$adherence[!df_agent$id%in%vaccinated2_ids] <- 0    
      }
    }
    
    if(vacc_scr_corr==-1){ # Unvaccinated individuals are more likely to participate in self-testing
      if(length(unvaccinated2_ids) < n_test_adherent){
        df_agent$adherence[df_agent$id%in%unvaccinated2_ids] <- 1
        n_remaining <- n_test_adherent - length(unvaccinated2_ids)
        prop_remaining <- n_remaining/length(vaccinated2_ids)
        df_agent$adherence[!df_agent$id%in%unvaccinated2_ids] <- sample(c(0,1), size=length(vaccinated2_ids), prob=c(1-prop_remaining, prop_remaining), replace=T)
      }else{
        prop_screening <- n_test_adherent/length(unvaccinated2_ids)
        df_agent$adherence[df_agent$id%in%unvaccinated2_ids] <- sample(c(0,1), size=length(unvaccinated2_ids), prob=c(1-prop_screening, prop_screening), replace=T)
        df_agent$adherence[!df_agent$id%in%unvaccinated2_ids] <- 0  
      }
    }
    
    if(vacc_scr_corr==0){ # Adherence and compliance independent from vaccination status
      df_agent$adherence <- sample(c(0,1), size=length(df_agent$id), prob=c(1-screening_adherence, screening_adherence), replace=T)
    }
  }
  return(df_agent)
}

# ============================================================================ #
# Returning to susceptible status after recovery
# ---------------------------------------------------------------------------- #
# 1. For each recovered individual, draw a previous time of infection 
#    (based on the incidence of the past waves in NL)
# 2. For each recovered individual, flip a coin of whether state is changed 
#    from "R"to "S" according to the daily probability of reinfection based 
#    on literature
# 3. For each vaccinated individual who has not been infected, change back
#    susceptibility (no reduced susc due to vaccination) according to 
#    daily probability of reinfection based on literature
# ============================================================================ #
reinfection <- function(t_i, 
                        df_agent, 
                        df_history,
                        df_time_inf, 
                        df_prob_reinfection,
                        df_prob_reinfection_2=NULL,
                        lag_reinfection=0, 
                        init_prev_inf = F, 
                        rel_susc_waning = 1.0, 
                        slower_natural_waning = T){
  if(nrow(df_agent)==0) print("In reinfection function: df_agent is empty.")
  if(nrow(df_history)==0) print("In reinfection function: df_history is empty.")
  ### Determine number and IDs of recovered individuals
  n_recovered <- sum(df_agent$state=="R")
  id_recovered <- df_agent[df_agent$state=="R", "id"]
  
  reinfected_ids <- reinfected_rec_ids <- reinfected_vacc_ids <- NULL
  
  ### If initial call: Assign time of previous infection (before study period)
  if(init_prev_inf){
    df_agent[df_agent$state=="R", "t_prev_inf"] <- as.numeric(sample(df_time_inf$time, size=n_recovered, prob=df_time_inf$prob), replace=T) 
  }
  
  ### RECOVERED
  # Go through all recovered individuals and determine whether they become 
  # susceptible again or not
  for(id in id_recovered){
    ind <- which(df_agent$id==id)
    not_vaccinated <- df_agent[ind, "vaccinated_2"]==0
    # Determine how many reinfections already occurred
    if(df_agent[ind, "prev_inf"]){
      n_inf <- 0
      t_inf <- "t_prev_inf"
    }else{
      n_inf <- df_agent[ind, "n_inf"]
      t_inf <- paste0("t_inf_", n_inf)
    }
    if(is.na(df_agent[ind, eval(t_inf)])){
      print("Reinfection: t_inf is NA!")
      print(df_agent[ind,])
    }
    # Determine the probability to get reinfected
    if(t_i > df_agent[ind, eval(t_inf)]+lag_reinfection){
      t_waning <- floor(t_i) - (df_agent[ind, eval(t_inf)]+lag_reinfection)
      if(n_inf>=2 || df_agent[ind, "waning"]==1 || not_vaccinated*slower_natural_waning){
        prob_reinf <- df_prob_reinfection_2[abs(df_prob_reinfection_2$time-t_waning)<=tol, "prob"]
        if(length(prob_reinf)!=1){
          print("Probability of reinfection after second infection or after vaccination.")
          print(paste0("t_waning = ", t_waning))
          print(paste0("Probability of reinfection = "), prob_reinf)
        }
      }else{
        prob_reinf <- df_prob_reinfection[abs(df_prob_reinfection$time-t_waning)<=tol, "prob"]
        if(length(prob_reinf)!=1){
          print("Probability of reinfection after previous or first infection.")
          print(paste0("t_waning = ", t_waning))
          print(paste0("Probability of reinfection = "), prob_reinf)
        }
      }
      # Waning is implemented as all or nothing:
      # Flip a coin whether recovered individual is susceptible again
      change_state <- sample(c(0,1), size=1, prob=c(1-prob_reinf, prob_reinf))==1
      reinfected_ids <- c(reinfected_ids, id[change_state])
      df_agent[ind, "state"] <- ifelse(change_state, "S", "R")
      # If individual is susceptible again, the new susceptibility 
      df_agent[ind, "susc"] <- ifelse(change_state, min(rel_susc_waning*df_agent[ind, "init_susc"], 1), df_agent[ind, "susc"])
      
      if(change_state){
        reinfected_rec_ids <- c(reinfected_rec_ids, id)
        # Update df_history
        ind_history <- df_history$id==id & df_history$time+tol>=t_i
        df_history[ind_history, "state"] <- rep("S", sum(ind_history))
        df_agent[ind, "waning"] <- 1
        df_history[ind_history, "waning"] <- rep(1, sum(ind_history))
        df_history[ind_history, "susc"] <-  rep(df_agent[ind, "susc"], sum(ind_history))
        ### Test
        susc_ids <- df_history[abs(df_history$time-t_i)<=tol & df_history$state=="S" & df_history$pres==1, "id"]
        if(!id[change_state]%in%susc_ids){
          print(paste0("In reinfection (recovered): id = ", id, " is not in susceptible ids. t_i = ", t_i))
          print(df_history[ind_history[1:4], ])
        }
      }
    }
  }
  
  ### Vaccinated
  # Go through all vaccinated and determine their probability of reinfection
  # Note that t_vacc_2 may get overwritten by new vaccination date of booster dose
  id_vaccinated <- setdiff(df_agent[df_agent$vaccinated_2==1 & df_agent$state=="S", "id"], id_recovered)
  id_vaccinated <- intersect(id_vaccinated, df_agent[df_agent$waning==0,"id"])
  for(id in id_vaccinated){
    ind <- which(df_agent$id==id)
    if(t_i > df_agent[ind, "t_vacc_2"] + lag_reinfection){
      t_waning <- floor(t_i) - (df_agent[ind, "t_vacc_2"]+lag_reinfection)
      prob_reinf <- df_prob_reinfection[abs(df_prob_reinfection$time-t_waning)<=tol, "prob"]
      if(length(prob_reinf)!=1){
        print(paste0("Probability of reinfection = "), prob_reinf)
      }
      change_state <- sample(c(0,1), size=1, prob=c(1-prob_reinf, prob_reinf))==1
      reinfected_ids <- c(reinfected_ids, id[change_state])
      df_agent[ind, "susc"] <- ifelse(change_state, min(rel_susc_waning*df_agent[ind,"init_susc"], 1), df_agent[ind, "susc"])
      # Update df_history
      if(change_state){
        reinfected_vacc_ids <- c(reinfected_vacc_ids, id)
        ind_history <- df_history$id==id & df_history$time+tol>=t_i
        df_history[ind_history, "state"] <- rep("S", sum(ind_history))
        df_history[ind_history, "vacc_effective"] <- rep(0, sum(ind_history))
        df_history[ind_history, "waning"] <- rep(1, sum(ind_history))
        df_history[ind_history, "susc"] <-  rep(df_agent[ind, "susc"], sum(ind_history))
        df_agent[ind, "waning"] <- 1
        
        ### Test
        susc_ids <- df_history[abs(df_history$time-t_i)<=tol & df_history$state=="S" & df_history$pres==1, "id"]
        if(!id%in%susc_ids){
          print(paste0("In reinfection (vaccinated): id = ", id, " is not in susceptible ids. t_i = ", t_i))
          print(df_history[ind_history[1:4], ])
        }
      }


    }
  }
  
  return(list(df_agent = df_agent, 
              df_history = df_history, 
              reinfected_rec_ids = reinfected_rec_ids,
              reinfected_vacc_ids = reinfected_vacc_ids,
              reinfected_ids = reinfected_ids))
}




# ============================================================================ #
# Reduced susceptibility due to vaccination
# ---------------------------------------------------------------------------- #
# Accounts for lag of vaccine effectiveness and time of vaccination for 1st dose
# For second dose, this is already accounted for when vaccinated_2 was set 
reduced.susc.vacc <- function(t_i, 
                              df_agent, 
                              curr_susc_ids,  
                              vacc_eff_1 = c(0.57, 0.70),
                              vacc_eff_2 = c(0.85, 0.93), 
                              vacc_eff_lag=c(14,14),
                              tol=1e-5){
  vacc_vec <- rep(1, length(curr_susc_ids))
  # First dose
  # Students that are currently "susceptible" and have 1st dose but not 2nd dose
  curr_susc_vacc1_ids <- df_agent[df_agent$id%in%curr_susc_ids & df_agent$vaccinated_1==1 & df_agent$vaccinated_2==0,"id"]
  # Students that were vaccinated at least vacc_eff_lag ago 
  if(length(curr_susc_vacc1_ids)>0){
    ind1 <- which(df_agent$id%in%curr_susc_vacc1_ids) 
    if(vacc_eff_lag[1]!=0) ind2 <- which(t_i + tol >= (df_agent$t_vacc_1 + vacc_eff_lag[1]))
    else ind2 <- ind1

    ind <- intersect(ind1, ind2)
    id_vacc_1 <- df_agent[ind , "id"]
    ind_vacc_1 <- which(curr_susc_ids%in%id_vacc_1)
    vacc_vec_1 <- susc.vacc.scale(length(id_vacc_1), min=1-vacc_eff_1[2], max=1-vacc_eff_1[1]) # Vector with reduced susceptibility for each susceptible student
    # Set susceptibility values
    vacc_vec[ind_vacc_1] <- vacc_vec_1
  }
  # Second dose
  curr_susc_vacc2_ids <- df_agent[df_agent$id%in%curr_susc_ids & df_agent$vaccinated_2==1,"id"]
  if(length(curr_susc_vacc2_ids)>0){
    ind1 <- which(df_agent$id%in%curr_susc_vacc2_ids)
    if(vacc_eff_lag[2]!=0) ind2 <- which(t_i + tol >= (df_agent$t_vacc_2 + vacc_eff_lag[2]))
    else ind2 <- ind1

    ind <- intersect(ind1, ind2)
    id_vacc_2 <- df_agent[ind, "id"]
    ind_vacc_2 <- which(curr_susc_ids%in%id_vacc_2)
    vacc_vec_2 <- susc.vacc.scale(length(id_vacc_2), min=1-vacc_eff_2[2], max=1-vacc_eff_2[1])
    # Set susceptibility values
    vacc_vec[ind_vacc_2] <- vacc_vec_2
  }
  return(vacc_vec)
}



# ============================================================================ #
# SYMPTOM ONSET
# ---------------------------------------------------------------------------- #
# Input:
# df_history = dataframe with information per day
# df_agent = dataframe with information per individual
# new_inf_ids = ids of newly infected individuals
# prop_symp = Proportion of symptomatically infected individuals
# Output:
# Updated df_history and df_agent
# ============================================================================ #
symptom.onset <- function(df_history, df_agent, new_inf_ids, teacher=F, vaccination_flag=F, scaling_factor=0.8, tol=1e-5){
  # Sample probabilities of being symptomatically infected (applied to newly infecteds)
  prop_symp_vec <- prop.symp(n=length(new_inf_ids), teacher=teacher, vaccination_flag=vaccination_flag, scaling_factor=scaling_factor)
  if(any(is.na(prop_symp_vec))){
    print(paste0("NA in symptom.onset function. scaling factor =", scaling_factor))
  }
  symp_inf <- sapply(prop_symp_vec, function(x) sample(c(0,1), size=1, prob=c(1-x, x)))
  symp_ids <- new_inf_ids[symp_inf==1]
  nonsymp_ids <- new_inf_ids[symp_inf==0]
  
  ### Update data frames for symptomatically infected 
  if(length(symp_ids)>0){
    ind_symp <- which(df_agent$id%in%symp_ids)
    # Determine number of infection
    n_inf <- df_agent[ind_symp, "n_inf"]
    t_inc <- paste0("t_inc_", n_inf)
    # Set infection state
    df_agent[ind_symp, "state"] <- "IS"
    # Set incubation time in df_history
    # Sample incubation time
    df_agent[ind_symp, "t_inc"] <- inc.period(length(ind_symp))
    # Set incubation time in df_history
    inc_days <- max(0, df_agent[ind_symp, "t_inf"]) + df_agent[ind_symp, "t_inc"]
    t_inf <- df_agent[ind_symp, "t_inf"]
    for(x in 1:length(symp_ids)){
      df_agent[ind_symp[x], "n_inc"] <- df_agent[ind_symp[x], "n_inc"] + 1
      df_agent[ind_symp[x], eval(t_inc[x])] <- df_agent[ind_symp[x], "t_inc"]
      df_history[df_history$id==symp_ids[x] & df_history$time+tol>=t_inf[x], "state"] <- "PS"
      ind_temp <- which(df_history$id==symp_ids[x] & df_history$time+tol>=inc_days[x])
      df_history[ind_temp, "state"] <- rep("IS", length(ind_temp))
    }
  }
  ### Update data frames for asymptomatically infected 
  if(length(nonsymp_ids)>0){
    ind_nonsymp <- which(df_agent$id%in%nonsymp_ids)
    # Set infection state in df_agent
    df_agent[df_agent$id%in%nonsymp_ids, "state"] <- "IA"
    # Set infection state in df_history
    t_inf <- df_agent[ind_nonsymp, "t_inf"]
    for(x in 1:length(nonsymp_ids)){
      n_inf <- df_agent[ind_nonsymp[x], "n_inf"]
      df_agent[ind_nonsymp[x], eval(paste0("t_inf_", n_inf))] <- t_inf[x]
      ind_temp <- which(df_history$id==nonsymp_ids[x] & df_history$time+tol>=t_inf[x])
      df_history[ind_temp, "state"] <- rep("IA", length(ind_temp))
    }
  }
  return(list(df_history = df_history,
              df_agent = df_agent))
}

# ============================================================================ #
# Recovery
# ---------------------------------------------------------------------------- #
# Input:
# t_i = current time
# df_history = data frame with information per day
# df_agent = data frame with information per individual
# rec_time = recovery time
# Output:
# Updated df_history and df_agent  
# ============================================================================ #
recovery <- function(t_i, df_history, df_agent, rec_time=c(14, 10), tol=1e-5){
  # curr_infected can be IS, IA
  recovered_ids <- unique(df_history[abs(df_history$time-t_i)<=tol & df_history$state=="R", "id"])
  new_recovered_ids <- df_agent[df_agent$id%in%recovered_ids & df_agent$state!="R", "id"]
  df_agent[df_agent$id%in%new_recovered_ids, "state"] <- "R"
  # ind_hist <- intersect(which(df_history$id%in%new_recovered_ids), which(abs(df_history$time-t_i)<=tol))
  # df_history[ind_hist, "state"] <- "R"
  df_agent[df_agent$id%in%new_recovered_ids, "recovered"] <- df_agent[df_agent$id%in%new_recovered_ids, "recovered"] + 1
  
  return(list(df_agent = df_agent,
              df_history = df_history, 
              recovered_ids = new_recovered_ids))
}

# recovery <- function(t_i, df_history, df_agent, rec_time=c(14, 10), tol=1e-5){
#   # curr_infected can be IS, IA
#   curr_inf_ids <- unique(df_history[df_history$state%in%c("IA","IS") & abs(df_history$time-t_i)<=tol,"id"])
#   # curr_inf_ids <- intersect(df_agent[df_agent$recovered==0, "id"], curr_inf_ids)
#   
#   curr_inf_vacc_ids <- intersect(curr_inf_ids, df_agent[df_agent$vaccinated_2==1, "id"])
#   curr_inf_unvacc_ids <- setdiff(curr_inf_ids, curr_inf_vacc_ids)
#   
#   if(length(curr_inf_unvacc_ids)>0){
#     ind_inf_unvacc <- which(df_agent$id%in%curr_inf_unvacc_ids)
#     ind_inf_unvacc_1 <- ind_inf_unvacc[which(df_agent[ind_inf_unvacc, "n_inf"]==1)] # Those for which it's their 1st infection (during the study period)
#     inf_unvacc_ids_1 <- df_agent[ind_inf_unvacc_1, "id"]
#     ind_inf_unvacc_2 <- ind_inf_unvacc[which(df_agent[ind_inf_unvacc, "n_inf"]==2)] # Those with secondary infection
#     inf_unvacc_ids_2 <- df_agent[ind_inf_unvacc_2, "id"]
#     recovered_unvacc_ids <- NULL
#     
#     if(length(inf_unvacc_ids_1)>0){
#       # inc_time_1 <- sapply(df_agent[df_agent$id%in%inf_unvacc_ids_1, "t_inc_1"], function(x) ifelse(x>=0 & !is.na(x), x, 0))
#       # inf_days_1 <- df_agent[df_agent$id%in%inf_unvacc_ids_1, "t_inf_1"] + inc_time_1
#       inf_days_1 <- df_agent[df_agent$id%in%inf_unvacc_ids_1, "t_inf_1"]
#       recovered_unvacc_ids <- c(recovered_unvacc_ids, inf_unvacc_ids_1[sapply(inf_days_1, function(x) t_i+tol>=x+rec_time[1])])    
#     }
#     
#     if(length(inf_unvacc_ids_2)>0){
#       # inc_time_2 <- sapply(df_agent[df_agent$id%in%inf_unvacc_ids_2, "t_inc_2"], function(x) ifelse(x>=0 & !is.na(x), x, 0))
#       # inf_days_2 <- df_agent[df_agent$id%in%inf_unvacc_ids_2, "t_inf_2"] + inc_time_2
#       inf_days_2 <- df_agent[df_agent$id%in%inf_unvacc_ids_2, "t_inf_2"]
#       recovered_unvacc_ids <- c(recovered_unvacc_ids, inf_unvacc_ids_2[sapply(inf_days_2, function(x) t_i+tol>=x+rec_time[1])])    
#     }
#     
#     df_agent[df_agent$id%in%recovered_unvacc_ids, "state"] <- "R"
#     df_history[df_history$id%in%recovered_unvacc_ids & df_history$time+tol>=t_i,"state"] <- "R"
#     # df_agent[df_agent$id%in%recovered_unvacc_ids, "recovered"] <- rep(1, length(recovered_unvacc_ids))
#     df_agent[df_agent$id%in%recovered_unvacc_ids, "recovered"] <- df_agent[df_agent$id%in%recovered_unvacc_ids, "recovered"] + 1 
#   }else recovered_unvacc_ids <- NULL
#   
#   if(length(curr_inf_vacc_ids)>0){
#     ind_inf_vacc <- which(df_agent$id%in%curr_inf_vacc_ids)
#     ind_inf_vacc_1 <- ind_inf_vacc[which(df_agent[ind_inf_vacc, "n_inf"]==1)] # Those for which it's their 1st infection (during the study period)
#     inf_vacc_ids_1 <- df_agent[ind_inf_vacc_1, "id"]
#     ind_inf_vacc_2 <- ind_inf_vacc[which(df_agent[ind_inf_vacc, "n_inf"]==2)] # Those with secondary infection
#     inf_vacc_ids_2 <- df_agent[ind_inf_vacc_2, "id"]
#     recovered_vacc_ids <- NULL
#     
#     if(length(inf_vacc_ids_1)>0){
#       # inc_time_1 <- sapply(df_agent[df_agent$id%in%inf_vacc_ids_1, "t_inc_1"], function(x) ifelse(x>=0 & !is.na(x), x, 0))
#       # inf_days_1 <- df_agent[df_agent$id%in%inf_vacc_ids_1, "t_inf_1"] + inc_time_1  
#       inf_days_1 <- df_agent[df_agent$id%in%inf_vacc_ids_1, "t_inf_1"]  
#       recovered_vacc_ids <- c(recovered_vacc_ids, inf_vacc_ids_1[sapply(inf_days_1, function(x) t_i+tol>=x+rec_time[2])])
#     }
#     
#     if(length(inf_vacc_ids_2)>0){
#       # inc_time_2 <- sapply(df_agent[df_agent$id%in%inf_vacc_ids_2, "t_inc_2"], function(x) ifelse(x>=0 & !is.na(x), x, 0))
#       # inf_days_2 <- df_agent[df_agent$id%in%inf_vacc_ids_1, "t_inf_2"] + inc_time_2
#       inf_days_2 <- df_agent[df_agent$id%in%inf_vacc_ids_2, "t_inf_2"]
#       recovered_vacc_ids <- c(recovered_vacc_ids, inf_vacc_ids_2[sapply(inf_days_2, function(x) t_i+tol>=x+rec_time[2])])
#     }
#     df_agent[df_agent$id%in%recovered_vacc_ids, "state"] <- "R"
#     df_history[df_history$id%in%recovered_vacc_ids & df_history$time+tol>=t_i,"state"] <- "R"
#     # df_agent[df_agent$id%in%recovered_vacc_ids, "recovered"] <- rep(1, length(recovered_vacc_ids))
#     df_agent[df_agent$id%in%recovered_vacc_ids, "recovered"] <- df_agent[df_agent$id%in%recovered_vacc_ids, "recovered"] + 1
#   }else recovered_vacc_ids <- NULL
# 
#   recovered_ids <- c(recovered_unvacc_ids, recovered_vacc_ids)
#   
#   return(list(df_agent = df_agent,
#               df_history = df_history, 
#               recovered_ids = recovered_ids))
# }

# Testing function
testing <- function(t_i, df_history, df_agent, curr_noninf, curr_inf, tol=1e-5){
  FP_ind <- pos_ind <- pos_tested <- pos_tested_ids <- FP_ids <- probs <- NULL
  # False positives among non-infected individuals
  # Accounting for imperfect specificity
  if(length(curr_noninf$id)>0){
    FP <- sample(c(0,1), size=length(curr_noninf$id), prob=c(spec,1-spec), replace=T)
    FP_ids <- curr_noninf$id[FP==1]
    FP_ind <- unlist(lapply(FP_ids, function(x) which(df_history$id==x & df_history$time+tol>=t_i & df_history$time<=t_i+fp_iso_time+tol)))
  }
  # Testing of infected individuals
  # Accounting for imperfect sensitivity
  if(length(curr_inf$id)>0){
    curr_inf_ids <- curr_inf$id
    ind_curr_inf_ids <- which(df_agent$id%in%curr_inf_ids)
    t_inf <- df_agent[ind_curr_inf_ids, "t_inf"]
    t_since_inf <- t_i-t_inf
    probs <- predict(test_sens_fun, ifelse(t_since_inf<0, 0, t_since_inf))$y
    probs <- ifelse(probs>1, 1, probs)
    probs <- ifelse(probs<0, 0, probs)
    pos_tested <- sapply(probs, function(x) sample(c(0,1), size=1, prob = c(1-x, x)))
    pos_tested_ids <- curr_inf_ids[pos_tested==1]
    pos_ind <- unlist(lapply(pos_tested_ids, function(x) which(df_history$id==x & df_history$time+tol>=t_i & df_history$time<=t_i+iso_time+tol)))
  }
  return(list(pos_ind=pos_ind, 
              FP_ind=FP_ind, 
              pos_tested=pos_tested, 
              pos_tested_ids=pos_tested_ids, 
              FP_ids = FP_ids,
              probs=probs))
}


# ============================================================================ #
# Function to sample contacts of students per day
# NOT USED AT THE MOMENT
# ============================================================================ #
sample.contacts <- function(t_i, df_history, tol=1e-5){
  cont_close <- cont_class <- cont_grade <- cont_other_grades <- cont_out_school <- list()
  susc_close <- susc_class <- susc_grade <- susc_other_grades <- susc_out_school <- list()
  student_ids <- unique(df_agent$id)
  remove_ids <- unlist(df_history %>% filter(abs(time-t_i)<=tol & state!="S") %>% summarize(id))
  for(i in 1:length(student_ids)){
    id <- student_ids[i]
    row <- which(df_agent$id==id)
    class <- df_agent[row, "class"]
    grade <- df_agent[row, "grade"]
    students_class <- df_agent[df_agent$class==class & df_agent$grade==grade, "id"] # Id of students that are in the same class
    students_grade <- setdiff(df_agent[df_agent$grade==grade, "id"], students_class) # Id of students that are in the same grade but not in the same class
    students_class <- setdiff(students_class, id) # Remove the current student from students_class
    
    cont_close[[i]] <- sample(students_class, n_cont_close)
    susc_close[[i]] <- setdiff(cont_close[[i]], remove_ids)
    cont_class[[i]] <- sample(setdiff(students_class, cont_close[[i]]), n_cont_class[grade])
    susc_class[[i]] <- setdiff(cont_class[[i]], remove_ids)
    cont_grade[[i]] <- sample(students_grade, n_cont_grade)
    susc_grade[[i]] <- setdiff(cont_grade[[i]], remove_ids)
    cont_out_school[[i]] <- sample(c(students_class, students_grade), n_cont_out_school)
    susc_out_school[[i]] <- setdiff(cont_out_school[[i]], remove_ids)
  }
  names(cont_close) <- names(cont_class) <- names(cont_grade) <- names(cont_out_school) <- student_ids
  names(susc_close) <- names(susc_class) <- names(susc_grade) <- names(susc_out_school) <- student_ids
  
  return(list(cont_close=cont_close, 
              cont_class=cont_class, 
              cont_grade=cont_grade, 
              cont_out_school=cont_out_school,
              susc_close=susc_close, 
              susc_class=susc_class, 
              susc_grade=susc_grade, 
              susc_out_school=susc_out_school))
}

