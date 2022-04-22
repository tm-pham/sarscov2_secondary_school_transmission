# ============================================================================ #
# Introductions from community
# ============================================================================ #
external.introductions <- function(t_i, 
                                   df_history, 
                                   df_agent, 
                                   external_prob=0.0006341289, 
                                   teacher=F,
                                   vaccination_flag=T, 
                                   vacc_eff_1 = c(0.57, 0.70),
                                   vacc_eff_2 = c(0.85, 0.93),
                                   vacc_eff_lag = c(14,14),
                                   scaling_factor = 0.8,
                                   scaling_constant = F,
                                   intro_fix_per_week=NULL,
                                   rec_time = c(14, 10),
                                   tol=1e-5){
  external_inf_ids <- NULL
  # Determine individuals that are currently susceptible
  curr_susc_ids <- unique(df_history[abs(df_history$time-t_i)<=tol & df_history$state=="S" & df_history$iso_state=="P", "id"])
  if(length(curr_susc_ids)>0){
    # Accounts for reduced susceptibility due to vaccinationa and being a student
    susc_scale_vec <- df_agent[df_agent$id%in%curr_susc_ids, "susc"] 
    if(is.null(intro_fix_per_week)){ # Introductions according a set probability 
      # Randomly assign external infections from community 
      external_inf <- sapply(1:length(curr_susc_ids), function(x) sample(c(0,1), size=1, prob=c(1-susc_scale_vec[x]*external_prob, susc_scale_vec[x]*external_prob)))
      external_inf_ids <- curr_susc_ids[external_inf==1]
    }else{ # Fixed number of introductions from community PER WEEK
      if(length(curr_susc_ids)==1) external_inf_ids <- curr_susc_ids
      else external_inf_ids <- sample(curr_susc_ids, size=1, prob=susc_scale_vec)
    }
    if(length(external_inf_ids)>0){
      ind_agent <- which(df_agent$id%in%external_inf_ids)
      # Update number of infection 
      df_agent[ind_agent, "n_inf"] <- df_agent[ind_agent, "n_inf"] + 1
      n_inf <- df_agent[ind_agent, "n_inf"]
      prev_inf <- df_agent[ind_agent, "prev_inf"]==T
      vaccination_flags <- df_agent[ind_agent, "vaccinated_2"]
      t_inf <- paste0("t_inf_", n_inf)
      location <- paste0("location_", n_inf)
      source <- paste0("source_", n_inf)
      # Set current infection time and location of infection
      df_agent[ind_agent, "t_inf"] <- rep(t_i, length(ind_agent)) # Set infection time
      df_agent[ind_agent, "location"] <- rep(2, length(ind_agent))
      df_agent[ind_agent, "source"] <- rep(NA, length(ind_agent))
      # Save respective infection time and location of infection (depending on number of infection)
      for(x in 1:length(external_inf_ids)){
        df_agent[ind_agent[x], eval(t_inf[x])] <- t_i# Set infection time
        df_agent[ind_agent[x], eval(location[x])] <- 2
        df_agent[ind_agent[x], eval(source[x])] <- NA
        df_history[abs(df_history$time-t_i)<=tol & df_history$id==ind_agent[x], "location"] <- 2
        df_history[abs(df_history$time-t_i)<=tol & df_history$id==ind_agent[x], "n_inf"] <- n_inf[x]
      }
      
      ### Symptom onset
      if(scaling_factor < 1 & !scaling_constant){
        n_total_inf <- as.numeric(prev_inf) + as.numeric(n_inf>1) + vaccination_flags
      }else n_total_inf <- 1
      
      symptom_onset <- symptom.onset(df_history, df_agent, external_inf_ids, teacher=teacher, vaccination_flag=vaccination_flag, scaling_factor=scaling_factor^n_total_inf)
      df_history <- symptom_onset$df_history
      df_agent <- symptom_onset$df_agent
      
      ### Recovery
      inf_vacc_ids <- df_agent[df_agent$id%in%external_inf_ids & df_agent$vaccinated_2==1, "id"]
      inf_unvacc_ids <- df_agent[df_agent$id%in%external_inf_ids & df_agent$vaccinated_2==0, "id"]
      df_history[df_history$id%in%inf_vacc_ids & df_history$time + tol >= t_i + rec_time[1], "state"] <- "R"
      df_history[df_history$id%in%inf_unvacc_ids & df_history$time + tol >= t_i + rec_time[2], "state"] <- "R"
    }
  }
  return(list(df_history = df_history,
              df_agent = df_agent, 
              external_inf_ids = external_inf_ids))
}
