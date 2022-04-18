# ============================================================================ #
# Isolation
# ---------------------------------------------------------------------------- #
# Symptomatically infected individuals who are tested positive using a PCR test
# will be isolated. 
# 1. Determine all individuals that developed symptoms at time t_i. 
# 2. Test them with a PCR test & put them in self-isolation if test is positive
# ============================================================================ #
isolation <- function(t_i, df_history, df_agent, tol = 1e-5){
  curr_symp_ids <- unique(unlist(df_history %>% filter(state=="IS", abs(time-t_i)<=tol, iso_state=="P") %>% summarize(id)))
  if(length(curr_symp_ids)>0){
    ind_curr_symp <- which(df_agent$id%in%curr_symp_ids)
    t_inc <- df_agent[ind_curr_symp, "t_inf"] + df_agent[ind_curr_symp, "t_inc"]
    t_since_inf <- t_i-df_agent[ind_curr_symp, "t_inf"]
    probs <- predict(pcr_test_sens_fun, ifelse(t_since_inf>0, 1, t_since_inf))$y
    probs <- ifelse(probs>1, 1, probs)
    probs <- ifelse(probs<0, 0, probs)
    pos_tested <- sapply(probs, function(x) sample(c(0,1), size=1, prob = c(1-x, x))) 
    compliant_ids <- unlist(df_agent %>% filter(iso_compliance==1) %>% summarize(id))
    pos_tested_ids <- intersect(curr_symp_ids[pos_tested==1], compliant_ids)
    t_inc_pos <- t_inc[pos_tested==1]
    if(length(pos_tested_ids)>0){
      # Isolate positive tested individuals
      for(x in 1:length(pos_tested_ids)){
        t_inc_pos <- df_agent[df_agent$id==pos_tested_ids[x], "t_inf"] + df_agent[df_agent$id==pos_tested_ids[x], "t_inc"]
        ind <- which(df_history$state=="IS" & df_history$id==pos_tested_ids[x] & df_history$time+tol>=t_inc_pos & df_history$time<=t_inc_pos+iso_time+tol)
        df_history[ind,"iso_state"] <- "IH"
      }      
      df_agent[df_agent$id%in%pos_tested_ids, "iso_time"] <- rep(t_i, length(pos_tested_ids))
      df_agent[df_agent$id%in%pos_tested_ids, "iso"] <- rep(1, length(pos_tested_ids))  
    }else pos_tested_ids <- NULL
  }else pos_tested_ids <- NULL
  
  return(list(df_history = df_history, 
              df_agent = df_agent,
              curr_symp_ids = curr_symp_ids,
              pos_tested_ids = pos_tested_ids))
}

# ============================================================================ #
# Quarantine close contacts and n_quaran other contacts in other grades
# upon symptom onset of index case (without test)
# ---------------------------------------------------------------------------- #
# index case = Symptomatically infected case 
# 1. Determine close contacts of index case
# 2. Randomly choose n_quaran contacts from contact list of index case
# 3. Put them in quarantine if they are compliant
# Note: Teachers never have to quarantine
# Note: Independent of infection status
# Note: No check if students already have been quarantined or isolated before
# ============================================================================ #
quarantine.close.contacts <- function(t_i, 
                                      df_history, df_agent, 
                                      df_teach_hist, df_teacher, 
                                      pos_tested_ids, 
                                      cont_close, cont_class, cont_grade, cont_teacher,
                                      susc_close, susc_class, susc_grade, susc_out_school, 
                                      susc_teacher, susc_tt, susc_ts,
                                      tol=1e-5){
  for(i in pos_tested_ids){
    ind <- which(as.numeric(names(cont_close))==i)
    # Determine close contacts of index case
    close_cont_ids <- unlist(cont_close[[ind]])
    # Randomly choose n_quaran contacts from contact list (excluding classmates)
    # that have to immediately quarantine as well
    contacts <- unlist(cont_grade[[ind]])
    len_cont_grade <- length(contacts)
    if(len_cont_grade>0){
      sample_size <- min(len_cont_grade, n_quaran)
      quaran_ids <- c(contacts[sample(sample_size)], close_cont_ids)
    }else{
      quaran_ids <- close_cont_ids
    }

    quaran_ids <- quaran_ids[order(match(quaran_ids, unique(df_agent$id)))]
    compliant_ids <- unlist(df_agent %>% filter(iso_compliance==1) %>% summarize(id))
    unvaccinated2doses_ids <- unlist(df_agent %>% filter(vaccinated_2==0) %>% summarize(id))
    quaran_ids <- intersect(quaran_ids, compliant_ids)
    quaran_ids <- intersect(quaran_ids, unvaccinated2doses_ids)
    if(length(quaran_ids)>0){
      ind_time <- intersect(which(df_history$time+tol>=t_i), which(df_history$time<=t_i+quaran_time+tol))
      ind_Q <- intersect(ind_time, which(df_history$id%in%quaran_ids))
      df_history[ind_Q, "iso_state"] <- rep("Q", length(ind_Q))
      df_agent[df_agent$id%in%quaran_ids, "quaran"] <- rep(1, length(which(df_agent$id%in%quaran_ids)))
      df_agent[df_agent$id%in%quaran_ids, "iso_time"] <- rep(t_i, length(which(df_agent$id%in%quaran_ids)))
      # Remove quarantined individuals from eligible susceptible contacts
      susc_close <- lapply(susc_close, function(x) setdiff(unlist(x), quaran_ids))
      susc_class <- lapply(susc_class, function(x) setdiff(unlist(x), quaran_ids))
      susc_grade <- lapply(susc_grade, function(x) setdiff(unlist(x), quaran_ids))
      susc_out_school <- lapply(susc_out_school, function(x) setdiff(unlist(x), quaran_ids))
      susc_ts <- lapply(susc_ts, function(x) setdiff(unlist(x), quaran_ids))
    }
  }
  return(list(df_history = df_history, 
              df_agent = df_agent,
              df_teach_hist = df_teach_hist, 
              df_teacher = df_teacher, 
              susc_close = susc_close, 
              susc_class = susc_class, 
              susc_grade = susc_grade,
              susc_out_school = susc_out_school,
              susc_teacher = susc_teacher,
              susc_tt = susc_tt,
              susc_ts = susc_ts))
}


# ============================================================================ #
# Quarantine class mates and close contacts upon symptom onset of index case 
# (without test)
# ---------------------------------------------------------------------------- #
# index case = Symptomatically infected case 
# 1. Determine close contacts of index case
# 2. Determine class mates of index case
# 3. Put them in quarantine if they are compliant and are unvaccinated or only
# partially vaccinated
# Note: Only class mates of students have to quarantine
# Note: Teachers never have to quarantine
# Note: Independent of infection status
# Note: No check if students already have been quarantined or isolated before
# ============================================================================ #
quarantine.class <- function(t_i, 
                             df_history, df_agent, 
                             df_teach_hist, df_teacher, 
                             pos_tested_ids, 
                             cont_close, cont_class, cont_grade, cont_teacher,
                             susc_close, susc_class, susc_grade, susc_out_school, 
                             susc_teacher, susc_tt, susc_ts,
                             quarantine_fully_vacc = F,
                             tol=1e-5){
  for(i in pos_tested_ids){
    ind <- which(as.numeric(names(cont_close))==i)
    # Determine close contacts of index case
    close_cont_ids <- unlist(cont_close[[ind]])
    id_row <- which(df_agent$id==i)
    c <- df_agent[id_row, "class"]
    g <- df_agent[id_row, "grade"]
    gr <- df_agent[id_row, "group"]
    class_ids <- df_agent[df_agent$class==c & df_agent$grade==g & df_agent$group==gr, "id"]
    quaran_ids <- c(class_ids, close_cont_ids)
    quaran_ids <- quaran_ids[order(match(quaran_ids, unique(df_agent$id)))]
    # Account for compliance
    compliant_ids <- unlist(df_agent %>% filter(iso_compliance==1) %>% summarize(id))
    quaran_ids <- intersect(quaran_ids, compliant_ids)
    # Account for vaccination (fully vaccinated don't have to quarantine)
    if(!quarantine_fully_vacc){
      unvaccinated2doses_ids <- unlist(df_agent %>% filter(vaccinated_2==0) %>% summarize(id))
      quaran_ids <- intersect(quaran_ids, unvaccinated2doses_ids)
    }
    if(length(quaran_ids)>0){
      ind_time <- intersect(which(df_history$time+tol>=t_i), which(df_history$time<=t_i+quaran_time+tol))
      ind_Q <- intersect(ind_time, which(df_history$id%in%quaran_ids))
      df_history[ind_Q, "iso_state"] <- rep("Q", length(ind_Q))
      df_agent[df_agent$id%in%quaran_ids, "quaran"] <- rep(1, length(which(df_agent$id%in%quaran_ids)))
      df_agent[df_agent$id%in%quaran_ids, "iso_time"] <- rep(t_i, length(which(df_agent$id%in%quaran_ids)))
      # Remove quarantined individuals from eligible susceptible contacts
      susc_close <- lapply(susc_close, function(x) setdiff(unlist(x), quaran_ids))
      susc_class <- lapply(susc_class, function(x) setdiff(unlist(x), quaran_ids))
      susc_grade <- lapply(susc_grade, function(x) setdiff(unlist(x), quaran_ids))
      susc_out_school <- lapply(susc_out_school, function(x) setdiff(unlist(x), quaran_ids))
      susc_ts <- lapply(susc_ts, function(x) setdiff(unlist(x), quaran_ids))
    }
  }
  return(list(df_history = df_history, 
              df_agent = df_agent,
              df_teach_hist = df_teach_hist, 
              df_teacher = df_teacher, 
              susc_close = susc_close, 
              susc_class = susc_class, 
              susc_grade = susc_grade,
              susc_out_school = susc_out_school,
              susc_teacher = susc_teacher,
              susc_tt = susc_tt,
              susc_ts = susc_ts))
}

# ============================================================================ #
# Release quarantined individuals with a negative test after 5 days 
# ---------------------------------------------------------------------------- #
# 1. Identify student ids that are eligible for being released
# 2. Test eligible students with antigen test
# 3. Release students that were tested negative from quarantine
# 4. Update susceptible contacts (released students may have contacts again)
# ============================================================================ #
test.after.quarantine <- function(t_i, 
                                  df_history, df_agent, 
                                  df_teach_hist, df_teacher, 
                                  cont_close, cont_class, cont_grade, cont_teacher,
                                  susc_close, susc_class, susc_grade, susc_out_school, 
                                  susc_teacher, susc_tt, susc_ts,
                                  tol=1e-5){
  # Identify student ids that are eligible for being released
  to_be_released <- df_agent[df_agent$quaran==1 & df_agent$iso_time+5<= t_i+tol,]
  # to_be_released <- df_agent[df_agent$quaran==1 & abs(df_agent$iso_time+5-t_i)<=tol,]
  curr_noninf <- to_be_released[to_be_released$state%in%c("S","R"),]
  curr_inf <- to_be_released[to_be_released$state%in%c("PS","IS","IA"),]
  # Test those students
  tested <- testing(t_i, df_history, df_agent, curr_noninf, curr_inf)
  released_ids <- setdiff(to_be_released$id, c(tested$FP_ids, tested$pos_tested_ids))
  # Release students that were tested negative from quarantine
  ind_released <- unlist(lapply(released_ids, function(x) which(df_history$id==x & df_history$time>=t_i)))
  df_history[ind_released, "iso_state"] <- "P"
  # Update susceptible contacts (released students may have contacts again)
  susc_close <- update.susc.contacts(t_i, df_history, susc_close)
  susc_class <- update.susc.contacts(t_i, df_history, susc_class)
  susc_grade <- update.susc.contacts(t_i, df_history, susc_grade)
  susc_out_school <- update.susc.contacts(t_i, df_history, susc_out_school)
  susc_ts <- update.susc.contacts(t_i, df_history, susc_ts)
  
  return(list(df_history = df_history, 
              df_agent = df_agent,
              df_teach_hist = df_teach_hist, 
              df_teacher = df_teacher, 
              susc_close = susc_close, 
              susc_class = susc_class, 
              susc_grade = susc_grade,
              susc_out_school = susc_out_school, 
              susc_ts = susc_ts))
}
