# ============================================================================ #
# Transmission events
# ---------------------------------------------------------------------------- #
# 
# ============================================================================ #
transmission.events <- function(t_i, 
                                time_steps,
                                time_names,
                                df_history, 
                                df_agent, 
                                df_teach_hist, 
                                df_teacher, 
                                df_teacher_gcs,
                                susc_close, 
                                susc_class, 
                                susc_grade, 
                                susc_out_school, 
                                susc_teacher, 
                                susc_tt, 
                                susc_ts,
                                infecteds=NA, 
                                contacts_in_school, 
                                contacts_out_school,
                                vaccination_flag=T,
                                vacc_eff_1 = c(0.57, 0.70),
                                vacc_eff_2 = c(0.85, 0.93),
                                vacc_eff_lag = 14,
                                scaling_factor = 0.8,
                                scaling_constant = F,
                                sample_prop=1, 
                                cont_susc_scale=c(0.5,0.85),
                                aerosol_susc_scale=c(0.001,0.005),
                                aerosol = 1.0,
                                R_0=1.0,
                                tol=1e-5){
  new_inf <- new_inf_id <- NULL
  if(is.null(df_agent)) print(paste0("Transmission function: df_agent is empty."))
  if(is.null(df_teacher)) print(paste0("Transmission function: df_teacher is empty."))
  # -------------------------------------------------------------------------- #
  # Transmission from students
  # -------------------------------------------------------------------------- #
  # Determine which students are currently infected and present in school
  # df_infected contains information on day, grade, class, student
  df_infected <- df_history %>% filter(state%in%c("PS","IA"), 
                                       !iso_state%in%c("Q","IH"), 
                                       abs(time-t_i)<=tol, 
                                       pres==1)   
  inf_id <- unique(df_infected$id) # Ids of currently infected individuals
  # For each infected individual, perform transmission events to their susceptible contacts
  for(i in inf_id){
    student_infections <- infect.susc(i, 
                                      t_i, time_steps, time_names, 
                                      df_history, df_agent, 
                                      df_teach_hist, df_teacher, df_teacher_gcs,
                                      susc_close, susc_class, susc_grade, 
                                      susc_out_school, susc_teacher, 
                                      susc_tt, susc_ts,
                                      infecteds,
                                      teacher=F,
                                      contacts_in_school=contacts_in_school, 
                                      contacts_out_school=contacts_out_school,
                                      vaccination_flag=vaccination_flag,
                                      vacc_eff_1 = vacc_eff_1, 
                                      vacc_eff_2 = vacc_eff_2, 
                                      vacc_eff_lag = vacc_eff_lag,
                                      scaling_factor = scaling_factor,
                                      scaling_constant=scaling_constant,
                                      sample_prop = sample_prop,
                                      cont_susc_scale = cont_susc_scale, 
                                      aerosol_susc_scale = aerosol_susc_scale,
                                      aerosol = aerosol, 
                                      R_0=R_0)
    df_history <- student_infections$df_history
    df_agent <- student_infections$df_agent
    df_teach_hist <- student_infections$df_teach_hist
    df_teacher <- student_infections$df_teacher
    susc_close <- student_infections$susc_close
    susc_class <- student_infections$susc_class
    susc_grade <- student_infections$susc_grade
    susc_out_school <- student_infections$susc_out_school
    susc_teacher <- student_infections$susc_teacher
    susc_tt <- student_infections$susc_tt
    susc_ts <- student_infections$susc_ts
    infecteds <- student_infections$infecteds
  } 
  # -------------------------------------------------------------------------- #
  # Transmissions from teachers
  # Only done on week days
  # -------------------------------------------------------------------------- #
  if(contacts_in_school[abs(time_steps-t_i)<=tol]==1){
    # Determine which teachers are currently infected and neither isolated nor 
    # quarantined
    df_infected <- df_teach_hist %>% filter(state%in%c("PS","IA"), 
                                            !iso_state%in%c("Q","IH"), 
                                            abs(time-t_i)<=tol)   
    inf_id <- unique(df_infected$id) # Ids of currently infected teachers
    for(i in inf_id){
      teacher_infections <- infect.susc(i, 
                                        t_i, time_steps, time_names, 
                                        df_history, df_agent, 
                                        df_teach_hist, df_teacher, df_teacher_gcs, 
                                        susc_close, susc_class, susc_grade, 
                                        susc_out_school, susc_teacher, 
                                        susc_tt, susc_ts,
                                        infecteds, 
                                        teacher=T,
                                        contacts_in_school = contacts_in_school, 
                                        contacts_out_school = contacts_out_school,
                                        vaccination_flag = vaccination_flag, 
                                        vacc_eff_1 = vacc_eff_1,
                                        vacc_eff_2 = vacc_eff_2,
                                        vacc_eff_lag = vacc_eff_lag,
                                        scaling_factor = scaling_factor,
                                        scaling_constant=scaling_constant,
                                        sample_prop=sample_prop,
                                        cont_susc_scale = cont_susc_scale, 
                                        aerosol_susc_scale = aerosol_susc_scale,
                                        aerosol = aerosol, 
                                        R_0 = R_0)
      df_history <- teacher_infections$df_history
      df_agent <- teacher_infections$df_agent
      df_teach_hist <- teacher_infections$df_teach_hist
      df_teacher <- teacher_infections$df_teacher
      susc_close <- teacher_infections$susc_close
      susc_class <- teacher_infections$susc_class
      susc_grade <- teacher_infections$susc_grade
      susc_teacher <- teacher_infections$susc_teacher
      susc_tt <- teacher_infections$susc_tt
      susc_ts <- teacher_infections$susc_ts
      infecteds <- teacher_infections$infecteds
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
              susc_ts = susc_ts,
              infecteds=infecteds))
}


# ============================================================================ #
# Infection of susceptible contacts of index case
# ---------------------------------------------------------------------------- #
infect.susc <- function(i, 
                        t_i, time_steps, time_names,
                        df_history, 
                        df_agent, 
                        df_teach_hist, 
                        df_teacher, 
                        df_teacher_gcs=NULL, 
                        susc_close=NULL, 
                        susc_class=NULL, 
                        susc_grade=NULL, 
                        susc_out_school=NULL, 
                        susc_teacher=NULL, 
                        susc_tt=NULL, 
                        susc_ts=NULL,
                        infecteds=NA, 
                        teacher=F, 
                        contacts_in_school, 
                        contacts_out_school, 
                        vaccination_flag=F,
                        vacc_eff_1 = c(0.57, 0.70),
                        vacc_eff_2 = c(0.85, 0.93),
                        vacc_eff_lag = c(0,0),
                        scaling_factor=0.8,
                        scaling_constant=F,
                        sample_prop=1, 
                        cont_susc_scale=c(0.5,0.85),
                        aerosol_susc_scale=c(0.005,0.010),
                        aerosol = F, 
                        R_0=1.0,
                        tol=1e-5){
  susc_student_ids <- susc_teacher_ids <- NULL
  if(is.null(df_agent)) print("infect.susc function: df_agent is empty.")
  if(is.null(df_teacher)) print("infect.susc function: df_teacher is empty.")
  # -------------------------------------------------------------------------- #
  # Compute transmission probability and determine susceptible contacts
  # -------------------------------------------------------------------------- #
  # Flag for primary or secondary infection
  inf_flag <- NA # 1 = primary infection during study period, 2 = secondary infection during study period
  if(teacher){ ### Teacher is index case
    # Identify day since infection of that specific individual at time t_i
    ind <- which(df_teacher$id==i)
    n_inf <- df_teacher[ind, "n_inf"]
    t_inf <- paste0("t_inf_", n_inf)
    inf_flag <- n_inf

    source_var <- paste0("source_", inf_flag)
    infector_var <- paste0("infector_", inf_flag)
    location_var <- paste0("location_", inf_flag)
    week_day_var <- paste0("week_day_", inf_flag)

    source <- df_teacher[df_teacher$id==i, "source"] # Source of infector i
    t_since_inf <- t_i - df_teacher[df_teacher$id==i, eval(t_inf)]
    if(is.na(t_since_inf)){
      print("t_since_inf is NA!")
      print(paste0("t_i=", t_i))
      print(df_teacher[df_teacher$id==i,])
    }
    # Flag whether infected student is asymptomatically infected
    asymp <- ifelse(df_teacher[df_teacher$id==i, "state"]=="IA", 1, 0)
    # ---------------------------------------------------------------------- # 
    # Translate into infectiousness on day d and transmission prob per contact
    red_inf <- (df_teacher[ind, "vaccinated_2"]==1 | n_inf > 1)
    transmprob <- prob.trans(time = ifelse(t_since_inf<0, 0, t_since_inf), 
                             infec = F, susc = F, asymp = as.logical(asymp), 
                             vacc = ifelse(red_inf==1, T, F), 
                             R_0 = R_0)
    if(is.na(transmprob)| transmprob<0){
      print("transprob is NA or <0!")
      print("teacher?", teacher)
      print(paste0("t_i=", t_i))
      print(paste0("R_0=", R_0))
    }
    # Susceptible students in contact with infected teacher
    susc_student_ids <- susc_ts[[i]]
    # Susceptible students in class: to be infected through aerosol transmission
    id_row <- which(df_teacher_gcs$id==i)
    c <- df_teacher_gcs[id_row, "class"]
    g <- df_teacher_gcs[id_row, "grade"]
    gr <- df_teacher_gcs[id_row, "group"]
    susc_student_ids_aerosol <- df_agent[df_agent$class%in%c & df_agent$grade%in%g & df_agent$group%in%gr & df_agent$state=="S", "id"]
    # Susceptible teachers in contact with infected teacher
    ind <- which(names(susc_tt)==i)
    susc_teacher_ids <- unlist(susc_tt[[ind]])
    susc_teacher_ids_aerosol <- NULL # Teachers do not transmit to teachers via aerosol
  }else{ ### Student is index case
    # Identify day since infection of that specific individual at time t_i
    ind <- which(df_agent$id==i)
    n_inf <- df_agent[ind, "n_inf"]
    t_inf <- paste0("t_inf_", n_inf)
    inf_flag <- n_inf

    source_var <- paste0("source_", inf_flag)
    infector_var <- paste0("infector_", inf_flag)
    location_var <- paste0("location_", inf_flag)
    week_day_var <- paste0("week_day_", inf_flag)
    # Time since infection
    t_since_inf <- t_i - df_agent[df_agent$id==i, eval(t_inf)]
    if(is.na(t_since_inf)){
      print("t_since_inf is NA!")
      print(paste0("t_i=", t_i))
      print(df_agent[df_agent$id==i,])
    }
    # Source of infector i
    source <- df_agent[df_agent$id==i, "source"]
    # Flag whether infected student is asymptomatically infected
    asymp <- ifelse(df_agent[df_agent$id==i, "state"]=="IA", 1, 0)
    # ------------------------------------------------------------------------ # 
    # Translate into infectiousness on day d and transmission prob per contact
    red_inf <- (df_agent[ind, "vaccinated_2"]==1 | n_inf > 1)
    transmprob <- prob.trans(time = ifelse(t_since_inf<0, 0, t_since_inf), 
                             infec = T, susc = F, asymp = as.logical(asymp), 
                             vacc = ifelse(red_inf==1, T, F), 
                             R_0 = R_0)
    if(is.na(transmprob)| transmprob<0){
      print("transprob is NA or <0!")
      print("teacher?", teacher)
      print(paste0("t_i=", t_i))
      print(paste0("R_0=", R_0))
    }
    # Determine susceptible contacts of infected student
    # Get index of infected id in contact lists
    ind <- which(as.numeric(names(susc_close))==i)
    # Susceptible contacts are dependent on whether student is currently
    # in school or outside school
    if(contacts_out_school[abs(time_steps-t_i)<=tol]==1){
      susc_student_ids <- intersect(unlist(susc_out_school[[ind]]), unique(df_history[df_history$state=="S" & abs(df_history$time-t_i)<=tol,"id"]))
      susc_teacher_ids <- susc_student_ids_aerosol <- susc_teacher_ids_aerosol <- NULL
    }   
    if(contacts_in_school[abs(time_steps-t_i)<=tol]==1){
      susc_student_ids <- c(unlist(susc_close[[ind]]), unlist(susc_class[[ind]]), unlist(susc_grade[[ind]]))
      susc_student_ids <- intersect(susc_student_ids, unique(df_history[df_history$pres==1 & abs(df_history$time-t_i)<=tol,"id"]))
      susc_teacher_ids <- unlist(susc_teacher[[ind]])
      # Susceptible students in class: to be infected through aerosol transmission
      id_row <- which(df_agent$id==i)
      c <- df_agent[id_row, "class"]
      g <- df_agent[id_row, "grade"]
      gr <- df_agent[id_row, "group"]
      susc_student_ids_aerosol <- df_agent[df_agent$class%in%c & df_agent$grade%in%g & df_agent$group%in%gr & df_agent$state=="S", "id"]
      susc_teacher_ids_aerosol <- df_teacher_gcs[df_teacher_gcs$class%in%c & df_teacher_gcs$grade%in%g & df_teacher_gcs$group%in%gr, "id"]
      susc_teacher_ids_aerosol <- intersect(susc_teacher_ids_aerosol, df_teacher[df_teacher$state=="S", "id"])
    }
  }
  # -------------------------------------------------------------------------- # 
  # Infect susceptible contacts
  # -------------------------------------------------------------------------- #
  susc_cont_ids <- c(susc_student_ids, susc_teacher_ids)
  ### Infect susceptible students
  if(length(susc_student_ids)>0){
    susc_student_ids <- susc_student_ids[order(match(susc_student_ids, unique(df_agent$id)))]
    # Reduced susceptibility
    susc_vec <- df_agent[df_agent$id%in%susc_student_ids, "susc"]
    # susc_vec <- susc.scale(length(susc_student_ids))
    # # Reduce susceptibility due to vaccination
    # # Vaccination is explicitly modeled with 2 doses
    # if(vaccination_flag) vacc_vec_s <- reduced.susc.vacc(t_i, df_agent, susc_student_ids, vacc_eff_1, vacc_eff_2, vacc_eff_lag, tol=tol)
    # else vacc_vec_s <- rep(1, length(susc_vec))
    # susc_vec <- vacc_vec_s*susc_vec
    # If teacher was index case, then reduce infectiousness
    if(teacher) transmprob <- contact.infec.scale(min=cont_susc_scale[1], max=cont_susc_scale[2])*transmprob
    new_inf <- sapply(susc_vec, function(x) sample(c(0,1), size=1, prob=c(max(0, 1-x*transmprob), min(1, x*transmprob))))
    inf_student_ids <- susc_student_ids[which(new_inf==1)]
    infecteds <- c(infecteds, inf_student_ids)
    if(length(inf_student_ids)>0){ # Students
      infected_students <- infect.students(i, 
                                           t_i, time_steps, time_names,
                                           source, 
                                           inf_student_ids,
                                           df_history,
                                           df_agent,
                                           susc_close, 
                                           susc_class, 
                                           susc_grade, 
                                           susc_out_school, 
                                           susc_teacher,
                                           susc_ts,
                                           infecteds,
                                           contacts_in_school=contacts_in_school,
                                           vaccination_flag = vaccination_flag,
                                           scaling_factor = scaling_factor,
                                           scaling_constant=scaling_constant,
                                           R_0=R_0)
      df_history <- infected_students$df_history
      df_agent <- infected_students$df_agent
      susc_close <- infected_students$susc_close
      susc_class <- infected_students$susc_class
      susc_grade <- infected_students$susc_grade
      susc_teacher <- infected_students$susc_teacher
      susc_ts <- infected_students$susc_ts
      infecteds <- infected_students$infecteds
    }
  }
  
  ### Infect susceptible teachers
  susc_teacher_ids <- intersect(susc_teacher_ids, df_teacher[df_teacher$state=="S", "id"])
  if(length(susc_teacher_ids)>0){
    susc_teacher_ids <- susc_teacher_ids[order(match(susc_teacher_ids, unique(df_teacher$id)))]
    # If teachers are vaccinated (vaccination_flag=1), then reduce susceptibility
    # of teachers to be infected
    vacc_vec <- df_teacher[df_teacher$id%in%susc_teacher_ids, "susc"]
    # if(vaccination_flag) vacc_vec <- reduced.susc.vacc(t_i, df_teacher, susc_teacher_ids, vacc_eff_1, vacc_eff_2, vacc_eff_lag, tol=tol)
    # else vacc_vec <- rep(1, length(susc_teacher_ids))
    # Reduce infectiousness of infected individual
    # Depends on whether student-teacher or teacher-teacher contact
    if(!teacher) transmprob <- contact.infec.scale(n=1, min=cont_susc_scale[1], max=cont_susc_scale[2])*transmprob
    if(is.na(transmprob)| transmprob<0){
      print("transprob is NA or <0!")
      print("teacher?", teacher)
      print(paste0("t_i=", t_i))
      print(paste0("R_0=", R_0))
    }
    new_inf <- sapply(vacc_vec, function(x) sample(c(0,1), size=1, prob=c(max(0, 1-x*transmprob), min(1, x*transmprob))))
    inf_teacher_ids <- susc_teacher_ids[which(new_inf==1)]
    infecteds <- c(infecteds, inf_teacher_ids)
    if(length(inf_teacher_ids)>0){ # Teachers
      infected_teachers <- infect.teachers(i, 
                                           t_i, time_steps, time_names,
                                           source, 
                                           inf_teacher_ids,
                                           df_teach_hist,
                                           df_teacher,
                                           susc_teacher,
                                           susc_tt, 
                                           infecteds,
                                           scaling_factor=scaling_factor, 
                                           scaling_constant=scaling_constant,
                                           R_0=R_0)
      df_teach_hist <- infected_teachers$df_teach_hist
      df_teacher <- infected_teachers$df_teacher
      susc_teacher <- infected_teachers$susc_teacher
      susc_tt <- infected_teachers$susc_tt
      infecteds <- infected_teachers$infecteds
    }
  }
  
  ### Infect susceptible students through aerosol
  # Update susceptible students
  susc_student_ids_aerosol <- intersect(susc_student_ids_aerosol, df_agent[df_agent$state=="S", "id"])
  super_spreading <- sample(c(0,1), size=1, prob = c(1-aerosol, aerosol))
  if(length(susc_student_ids_aerosol)>0 & super_spreading==1){
    susc_student_ids_aerosol <- susc_student_ids_aerosol[order(match(susc_student_ids_aerosol, unique(df_agent$id)))]
    susc_vec <- df_agent[df_agent$id%in%susc_student_ids_aerosol, "susc"]
    # susc_vec <- susc.scale(length(susc_student_ids_aerosol))
    # # Reduce susceptibility due to vaccination of students to be infected
    # # Reduce susceptibility due to vaccination
    # # Vaccination is explicitly modeled with 2 doses
    # if(vaccination_flag) vacc_vec_s <- reduced.susc.vacc(t_i, df_agent, susc_student_ids_aerosol, vacc_eff_1, vacc_eff_2, vacc_eff_lag, tol=tol)
    # else vacc_vec_s <- rep(1, length(susc_vec))
    # susc_vec <- vacc_vec_s*susc_vec
    
    # Scale transmission probability for aerosol transmission
    transmprob <- contact.infec.scale(n=1, min=aerosol_susc_scale[1], max=aerosol_susc_scale[2])*transmprob
    transmprob <- 1-exp(-transmprob)
    if(is.na(transmprob)){
      print("transprob is NA!")
      print("teacher?", teacher)
      print(paste0("t_i=", t_i))
      print(paste0("R_0=", R_0))
    }
    new_inf <- sapply(susc_vec, function(x) sample(c(0,1), size=1, prob=c(max(0, 1-x*transmprob), min(1, x*transmprob))))
    inf_student_ids <- susc_student_ids_aerosol[which(new_inf==1)]
    infecteds <- c(infecteds, inf_student_ids)
    if(length(inf_student_ids)>0){ # Students
      infected_students <- infect.students(i, 
                                           t_i, time_steps, time_names,
                                           source, 
                                           inf_student_ids,
                                           df_history,
                                           df_agent,
                                           susc_close, 
                                           susc_class, 
                                           susc_grade, 
                                           susc_out_school, 
                                           susc_teacher,
                                           susc_ts,
                                           infecteds,
                                           contacts_in_school, 
                                           vaccination_flag=vaccination_flag,
                                           scaling_factor=scaling_factor,
                                           scaling_constant=scaling_constant,
                                           R_0=R_0)
      df_history <- infected_students$df_history
      df_agent <- infected_students$df_agent
      susc_close <- infected_students$susc_close
      susc_class <- infected_students$susc_class
      susc_grade <- infected_students$susc_grade
      susc_teacher <- infected_students$susc_teacher
      susc_ts <- infected_students$susc_ts
      infecteds <- infected_students$infecteds
    }
  }
  
  ### Infect susceptible teachers via aerosol (only if index case is student)
  susc_teacher_ids_aerosol <- intersect(susc_teacher_ids_aerosol, df_teacher[df_teacher$state=="S", "id"])
  super_spreading <- sample(c(0,1), size=1, prob = c(1-aerosol, aerosol))
  if(length(susc_teacher_ids_aerosol)>0 & super_spreading==1){
    # Update susc_teacher_ids_aerosol as some teachers might be infected before
    susc_teacher_ids_aerosol <- susc_teacher_ids_aerosol[order(match(susc_teacher_ids_aerosol, unique(df_teacher$id)))]
    # Reduce susceptibility due to vaccination of teachers to be infected
    vacc_vec <- df_teacher[df_teacher$id%in%susc_teacher_ids_aerosol, "susc"]
    # if(vaccination_flag) vacc_vec <- reduced.susc.vacc(t_i, df_teacher, susc_teacher_ids_aerosol, vacc_eff_1, vacc_eff_2, vacc_eff_lag, tol=tol)
    # else vacc_vec <- rep(1, length(susc_teacher_ids_aerosol))
    # Scale transmission probability for aerosol transmission
    transmprob <- contact.infec.scale(n=1, min=aerosol_susc_scale[1], max=aerosol_susc_scale[2])*transmprob
    transmprob <- 1-exp(-transmprob)
    if(is.na(transmprob)){
      print("transprob is NA!")
      print("teacher?", teacher)
      print(paste0("t_i=", t_i))
      print(paste0("R_0=", R_0))
    }
    new_inf <- sapply(vacc_vec, function(x) sample(c(0,1), size=1, prob=c(max(0, 1-x*transmprob), min(1, x*transmprob))))
    inf_teacher_ids <- susc_teacher_ids_aerosol[which(new_inf==1)]
    infecteds <- c(infecteds, inf_teacher_ids)
    if(length(inf_teacher_ids)>0){ # Teachers
      infected_teachers <- infect.teachers(i, 
                                           t_i, time_steps, time_names,
                                           source, 
                                           inf_teacher_ids,
                                           df_teach_hist,
                                           df_teacher,
                                           susc_teacher,
                                           susc_tt, 
                                           infecteds, 
                                           scaling_factor=scaling_factor,
                                           scaling_constant=scaling_constant,
                                           R_0=R_0)
      df_teach_hist <- infected_teachers$df_teach_hist
      df_teacher <- infected_teachers$df_teacher
      susc_teacher <- infected_teachers$susc_teacher
      susc_tt <- infected_teachers$susc_tt
      infecteds <- infected_teachers$infecteds
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
              susc_ts = susc_ts,
              infecteds = infecteds))
} 

infect.students <- function(i, 
                            t_i, time_steps, time_names,
                            source, 
                            inf_student_ids,
                            df_history,
                            df_agent,
                            susc_close=NULL, 
                            susc_class=NULL, 
                            susc_grade=NULL, 
                            susc_out_school=NULL, 
                            susc_teacher=NULL,
                            susc_ts=NULL,
                            infecteds,
                            contacts_in_school,
                            vaccination_flag=T, 
                            scaling_factor=0.8,
                            scaling_constant=F,
                            rec_time=c(14,10),
                            R_0=1.0,
                            tol=1e-5){
  if(is.null(df_agent)) print("infect.students function: df_agent is empty.")
  ind_agent <- which(df_agent$id%in%inf_student_ids)
  ind_time <- which(abs(time_steps-t_i)<=tol)
  # Update the number of infections
  df_agent[ind_agent, "n_inf"] <- df_agent[ind_agent, "n_inf"] + 1
  prev_inf <- df_agent[ind_agent, "prev_inf"]==T
  vaccination_flags <- df_agent[ind_agent, "vaccinated_2"]
  n_inf <- df_agent[ind_agent, "n_inf"]
  t_inf <- paste0("t_inf_", n_inf)
  infector <- paste0("infector_", n_inf)
  source_var <- paste0("source_", n_inf)
  location <- paste0("location_", n_inf)
  week_day <- paste0("week_day_", n_inf)
  
  df_agent[ind_agent, "t_inf"] <- rep(t_i, length(ind_agent)) # Set infection time
  df_agent[ind_agent, "infector"] <- rep(i, length(ind_agent)) # Set infector
  df_agent[ind_agent, "source"] <- rep(ifelse(is.na(source), i, source), length(ind_agent)) # Set source of outbreak
  df_agent[ind_agent, "location"] <- rep(contacts_in_school[ind_time], length(ind_agent)) # 1=in school, 0=outside school
  df_agent[ind_agent, "week_day"] <- rep(time_names[ind_time], length(ind_agent))
  df_history[df_history$id%in%inf_student_ids & abs(time_steps-t_i)<=tol, "R_0"] <- R_0
  
  for(k in 1:length(ind_agent)){
    df_agent[ind_agent[k], eval(t_inf[k])] <- t_i
    df_agent[ind_agent[k], eval(infector[k])] <- i
    df_agent[ind_agent[k], eval(source_var[k])] <- ifelse(is.na(source), i, source)
    df_agent[ind_agent[k], eval(location[k])] <- contacts_in_school[ind_time]
    df_agent[ind_agent[k], eval(week_day[k])] <- time_names[ind_time]
    rec_ind <- ifelse(vaccination_flags[k], 1, 2)
    df_history[df_history$time + tol >= t_i & df_history$time + tol <= t_i + rec_time[rec_ind] & df_history$id==ind_agent[k], "location"] <- contacts_in_school[ind_time]
    df_history[df_history$time + tol >= t_i & df_history$time + tol <= t_i + rec_time[rec_ind] & df_history$id==ind_agent[k], "n_inf"] <- n_inf[k]
  }
  
  if(scaling_factor < 1 & !scaling_constant){
    n_total_inf <- as.numeric(prev_inf) + as.numeric(n_inf>1) + vaccination_flags
  }else n_total_inf <- 1
  
  student_symp_onset <- symptom.onset(df_history, df_agent, inf_student_ids, teacher=F, vaccination_flag=vaccination_flag, scaling_factor=scaling_factor^n_total_inf)
  df_history <- student_symp_onset$df_history
  df_agent <- student_symp_onset$df_agent
  
  ### Recovery
  inf_vacc_ids <- df_agent[df_agent$id%in%inf_student_ids & df_agent$vaccinated_2==1, "id"]
  inf_unvacc_ids <- df_agent[df_agent$id%in%inf_student_ids & df_agent$vaccinated_2==0, "id"]
  df_history[df_history$id%in%inf_vacc_ids & df_history$time+tol >= t_i + rec_time[1], "state"] <- "R"
  df_history[df_history$id%in%inf_unvacc_ids & df_history$time+tol >= t_i + rec_time[2], "state"] <- "R"
  ### ACHTUNG NEW
  df_history[df_history$id%in%inf_vacc_ids & df_history$time+tol >= t_i, "susc"] <- 0
  df_history[df_history$id%in%inf_unvacc_ids & df_history$time+tol >= t_i, "susc"] <- 0
  
  # Remove newly infected from susceptible contact list
  susc_close <- lapply(susc_close, function(x) setdiff(unlist(x), inf_student_ids))
  susc_class <- lapply(susc_class, function(x) setdiff(unlist(x), inf_student_ids))
  susc_grade <- lapply(susc_grade, function(x) setdiff(unlist(x), inf_student_ids))
  susc_out_school <- lapply(susc_out_school, function(x) setdiff(unlist(x), inf_student_ids))
  susc_ts <- lapply(susc_ts, function(x) setdiff(unlist(x), inf_student_ids))
  
  infecteds <- c(infecteds, inf_student_ids)
  
  return(list(df_history = df_history,
              df_agent = df_agent,
              susc_close = susc_close,
              susc_class = susc_class,
              susc_grade = susc_grade,
              susc_out_school = susc_out_school, 
              susc_teacher = susc_teacher,
              susc_ts=susc_ts,
              infecteds=infecteds))
}

infect.teachers <- function(i, 
                            t_i, time_steps, time_names,
                            source, 
                            inf_teacher_ids,
                            df_teach_hist,
                            df_teacher,
                            susc_teacher,
                            susc_tt, 
                            infecteds,
                            scaling_factor=0.8,
                            scaling_constant=F,
                            rec_time = c(14, 10),
                            R_0 = 1.0, 
                            tol=1e-5){
  if(is.null(df_teacher)) print("infect.teachers function: df_teacher is empty.")
  ind_teacher <- which(df_teacher$id%in%inf_teacher_ids)
  ind_time <- which(abs(time_steps-t_i)<=tol)
  # Update the number of infections
  df_teacher[ind_teacher, "n_inf"] <- df_teacher[ind_teacher, "n_inf"] + 1
  n_inf <- df_teacher[ind_teacher, "n_inf"]
  prev_inf <- df_teacher[ind_teacher, "prev_inf"]==T
  vaccination_flags <- df_teacher[ind_teacher, "vaccinated_2"]
  t_inf <- paste0("t_inf_", n_inf)
  infector <- paste0("infector_", n_inf)
  source_var <- paste0("source_", n_inf)
  location <- paste0("location_", n_inf)
  week_day <- paste0("week_day_", n_inf)
  
  df_teacher[ind_teacher, "t_inf"] <- rep(t_i, length(ind_teacher)) # Set infection time
  df_teacher[ind_teacher, "infector"] <- rep(i, length(ind_teacher)) # Set infector
  df_teacher[ind_teacher, "source"] <- rep(ifelse(is.na(source), i, source), length(ind_teacher)) # Set source of outbreak
  df_teacher[ind_teacher, "location"] <- rep(1, length(ind_teacher)) # 1=in school, 0=outside school
  df_teacher[ind_teacher, "week_day"] <- rep(time_names[ind_time], length(ind_teacher))
  df_teach_hist[df_teach_hist$id%in%inf_teacher_ids & abs(time_steps-t_i)<=tol, "R_0"] <- R_0
  
  for(k in 1:length(ind_teacher)){
    df_teacher[ind_teacher[k], eval(t_inf[k])] <- t_i
    df_teacher[ind_teacher[k], eval(infector[k])] <- i
    df_teacher[ind_teacher[k], eval(source_var[k])] <- ifelse(is.na(source), i, source)
    df_teacher[ind_teacher[k], eval(location[k])] <- 1
    df_teacher[ind_teacher[k], eval(week_day[k])] <- time_names[ind_time]
    rec_ind <- ifelse(vaccination_flags[k], 1, 2)
    df_teach_hist[df_teach_hist$time + tol >= t_i & df_teach_hist$time + tol <= t_i + rec_time[rec_ind] & df_teach_hist$id==ind_teacher[k], "location"] <- 1
    df_teach_hist[df_teach_hist$time + tol >= t_i & df_teach_hist$time + tol <= t_i + rec_time[rec_ind] & df_teach_hist$id==ind_teacher[k], "n_inf"] <- n_inf[k]
  }
  ### Update susceptible contact lists
  # Remove newly infected from susceptible contact list
  susc_teacher <- lapply(susc_teacher, function(x) setdiff(unlist(x), inf_teacher_ids))
  susc_tt <- lapply(susc_tt, function(x) setdiff(unlist(x), inf_teacher_ids))
  
  ### Symptom onset of newly infected teachers
  # Depends on vaccination status of teachers
  if(scaling_factor < 1 & !scaling_constant){
    n_total_inf <- as.numeric(prev_inf) + as.numeric(n_inf>1) + vaccination_flags
  }else n_total_inf <- 1
  
  teacher_symp_onset <- symptom.onset(df_teach_hist, df_teacher, inf_teacher_ids, teacher=T, vaccination_flag=T, scaling_factor=scaling_factor^n_total_inf)
  df_teach_hist <- teacher_symp_onset$df_history
  df_teacher <- teacher_symp_onset$df_agent
  
  vacc_teacher_ids <- df_teacher[df_teacher$id%in%inf_teacher_ids & df_teacher$vaccinated_2==1,"id"]
  unvacc_teacher_ids <- setdiff(inf_teacher_ids, vacc_teacher_ids)
  
  ### Recovery (depends on vaccination status)
  df_teach_hist[df_teach_hist$id%in%vacc_teacher_ids & df_teach_hist$time+tol >= t_i + rec_time[1], "state"] <- "R"
  df_teach_hist[df_teach_hist$id%in%unvacc_teacher_ids & df_teach_hist$time+tol >= t_i + rec_time[2], "state"] <- "R"
  df_teach_hist[df_teach_hist$id%in%vacc_teacher_ids & df_teach_hist$time+tol >= t_i, "susc"] <- 0
  df_teach_hist[df_teach_hist$id%in%unvacc_teacher_ids & df_teach_hist$time+tol >= t_i, "susc"] <- 0
  
  infecteds <- c(infecteds, inf_teacher_ids)
  
  return(list(df_teach_hist = df_teach_hist,
              df_teacher = df_teacher,
              susc_teacher=susc_teacher,
              susc_tt = susc_tt,
              infecteds = infecteds))
}