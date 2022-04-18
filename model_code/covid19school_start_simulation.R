# ============================================================================ #
# Starting the simulations
# ============================================================================ #

### Printing variable values
total_start_time <- Sys.time()
print(paste0("Simulation file: ", simulation_file))
print(paste0("Starting the simulations with ", iter, "iterations."))
print(paste0("Simulation scenario: ", scenarios))
print(paste0("Isolation flag = ", isolation_flags))
print(paste0("Quarantine close contacts flag = ", quarantine_flags))
print(paste0("Quarantine class flag = ", quarantine_class_flags))
print(paste0("Quarantine vaccinated flag = ", quarantine_fully_vacc_flags))
print(paste0("Screening flag = ", scr_flags))

for(scenario in 1){
  # Matrix output template
  mat <- matrix(0, nrow=time_period, ncol=iter)
  ns <- nt <- rep(list(mat), 24)
  names(ns) <- names(nt) <- c(types, "S_waned", "S_vacc1", "S_vacc0", "Waned", "PS_vacc1", "PS_vacc0", "SYMP_vacc1", "SYMP_vacc0", "AS_vacc1", "AS_vacc0", "vaccEff0", "RecVacc1","Vacc1W1","Vacc0W1","VaccM1W1","PopImm")
  
  # Reinfections
  reinfected_students_list[[scenario]] = vector(mode="list", length=iter)
  reinfected_students_rec_list[[scenario]] = vector(mode="list", length=iter)
  reinfected_students_vacc_list[[scenario]] = vector(mode="list", length=iter)
  df_reinfected_students_list[[scenario]]  = vector(mode="list", length=iter)
  
  reinfected_teachers_list[[scenario]] = vector(mode="list", length=iter)
  reinfected_teachers_rec_list[[scenario]] = vector(mode="list", length=iter)
  reinfected_teachers_vacc_list[[scenario]] = vector(mode="list", length=iter)
  df_reinfected_teachers_list[[scenario]]  = vector(mode="list", length=iter)
  
  
  # Start simulation 
  start_time <- Sys.time()
  for(it in 1:iter){
    seed_it=10234
    print(paste0("Seed = ", it*seed_it))
    epidemic <- school.epidemic(seed=it*seed_it, 
                                occup = occup[scenario],
                                steps = steps,
                                time_period = time_period, 
                                test_days = testing_mat[scenario,], 
                                screening_adherence = screening_adherence[scenario],
                                risk_testing_adherence = risk_testing_adherence,
                                compliance_iso = compliance_iso, 
                                compliance_quaran = compliance_quaran,
                                isolation_flag = isolation_flags[scenario],
                                quarantine_flag = quarantine_flags[scenario],
                                quarantine_class_flag = quarantine_class_flags[scenario],
                                quarantine_fully_vacc = quarantine_fully_vacc_flags[scenario],
                                day_names = day_names,
                                spec = spec, 
                                screening_flag = scr_flags[scenario], 
                                risk_based_flag = risk_flags[scenario],
                                vaccination_flag = vacc_flags[scenario],
                                external_prob = external_prob, 
                                intro_fix_per_week = intro_fix_per_week,
                                cont_close = cont_close, 
                                cont_class = cont_class, 
                                cont_grade = cont_grade, 
                                cont_out_school = cont_out_school,
                                cont_teacher = cont_teacher,
                                cont_tt = cont_tt,
                                cont_ts = cont_ts,
                                susc_close = susc_close, 
                                susc_class = susc_class, 
                                susc_grade = susc_grade, 
                                susc_out_school = susc_out_school, 
                                susc_teacher = susc_teacher, 
                                susc_tt = susc_tt, 
                                susc_ts = susc_ts,
                                sample_prop=sample_prop[scenario], 
                                prop_vaccinated = prop_vaccinated,
                                prop_vacc_1_dose = prop_vaccinated_1, 
                                prop_vacc_2_dose = prop_vaccinated_2,
                                vacc_eff_1 = vacc_eff_1,
                                vacc_eff_2 = vacc_eff_2,
                                vacc_eff_teacher_1 = vacc_eff_teacher_1, 
                                vacc_eff_teacher_2 = vacc_eff_teacher_2, 
                                vacc_rate_weekly_1 = vacc_rate_weekly_1,
                                vacc_rate_weekly_2 = vacc_rate_weekly_2,
                                vacc_weekly_flag = vacc_weekly_flags[scenario],
                                t_2nd_dose = t_2nd_dose,
                                vacc_eff_lag = vacc_eff_lag,
                                prop_student_boost = prop_student_boost, 
                                prop_teacher_boost = prop_teacher_boost,
                                VE_booster_increase = vacc_eff_boost_increase, 
                                scaling_factor = scaling_prop_symp,
                                scaling_constant = scaling_constant_flag,
                                vacc_rate_2 = vacc_rate_2, 
                                cont_susc_scale = cont_susc_scale,
                                aerosol_susc_scale = aerosol_susc_scale,
                                aerosol = aerosol_probs[scenario],
                                no_intervention = no_int_vec[scenario],
                                vacc_scr_corr = vc_corr[scenario],
                                df_time_inf_students = df_time_inf_students, 
                                df_time_inf_teachers = df_time_inf_teachers, 
                                df_prob_reinfection_students = df_prob_reinfection_students,
                                df_prob_reinfection_teachers = df_prob_reinfection_teachers,
                                df_prob_reinfection_students_2 = df_prob_reinfection_students_2,
                                df_prob_reinfection_teachers_2 = df_prob_reinfection_teachers_2,
                                df_prob_vacc_students = df_prob_vacc_students,
                                df_prob_vacc_teachers = df_prob_vacc_teachers,
                                lag_reinfection = lag_reinfection_vec[scenario],
                                rel_susc_waning = rel_susc_waning_vec[scenario],
                                vacation = vacation,
                                R0_vec = R0_vec,
                                rec_time = rec_time,
                                tol=tol)
    df_history <- epidemic$df_history
    df_agent <- epidemic$df_agent
    df_teach_hist <- epidemic$df_teach_hist
    df_teacher <- epidemic$df_teacher
    df_teacher_gcs <- epidemic$df_teacher_gcs
    df_screening <- epidemic$df_screening
    screening_list <- epidemic$screening_list
    df_risk_testing <- epidemic$df_risk_testing
    infecteds <- epidemic$infecteds
    susc_close <- epidemic$susc_close
    susc_class <- epidemic$susc_class
    susc_grade <- epidemic$susc_grade
    susc_out_school <- epidemic$susc_out_school
    susc_teacher <- epidemic$susc_teacher
    susc_tt <- epidemic$susc_tt
    susc_ts <- epidemic$susc_ts
    cont_close <- epidemic$cont_close
    cont_class <- epidemic$cont_class
    cont_grade <- epidemic$cont_grade
    cont_out_school <- epidemic$cont_out_school
    cont_teacher <- epidemic$cont_teacher
    cont_tt <- epidemic$cont_tt
    cont_ts <- epidemic$cont_ts
    vacc_cov1_vec <- epidemic$vacc_cov1_vec
    vacc_cov2_vec <- epidemic$vacc_cov2_vec
    screening_stat <- epidemic$screening_stat
    t_inf_student_list <- epidemic$t_inf_student_list
    t_inc_student_list <- epidemic$t_inc_student_list
    t_inf_teacher_list <- epidemic$t_inf_teacher_list
    t_inc_teacher_list <- epidemic$t_inc_teacher_list
    intro_per_day <- epidemic$intro_per_day
    eff_vacc_cov <- epidemic$eff_vacc_cov
    intros_per_day <- epidemic$intro_per_day
    n_same_class_students <- epidemic$n_same_class_students
    n_no_index_students <- epidemic$n_no_index_students
    n_same_class_teachers <- epidemic$n_same_class_teachers
    n_no_index_teachers <- epidemic$n_no_index_teachers
    max_n_inf_students <- epidemic$max_n_inf_students
    max_n_inf_teachers <- epidemic$max_n_inf_teachers
    max_n_inc_students <- epidemic$max_n_inc_students
    max_n_inc_teachers <- epidemic$max_n_inc_teachers
    
    ### Reinfections
    reinfected_students_list[[scenario]][[it]] <- epidemic$reinfected_students
    reinfected_students_rec_list[[scenario]][[it]] <- epidemic$reinfected_students_rec
    reinfected_students_vacc_list[[scenario]][[it]] <- epidemic$reinfected_students_vacc
    reinfected_teachers_list[[scenario]][[it]] <- epidemic$reinfected_teachers
    reinfected_teachers_rec_list[[scenario]][[it]] <- epidemic$reinfected_teachers_rec
    reinfected_teachers_vacc_list[[scenario]][[it]] <- epidemic$reinfected_teachers_vacc
    df_reinfected_students_list[[scenario]][[it]] <- epidemic$df_reinfected_students
    df_reinfected_teachers_list[[scenario]][[it]] <- epidemic$df_reinfected_teachers

    
    ### Proportion of vaccinated, infected, or recovered students
    vacc_ids <- df_agent[df_agent$vaccinated_2==1, "id"]
    recovered_ids <- df_agent[df_agent$recovered==1, "id"]
    infected_ids <- df_agent[df_agent$state%in%c("IA", "IS"), "id"]
    prop_immune_students[[scenario]][it] <- length(unique(c(vacc_ids, recovered_ids, infected_ids)))/nrow(df_agent)
    ### Proportion of vaccinated, infected, or recovered teachers
    vacc_ids_t <- df_teacher[df_teacher$vaccinated_2==1, "id"]
    recovered_ids_t <- df_teacher[df_teacher$recovered==1, "id"]
    infected_ids_t <- df_teacher[df_teacher$state%in%c("IA", "IS"), "id"]
    prop_immune_teachers[[scenario]][it] <- length(unique(c(vacc_ids_t, recovered_ids_t, infected_ids_t)))/nrow(df_teacher)
    
    
    ### Weekly number of infected individuals
    max_time <- max(df_history$time)
    n_weeks <- max_time/7
    t_start <- seq(1, max_time, by=7)
    t_end <- seq(t_start[2]-1, max_time, by=7)
    
    ### Infections within the same class (per index case that caused infections)
    n_same_class_students_list[[scenario]][[it]] <- n_same_class_students
    n_no_index_students_list[[scenario]][[it]] <- n_no_index_students
    n_same_class_teachers_list[[scenario]][[it]] <- n_same_class_teachers
    n_no_index_teachers_list[[scenario]][[it]] <- n_no_index_teachers
    
    os_weekly[[scenario]][[it]] <- vector(mode="list", length=max(max_n_inf_students, max_n_inf_teachers))
    os_student_weekly[[scenario]][[it]] <- vector(mode="list", length=max_n_inf_students)
    os_teacher_weekly <- vector(mode="list", length=max_n_inf_teachers)
    
    intro_weekly[[scenario]][[it]] <- vector(mode="list", length=max(max_n_inf_students, max_n_inf_teachers))
    intro_student_weekly[[scenario]][[it]] <- vector(mode="list", length=max_n_inf_students)
    intro_teacher_weekly <- vector(mode="list", length=max_n_inf_teachers)
    
    symp_weekly[[scenario]][[it]] <- vector(mode="list", length=max(max_n_inc_students, max_n_inc_teachers))
    symp_student_weekly[[scenario]][[it]] <- vector(mode="list", length=max_n_inc_students)
    symp_teacher_weekly <- vector(mode="list", length=max_n_inc_teachers)
    
    # Weekly new infections
    for(n in 1:max_n_inf_students){
      # Weekly school-related infections (outside school-related + within-school)
      t_os_student <- sort(c(t_inf_student_list[[n]][[1]], t_inf_student_list[[n]][[2]]))
      os_student_weekly[[scenario]][[it]][[n]] <- sapply(1:n_weeks, function(x) length(t_os_student [t_os_student >=t_start[x] & t_os_student <=t_end[x]]))
      os_weekly[[scenario]][[it]][[n]] <- os_student_weekly[[scenario]][[it]][[n]]
      
      # Weekly introductions from community
      t_intro_student <- sort(t_inf_student_list[[n]][[3]])
      intro_student_weekly[[scenario]][[it]][[n]] <- sapply(1:n_weeks, function(x) length(t_intro_student[t_intro_student>=t_start[x] & t_intro_student<=t_end[x]]))
      intro_weekly[[scenario]][[it]][[n]] <- intro_student_weekly[[scenario]][[it]][[n]] 
    }
    
    # Weekly symptomatic individuals (school-related + outside school-related + within-school)
    for(n in 1:max_n_inc_students){
      t_inc_student <- sort(c(t_inc_student_list[[n]][[1]], t_inc_student_list[[n]][[2]]))
      symp_student_weekly[[scenario]][[it]][[n]] <- sapply(1:n_weeks, function(x) length(t_inc_student[t_inc_student>=t_start[x] & t_inc_student<=t_end[x]]))
      symp_weekly[[scenario]][[it]][[n]] <- symp_student_weekly[[scenario]][[it]][[n]]
    }
    
    # Weekly new infections
    for(n in 1:max_n_inf_teachers){
      # Weekly school-related infections (outside school-related + within-school)
      t_os_teacher <- sort(c(t_inf_teacher_list[[n]][[1]], t_inf_teacher_list[[n]][[2]]))
      os_teacher_weekly[[scenario]][[it]][[n]] <- sapply(1:n_weeks, function(x) length(t_os_teacher[t_os_teacher>=t_start[x] & t_os_teacher<=t_end[x]]))
      os_weekly[[scenario]][[it]][[n]] <- os_teacher_weekly[[scenario]][[it]][[n]] + os_weekly[[scenario]][[it]][[n]]
      # Weekly introductions from community
      t_intro_teacher <- sort(t_inf_teacher_list[[n]][[3]])
      intro_teacher_weekly[[scenario]][[it]][[n]] <- sapply(1:n_weeks, function(x) length(t_intro_teacher[t_intro_teacher>=t_start[x] & t_intro_teacher<=t_end[x]]))
      intro_weekly[[scenario]][[it]][[n]] <- intro_weekly[[scenario]][[it]][[n]] + intro_teacher_weekly[[scenario]][[it]][[n]]
    }
    
    # Weekly symptomatic individuals (school-related + outside school-related + within-school)
    for(n in 1:max_n_inc_teachers){
      t_inc_teacher <- sort(c(t_inc_teacher_list[[n]][[1]], t_inc_teacher_list[[n]][[2]]))
      symp_teacher_weekly[[scenario]][[it]][[n]] <- sapply(1:n_weeks, function(x) length(t_inc_teacher[t_inc_teacher>=t_start[x] & t_inc_teacher<=t_end[x]]))
      symp_weekly[[scenario]][[it]][[n]] <- symp_weekly[[scenario]][[it]][[n]] + symp_teacher_weekly[[scenario]][[it]][[n]]
    }
    
    
    ### Screening
    screening_stats[[scenario]][it,] <- as.data.frame(cbind(students=screening_stat$students, teachers=screening_stat$teachers))
    det_screening[[scenario]][it,] <- apply(df_screening[,c("pos_students","pos_teachers")], 2, sum)
    
    # For plotting
    for(j in 2:7){
      # Students
      ns[[j]][,it] <- sapply(seq(1, max_time), function(x) nrow(df_history %>% filter(state==types[j] | iso_state==types[j], abs(time-x)<=tol)))
      # Teachers
      nt[[j]][,it] <- sapply(seq(1, max_time), function(x) nrow(df_teach_hist %>% filter(state==types[j] | iso_state==types[j], abs(time-x)<=tol)))
    }
    # Students
    ns[[1]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_history %>% filter((state==types[1] | iso_state==types[1]) & waning == 0, abs(time-x)<=tol) %>% summarize(id))))
    ns[[8]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_history %>% filter(vacc_effective == 1, abs(time-x)<=tol) %>% summarize(id))))
    ns[[9]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_history %>% filter((state==types[1] | iso_state==types[1]) & waning == 1, abs(time-x)<=tol) %>% summarize(id))))
    ns[[10]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_history %>% filter((state==types[1] | iso_state==types[1]) & vacc_effective == 0, abs(time-x)<=tol) %>% summarize(id))))
    ns[[11]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_history %>% filter((state==types[1] | iso_state==types[1]) & vacc_effective == 1, abs(time-x)<=tol) %>% summarize(id))))
    ns[[12]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_history %>% filter(waning == 1, abs(time-x)<=tol) %>% summarize(id))))
    # Teachers
    nt[[1]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_teach_hist %>% filter((state==types[1] | iso_state==types[1]) & waning == 0, abs(time-x)<=tol) %>% summarize(id))))
    nt[[8]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_teach_hist %>% filter(vacc_effective == 1, abs(time-x)<=tol) %>% summarize(id))))
    nt[[9]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_teach_hist %>% filter((state==types[1] | iso_state==types[1]) & waning == 1, abs(time-x)<=tol) %>% summarize(id))))
    nt[[10]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_teach_hist %>% filter((state==types[1] | iso_state==types[1]) & vacc_effective == 0, abs(time-x)<=tol) %>% summarize(id))))
    nt[[11]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_teach_hist %>% filter((state==types[1] | iso_state==types[1]) & vacc_effective == 1, abs(time-x)<=tol) %>% summarize(id))))
    nt[[12]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_teach_hist %>% filter(waning == 1, abs(time-x)<=tol) %>% summarize(id))))
    
    ### STUDENTS
    vacc_ids <- unlist(df_agent[df_agent$vaccinated_2==1, "id"])
    unvacc_ids <- setdiff(df_agent$id, vacc_ids)
    # Presymptomatic infections in students
    inf_id_t <- sapply(seq(1, max_time), function(x) unlist(df_history %>% filter(state%in%types[2], abs(time-x)<=tol) %>% summarize(id)))
    ns[[13]][,it] <- do.call(rbind, lapply(inf_id_t, function(x) length(intersect(x, vacc_ids))))
    ns[[14]][,it] <- do.call(rbind, lapply(inf_id_t, function(x) length(intersect(x, unvacc_ids))))
    
    # Symptomatic infections in students
    inf_id_t <- sapply(seq(1, max_time), function(x) unlist(df_history %>% filter(state%in%types[3], abs(time-x)<=tol) %>% summarize(id)))
    ns[[15]][,it] <- do.call(rbind, lapply(inf_id_t, function(x) length(intersect(x, vacc_ids))))
    ns[[16]][,it] <- do.call(rbind, lapply(inf_id_t, function(x) length(intersect(x, unvacc_ids))))
    
    # Asymptomatic infections in students
    inf_id_t <- sapply(seq(1, max_time), function(x) unlist(df_history %>% filter(state%in%types[4], abs(time-x)<=tol) %>% summarize(id)))
    ns[[17]][,it] <- do.call(rbind, lapply(inf_id_t, function(x) length(intersect(x, vacc_ids))))
    ns[[18]][,it] <- do.call(rbind, lapply(inf_id_t, function(x) length(intersect(x, unvacc_ids))))
    
    ns[[19]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_history %>% filter(vacc_effective == 0, abs(time-x)<=tol) %>% summarize(id))))
    ns[[20]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_history %>% filter(state=="R" & vacc_effective == 1, abs(time-x)<=tol) %>% summarize(id))))
    # Vaccinated but waned immunity 
    ns[[21]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_history %>% filter(waning==1 & vacc_effective == 1, abs(time-x)<=tol) %>% summarize(id))))
    # Vaccinated but waned immunity
    ns[[22]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_history %>% filter(waning==1 & vacc_effective == 0, abs(time-x)<=tol) %>% summarize(id))))
    # Recovered but waned immunity 
    ns[[23]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_history %>% filter(waning==1 & vacc_effective == -1, abs(time-x)<=tol) %>% summarize(id))))
    
    # Number of susceptibles weighed by susceptibility either reduced due to vaccination, reinfection etc.
    ns[[24]][, it] <- sapply(seq(1, max_time), function(x) unlist(df_history %>% filter((state==types[1] | iso_state==types[1]), abs(time-x)<=tol) %>% summarize(pop_immnunity=sum(susc))))
    
    ### TEACHERS
    vacc_ids <- unlist(df_teacher[df_teacher$vaccinated_2==1, "id"])
    unvacc_ids <- setdiff(df_teacher$id, vacc_ids)
    
    # Presymptomatic infections in teachers
    inf_id_t <- sapply(seq(1, max_time), function(x) unlist(df_teach_hist %>% filter(state%in%types[2], abs(time-x)<=tol) %>% summarize(id)))
    nt[[13]][,it] <- do.call(rbind, lapply(inf_id_t, function(x) length(intersect(x, vacc_ids))))
    nt[[14]][,it] <- do.call(rbind, lapply(inf_id_t, function(x) length(intersect(x, unvacc_ids))))
    
    # Symptomatic infections in teachers
    inf_id_t <- sapply(seq(1, max_time), function(x) unlist(df_teach_hist %>% filter(state%in%types[3], abs(time-x)<=tol) %>% summarize(id)))
    nt[[15]][,it] <- do.call(rbind, lapply(inf_id_t, function(x) length(intersect(x, vacc_ids))))
    nt[[16]][,it] <- do.call(rbind, lapply(inf_id_t, function(x) length(intersect(x, unvacc_ids))))
    
    # Asymptomatic infections in teachers
    inf_id_t <- sapply(seq(1, max_time), function(x) unlist(df_teach_hist %>% filter(state%in%types[4], abs(time-x)<=tol) %>% summarize(id)))
    nt[[17]][,it] <- do.call(rbind, lapply(inf_id_t, function(x) length(intersect(x, vacc_ids))))
    nt[[18]][,it] <- do.call(rbind, lapply(inf_id_t, function(x) length(intersect(x, unvacc_ids))))
    
    nt[[19]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_teach_hist %>% filter(vacc_effective == 0, abs(time-x)<=tol) %>% summarize(id))))
    nt[[20]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_teach_hist %>% filter(state=="R" & vacc_effective == 1, abs(time-x)<=tol) %>% summarize(id))))
    nt[[21]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_teach_hist %>% filter(waning==1 & vacc_effective == 1, abs(time-x)<=tol) %>% summarize(id))))
    nt[[22]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_teach_hist %>% filter(waning==1 & vacc_effective == 0, abs(time-x)<=tol) %>% summarize(id))))
    nt[[23]][, it] <- sapply(seq(1, max_time), function(x) length(unlist(df_teach_hist %>% filter(waning==1 & vacc_effective == -1, abs(time-x)<=tol) %>% summarize(id))))
    
    # Number of susceptibles weighed by susceptibility either reduced due to vaccination, reinfection etc.
    nt[[24]][, it] <- sapply(seq(1, max_time), function(x) unlist(df_teach_hist %>% filter((state==types[1] | iso_state==types[1]), abs(time-x)<=tol) %>% summarize(pop_immunity=sum(susc))))
    # Contacts between teachers and students
    contact_student_teachers[[scenario]][[it]] <- unlist(lapply(cont_ts, function(x) length(x)))
    
    # Outbreak size students
    outbreak_size_students[[scenario]][it] <- sum(!is.na(df_agent$infector))
    outbreak_size_teachers[[scenario]][it] <- sum(!is.na(df_teacher$infector))
    
    # Outbreak size per introduction
    outbreak_data_students[[scenario]][[it]] <- table(df_agent$source)
    outbreak_data_teachers[[scenario]][[it]] <- table(df_teacher$source)
    
    # Infector 
    infector_students[[scenario]][[it]] <- table(df_agent$infector)
    infector_teachers[[scenario]][[it]] <- table(df_teacher$infector)
    
    # Symptomatic cases
    symptomatic_students[[scenario]][[it]]<- length(which(df_agent$state=="IS"))
    symptomatic_teachers[[scenario]][[it]]<- length(which(df_teacher$state=="IS"))
    
    # Outbreak size per "location"
    outbreak_per_loc_st[[scenario]][it, ] <- table(factor(df_agent$location, levels=0:2))
    outbreak_per_loc_teach[[scenario]][it, ] <- table(factor(df_teacher$location, levels=0:2))
    
    # Peak number 
    peak_students[[scenario]][it, "Asymptomatic"] <- max(ns[["IA"]][,it], na.rm=T)
    peak_students[[scenario]][it, "Pre-symptomatic"] <- max(ns[["PS"]][,it], na.rm=T)
    peak_students[[scenario]][it, "Symptomatic"] <- max(ns[["IS"]][,it], na.rm=T)
    peak_students[[scenario]][it, "Infected"] <- sum(peak_students[[scenario]][it, -4])
    
    peak_teachers[[scenario]][it, "Asymptomatic"] <- max(nt[["IA"]][,it], na.rm=T)
    peak_teachers[[scenario]][it, "Pre-symptomatic"] <- max(nt[["PS"]][,it], na.rm=T)
    peak_teachers[[scenario]][it, "Symptomatic"] <- max(nt[["IS"]][,it], na.rm=T)
    peak_teachers[[scenario]][it, "Infected"] <- sum(peak_teachers[[scenario]][it, -4])
    
    # Number of absences
    abs_days_students[[scenario]][it, "Isolated"] <- nrow(unique(df_history[df_history$iso_state=="IH" & df_history$pres==1,]))*steps
    abs_days_students[[scenario]][it, "Quarantined"] <- nrow(unique(df_history[df_history$iso_state=="Q" & df_history$pres==1,]))*steps
    abs_days_students[[scenario]][it, "Total absent"] <- abs_days_students[[scenario]][it, 1] + abs_days_students[[scenario]][it, 2]
    
    # Number of absences
    absences_students[[scenario]][it, "Isolated"] <- length(unique(df_history[df_history$iso_state=="IH","id"]))
    absences_students[[scenario]][it, "Quarantined"] <- length(unique(df_history[df_history$iso_state=="Q","id"]))
    absences_students[[scenario]][it, "Total absent"] <- absences_students[[scenario]][it, 1] + absences_students[[scenario]][it, 2]
    
    # Number of absences
    abs_days_teachers[[scenario]][it, "Isolated"] <- nrow(unique(df_teach_hist[df_teach_hist$iso_state=="IH" & df_teach_hist$pres==1,]))*steps
    abs_days_teachers[[scenario]][it, "Quarantined"] <- nrow(unique(df_teach_hist[df_teach_hist$iso_state=="Q" & df_teach_hist$pres==1,]))*steps
    abs_days_teachers[[scenario]][it, "Total absent"] <- abs_days_teachers[[scenario]][it, 1] + abs_days_teachers[[scenario]][it, 2]
    
    # Number of absences
    absences_teachers[[scenario]][it, "Isolated"] <- length(unique(df_teach_hist[df_teach_hist$iso_state=="IH","id"]))
    absences_teachers[[scenario]][it, "Quarantined"] <- length(unique(df_teach_hist[df_teach_hist$iso_state=="Q","id"]))
    absences_teachers[[scenario]][it, "Total absent"] <- absences_teachers[[scenario]][it, 1] + absences_teachers[[scenario]][it, 2]
    
    infected_students <- unique(df_agent[!is.na(df_agent$location),"id"])
    infected_teachers <- unique(df_teacher[!is.na(df_teacher$location),"id"])
    sec_cases_students[[scenario]][[it]] <- sapply(infected_students, function(x) sum(x%in%df_agent$infector)+sum(x%in%df_teacher$infector))
    sec_cases_teachers[[scenario]][[it]] <- sapply(infected_teachers, function(x) sum(x%in%df_agent$infector)+sum(x%in%df_teacher$infector))
    
    infected_symp_students <- unique(df_agent[!is.na(df_agent$location) & df_agent$state=="IS","id"])
    sec_symp_students[[scenario]][[it]] <- sapply(infected_symp_students, function(x) sum(x%in%df_agent$infector)+sum(x%in%df_teacher$infector))
    infected_symp_teachers <- unique(df_teacher[!is.na(df_teacher$location) & df_teacher$state=="IS","id"])
    sec_symp_teachers[[scenario]][[it]] <- sapply(infected_symp_teachers, function(x) sum(x%in%df_agent$infector)+sum(x%in%df_teacher$infector))
    
    # Detected students and teachers by risk-based testing
    det_risk_students[[scenario]][[it]] <- sum(df_risk_testing$pos_students)
    det_risk_teachers[[scenario]][[it]] <- sum(df_risk_testing$pos_teachers)
    
    # Infected students detected in quarantine (symptomatic)
    # Only student go into quarantine
    IS_quaran_students[[scenario]][[it]] <- length(df_agent[df_agent$quaran==1 & df_agent$state=="IS", "id"])
    IA_quaran_students[[scenario]][[it]] <- length(df_agent[df_agent$quaran==1 & df_agent$state=="IA", "id"])
    
    # Asymptomatically infected students and teachers in isolation
    IA_iso_students[[scenario]][[it]] <- length(df_agent[df_agent$iso==1 & df_agent$state=="IA", "id"])
    IA_iso_teachers[[scenario]][[it]] <- length(df_teacher[df_teacher$iso==1 & df_teacher$state=="IA", "id"])
    
    vacc_coverage1[[scenario]][[it]] <- vacc_cov1_vec[1]
    vacc_coverage2[[scenario]][[it]] <- vacc_cov2_vec[1]
    vacc_coverage2_t[[scenario]][[it]] <- rep(length(unlist(vacc_ids_t)), time_period)
  }
  end_time <- Sys.time()
  run_time <- end_time-start_time
  print(paste0("Runtime for scenario ", scenario, ": ", run_time))
  
  ns_list[[scenario]] <- ns
  nt_list[[scenario]] <- nt
}
total_end_time <- Sys.time()
total_run_time <- total_end_time-total_start_time
print(paste0("Total runtime = ", total_run_time))

# Saving output
save.image(file=paste0(resultsPath,folder,'/sim_1.RData'))