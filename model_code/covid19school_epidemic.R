# ============================================================================ #
# Main function 
# ============================================================================ #
school.epidemic <- function(seed = 12345,
                            occup = 1,                    # Occupation in classes, value = 0.5 or 1
                            steps = 8/24,                 # Time steps
                            time_period = 4*7,            # Duration of study period 
                            test_days = c(1,0,1,0,0,0,0), # Test days, corresponds to Monday, Tuesday, etc.
                            screening_adherence = 0.5,    # Adherence to screening/testing
                            risk_testing_adherence = 0.5, # Adherence to risk-based testing
                            compliance_iso = 1,           # Compliance to isolation (percentage)
                            compliance_quaran = 1,        # Compliance to quarantine (percentage)
                            isolation_flag = T,           # Flag whether symptomatic individuals will be isolated
                            quarantine_flag = T,          # Flag for Quarantine of close contacts
                            quarantine_class_flag = T,    # Flag for Quarantine of class mates
                            quarantine_fully_vacc = F,    # Flag for Quarantine of fully vaccinated
                            day_names = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
                            spec = 1,                     # Specificity of the test
                            screening_flag = F,           # Flag whether screening is implemented
                            risk_based_flag = F,          # Flag whether risk-based testing is implemented
                            vaccination_flag = F,         # Flag whether there is vaccination
                            external_prob = c(0.0006341289, 0.0006341289), # External prob. of infection from community for sutdents and teachers
                            intro_fix_per_week = NULL, 
                            cont_close, cont_class, cont_grade, cont_out_school, cont_teacher, cont_tt, cont_ts, # List with contacts for each student and teacher
                            susc_close, susc_class, susc_grade, susc_out_school, susc_teacher, susc_tt, susc_ts,# List with susceptible contacts for each student and teacher
                            sample_prop=1,  # Proportion of effective contacts (for teachers), samples a subset of students that are educated by teachers
                            prop_vaccinated = c(0, 0.85), # Total percentage of vaccinated 
                            prop_vacc_1_dose = 0.35,      # Percentage of students with one dose
                            prop_vacc_2_dose = 0.15,      # Percentage of students with two doeses
                            vacc_eff_1 = c(0.2, 0.59),    # Vaccine efficacy for first dose (students)
                            vacc_eff_2 = c(0.78, 0.9),    # Vaccine efficacy for two doses (students)
                            vacc_eff_teacher_1 = c(0.2, 0.59), # Vaccine efficacy for first dose (teachers)
                            vacc_eff_teacher_2 = c(0.78, 0.9), # Vaccine efficacy for two doses (teachers)
                            vacc_rate_weekly_1 = 0.01,    # Weekly vaccination rate of first dose
                            vacc_rate_weekly_2 = 0.01,    # Weekly vaccination rate of two doses
                            vacc_weekly_flag = 0,         # Flag whether weekly increasing vaccination coverage 
                            t_2nd_dose = 21, 
                            vacc_eff_lag = c(14,14),      # Lag between vaccination and vaccine efficacy
                            prop_student_boost = 0,       # Proportion of vaccinated students with booster dose
                            prop_teacher_boost = 0,       # Proportion of vaccinated teachers with boooster dose
                            VE_booster_increase = 1.2,    # Increase of VE after booster
                            scaling_factor = 0.8, # Scaling the proportion of symptomatics
                            scaling_constant = F,
                            vacc_rate_2 = 0.18,
                            cont_susc_scale = c(0.5, 0.85),
                            aerosol_susc_scale = c(0.001,0.005),
                            aerosol = F, 
                            no_intervention=F,
                            vacc_scr_corr=0, # Can be 0, -1, or 1
                            df_time_inf_students = NULL, # Daily probability of time of infection (based on COVID-19 incidence in NL)
                            df_time_inf_teachers = NULL, # Daily probability of time of infection (based on COVID-19 incidence in NL)
                            df_prob_reinfection_students = NULL,
                            df_prob_reinfection_teachers = NULL,
                            df_prob_reinfection_students_2 = NULL,
                            df_prob_reinfection_teachers_2 = NULL,
                            df_prob_vacc_students = NULL,
                            df_prob_vacc_teachers = NULL,
                            lag_reinfection = 0,            # Time between infection and start of waning immunity
                            rel_susc_waning = 1.0,          
                            vacation = seq(6*7, 8*7, by=1), # Time where nothing happens
                            R0_vec = 1.2,                   # Basic school-related R
                            rec_time = c(14,10),
                            tol=1e-5
){
  set.seed(seed)
  # -------------------------------------------------------------------------- #
  # Time
  # -------------------------------------------------------------------------- #
  t_0 <- 1 # Start time
  time_steps = seq(t_0, time_period, by = steps)
  time_names = day.of.the.week(time_steps, day_names) # Assign day of the week to time_steps
  # For each time step:
  contacts_in_school = contacts.in.school(time_steps, time_names) # Are contacts in school taking place?
  contacts_out_school = contacts.leisure(time_steps, time_names)  # Are contacts outside school taking place?
  no_contacts = no.contacts(time_steps) # Are no contacts taking place? 
  
  remaining_flag <- remaining_flag_1 <- remaining_flag_2 <- 1
  
  # -------------------------------------------------------------------------- #
  # Initialization for reinfection
  # -------------------------------------------------------------------------- #
  reinfected_students <- reinfected_students_rec <- reinfected_students_vacc <- vector(mode="list", length=time_period)
  reinfected_teachers <- reinfected_teachers_rec <- reinfected_teachers_vacc <- vector(mode="list", length=time_period)
  df_reinfected_students <- df_reinfected_teachers <- as.data.frame(matrix(0, ncol=3, nrow=time_period))
  colnames(df_reinfected_students) <- colnames(df_reinfected_teachers) <- c("recovered", "vaccinated", "total") 
  
  # -------------------------------------------------------------------------- #
  # Initialize data frames for students and teachers and contact network
  # -------------------------------------------------------------------------- #
  t_inf_student_list_1 <- t_inc_student_list_1 <- t_inf_teacher_list_1 <- t_inc_teacher_list_1 <- NULL
  t_inf_student_list_2 <- t_inc_student_list_2 <- t_inf_teacher_list_2 <- t_inc_teacher_list_2 <- NULL
  external_inf_student_ids <- external_inf_teacher_ids <- NULL
  screening_stat <- risk_stat <- NULL
  init <- init.vars(sero_prev_students = 0.35, 
                    sero_prev_teacher = 0.3,
                    occup = occup, 
                    group_per_step = group_per_step, 
                    class_size = class_size, 
                    n_grades = n_grades, 
                    n_cont_same_class = n_cont_same_class, 
                    n_cont_vec = n_cont_vec, 
                    n_cont_close = n_cont_close, 
                    n_cont_out_school = n_cont_out_school, 
                    n_cont_other = n_cont_other, 
                    n_subjects = n_subjects, 
                    n_quaran = n_quaran, 
                    n_cont_teachers = n_cont_teachers, 
                    compliance_iso = compliance_iso, 
                    compliance_quaran = compliance_quaran)
  df_history <- init$df_history 
  df_agent <- init$df_agent
  df_teach_hist <- init$df_teach_hist 
  df_teacher <- init$df_teacher 
  df_teacher_gcs <- init$df_teacher_gcs
  df_screening <- init$df_screening 
  screening_list <- init$screening_list 
  df_risk_testing <- init$df_risk_testing 
  risk_testing_list <- init$risk_testing_list
  init_rec_student_ids <- init$init_rec_student_ids
  init_rec_teacher_ids <- init$init_rec_teacher_ids
  cont_close <- init$cont_close 
  cont_class <- init$cont_class 
  cont_grade <- init$cont_grade 
  cont_out_school <- init$cont_out_school
  cont_teacher <- init$cont_teacher 
  cont_tt <- init$cont_tt 
  susc_close <- init$susc_close 
  susc_class <- init$susc_class 
  susc_grade <- init$susc_grade 
  susc_out_school <- init$susc_out_school
  susc_teacher <- init$susc_teacher 
  susc_tt <- init$susc_tt
  
  # Total number of students
  n_students <- length(unique(df_agent$id))
  
  # Effective vaccine effectiveness (accounting for lag)
  eff_vacc_cov <- rep(1, time_period)
  
  # -------------------------------------------------------------------------- #
  # Fixed number of introductions per day
  # -------------------------------------------------------------------------- #
  # Warning: This only works for daily time intervals (time_period is in days) 
  # and if there is at most one introduction per day
  intro_per_day <- vector(mode="list", length=2)
  if(!is.null(intro_fix_per_week)){
    intro_per_day[[1]] <- as.numeric(runif(1)<rexp(time_period, rate=7/intro_fix_per_week[1]))
    intro_per_day[[2]] <- as.numeric(runif(1)<rexp(time_period, rate=7/intro_fix_per_week[2]))
  }else intro_per_day[[1]] <- intro_per_day[[2]] <-  rep(1, time_period)
  
  # -------------------------------------------------------------------------- #
  # Vaccinated students prior to study period
  # -------------------------------------------------------------------------- #
  # Vectors to save vaccination coverage for 1st and 2nd dose
  vacc_cov1_vec <- rep(0, length(time_steps[time_steps%%1==0]))
  vacc_cov2_vec <- rep(0, length(time_steps[time_steps%%1==0]))
  max_vacc_coverage <- ceiling(prop_vaccinated[1]*n_students)
  # The following part is to account for weekly increase of vaccination coverage
  # Currently not in use for medium-term predictions
  if(prop_vacc_1_dose[1]!=0 & prop_vacc_2_dose[1]!=0){
    student_ids <- unique(df_agent$id)
    vacc_stud_1_dose <- student_ids[sample(c(F,T), size=length(student_ids), prob=c(1-prop_vacc_1_dose[1], prop_vacc_1_dose[1]), replace=T)==1]
    vacc_stud_2_dose <- vacc_stud_1_dose
    if(prop_vacc_1_dose[1]==prop_vacc_2_dose[1]){
      df_agent[df_agent$id%in%vacc_stud_1_dose, "vaccinated_1"] <- 1 
      df_agent[df_agent$id%in%vacc_stud_2_dose, "vaccinated_2"] <- 1
    }else{
      remaining_student_ids <- setdiff(student_ids, vacc_stud_1_dose)
      new_prop <- ceiling(prop_vacc_2_dose[1]*length(student_ids))/length(remaining_student_ids)
      vacc_stud_2_dose <- remaining_student_ids[sample(c(F,T), size=length(remaining_student_ids), prob=c(1-new_prop, new_prop), replace=T)==1]
      vacc_students <- unique(c(vacc_stud_1_dose, vacc_stud_2_dose))
      # Students with one dose
      df_agent[df_agent$id%in%vacc_stud_1_dose, "vaccinated_1"] <- 1
      # Students with two doses
      df_agent[df_agent$id%in%vacc_stud_2_dose, "vaccinated_1"] <- 1 
      df_agent[df_agent$id%in%vacc_stud_2_dose, "vaccinated_2"] <- 1 
    }
    # Set vaccination times
    if(vacc_rate_2<1){
      n_vacc2dose_now <- vacc_rate_2*length(student_ids)
      prop_vacc2dose_now <- n_vacc2dose_now/length(vacc_stud_1_dose)
      vacc2dose_now <- vacc_stud_1_dose[sample(c(F,T), size=length(vacc_stud_1_dose), prob=c(1-prop_vacc2dose_now, prop_vacc2dose_now), replace=T)]
      df_agent[df_agent$id%in%vacc2dose_now, "t_vacc_1"] <- -21
      df_agent[df_agent$id%in%setdiff(vacc_stud_1_dose, vacc2dose_now), "t_vacc_1"] <- -14
    }
    # ------------------------------------------------------------------------ #
    # Draw vaccination times according to vaccination probability
    if(!is.null(df_prob_vacc_students)){
      # df_prob_vacc_students gives the probability of being vaccinated on a
      # certain day prior to the study period according to Dutch vaccination data
      # t_vacc_2 gets overwritten when booster doeses are considered
      df_agent[df_agent$id%in%vacc_stud_2_dose, "t_vacc_2"] <- as.numeric(sample(df_prob_vacc_students$time, size=length(vacc_stud_2_dose), prob=df_prob_vacc_students$prob, replace=T))
      df_agent[df_agent$id%in%vacc_stud_1_dose, "t_vacc_1"] <- df_agent[df_agent$id%in%vacc_stud_2_dose, "t_vacc_2"]
    }else{
      df_agent[df_agent$id%in%vacc_stud_2_dose, "t_vacc_1"] <- -21
      df_agent[df_agent$id%in%vacc_stud_2_dose, "t_vacc_2"] <- -14
    }
    df_history[df_history$id%in%vacc_stud_2_dose, "vacc_effective"] <- 1
  }
  
  # -------------------------------------------------------------------------- #
  # Vaccinated teachers
  # -------------------------------------------------------------------------- #
  vacc_teachers <- sample(c(0,1), size=nrow(df_teacher), prob=c(1-prop_vaccinated[2], prop_vaccinated[2]), replace=T)
  vacc_teacher_ids <- df_teacher$id[vacc_teachers==1]
  df_teacher[, "vaccinated_1"] <- as.numeric(vaccination_flag)*vacc_teachers
  df_teacher[, "vaccinated_2"] <- as.numeric(vaccination_flag)*vacc_teachers
  if(!is.null(df_prob_vacc_teachers)){
    df_teacher[df_teacher$id%in%vacc_teacher_ids, "t_vacc_2"] <- as.numeric(sample(df_prob_vacc_teachers$time, size=length(vacc_teacher_ids), prob=df_prob_vacc_teachers$prob, replace=T))
    df_teacher[df_teacher$id%in%vacc_teacher_ids, "t_vacc_1"] <- df_teacher[df_teacher$id%in%vacc_teacher_ids, "t_vacc_2"]
  }else{
    df_teacher[df_teacher$id%in%vacc_teacher_ids,"t_vacc_1"] <- -21
    df_teacher[df_teacher$id%in%vacc_teacher_ids,"t_vacc_2"] <- -14
  }
  df_teach_hist[df_teach_hist$id%in%vacc_teacher_ids, "vacc_effective"] <- 1
  
  # -------------------------------------------------------------------------- #
  # Reduced susceptibility of students and due to vaccination
  # -------------------------------------------------------------------------- #
  df_agent$susc = susc.scale(n_students)
  df_agent$init_susc = df_agent$susc
  df_agent[df_agent$id%in%init_rec_student_ids,"susc"] = 0
  
  df_teacher$init_susc = 1
  df_teacher[df_teacher$id%in%init_rec_teacher_ids,"susc"] = 0
  df_teacher$susc = 1
  # Save susceptibility values in df_history/df_teach_hist
  df_history$susc = 1
  df_teach_hist$susc = 1
  # Adapt susceptibility values with respect to vaccination
  if(vaccination_flag){
    vaccination_vec = vector(mode="list", length=2)
    vaccination_vec[[1]] <- reduced.susc.vacc(1, df_agent, df_agent$id, 
                                              vacc_eff_1 = vacc_eff_1, vacc_eff_2 = vacc_eff_2, 
                                              vacc_eff_lag = vacc_eff_lag)
    vaccination_vec[[2]] <- reduced.susc.vacc(1, df_teacher, df_teacher$id, 
                                              vacc_eff_1 = vacc_eff_teacher_1, vacc_eff_2 = vacc_eff_teacher_2, 
                                              vacc_eff_lag = vacc_eff_lag)
    df_agent$vacc_susc = vaccination_vec[[1]]
    df_teacher$vacc_susc = vaccination_vec[[2]]
    df_agent$susc = df_agent$susc*df_agent$vacc_susc
    df_agent[df_agent$id%in%init_rec_student_ids,"susc"] = 0
    df_teacher$susc = vaccination_vec[[2]]
    df_teacher[df_teacher$id%in%init_rec_teacher_ids,"susc"] = 0
    
    # Save susceptibility values in df_history/df_teach_hist
    df_history$susc = rep(df_agent$susc, each = length(time_steps))
    df_teach_hist$susc = rep(df_teacher$susc, each = length(time_steps))
  }

  
  # -------------------------------------------------------------------------- #
  # Infection times for recovered individuals and initial reinfections
  # -------------------------------------------------------------------------- #
  # 1. For each recovered individual, draw a previous time of infection 
  #    (based on the incidence of the past waves in NL)
  # 2. For each recovered individual, flip a coin of whether state is changed 
  #    from "R"to "S" according to the daily probability of reinfection based 
  #    on literature
  student_reinfections <- reinfection(t_i = t_0,
                                      df_agent = df_agent,
                                      df_history = df_history,
                                      df_time_inf = df_time_inf_students,
                                      df_prob_reinfection = df_prob_reinfection_students,
                                      df_prob_reinfection_2 = df_prob_reinfection_students_2,
                                      lag_reinfection = lag_reinfection,
                                      init_prev_inf = T, 
                                      rel_susc_waning = rel_susc_waning)
  df_agent <- student_reinfections$df_agent
  df_history <- student_reinfections$df_history
  reinfected_student_ids <- student_reinfections$reinfected_ids
  reinfected_students[[t_0]] <- student_reinfections$reinfected_ids
  reinfected_students_rec[[t_0]] <- student_reinfections$reinfected_rec_ids
  reinfected_students_vacc[[t_0]] <- student_reinfections$reinfected_vacc_ids
  df_reinfected_students[t_0, "recovered"] <- length(student_reinfections$reinfected_rec_ids)
  df_reinfected_students[t_0, "vaccinated"] <- length(student_reinfections$reinfected_vacc_ids)
  df_reinfected_students[t_0, "total"] <- length(student_reinfections$reinfected_ids)

  # Update susc_close, susc_class using cont_close, cont_class
  susc_student_ids <- df_history[abs(df_history$time-t_0)<=tol & df_history$state=="S" & df_history$pres==1, "id"]
  # Check whether reinfected students are susceptible
  if(!all(reinfected_student_ids%in%susc_student_ids)){
    print(paste0("Not all reinfected students are susceptible at time ", t_0))
    print(reinfected_student_ids[!reinfected_student_ids%in%susc_student_ids])
  }
  susc_close <- lapply(cont_close, function(x) intersect(unlist(x), susc_student_ids))
  susc_class <- lapply(cont_class, function(x) intersect(unlist(x), susc_student_ids))
  susc_grade <- lapply(cont_grade, function(x) intersect(unlist(x), susc_student_ids))
  susc_out_school <- lapply(cont_out_school, function(x) intersect(unlist(x), susc_student_ids))
  susc_teacher <- lapply(cont_teacher, function(x) intersect(unlist(x), susc_student_ids))
  ### susc_ts does not exist yet and does not have to be updated!

  teacher_reinfections <- reinfection(t_i = t_0,
                                      df_agent = df_teacher,
                                      df_history= df_teach_hist,
                                      df_time_inf = df_time_inf_teachers,
                                      df_prob_reinfection = df_prob_reinfection_teachers,
                                      df_prob_reinfection_2 = df_prob_reinfection_teachers_2,
                                      lag_reinfection = lag_reinfection,
                                      init_prev_inf = T, 
                                      rel_susc_waning = rel_susc_waning)
  df_teacher <- teacher_reinfections$df_agent
  df_teach_hist <- teacher_reinfections$df_history
  reinfected_teacher_ids <- teacher_reinfections$reinfected_ids
  reinfected_teachers[[t_0]] <- teacher_reinfections$reinfected_ids
  reinfected_teachers_rec[[t_0]] <- teacher_reinfections$reinfected_rec_ids
  reinfected_teachers_vacc[[t_0]] <- teacher_reinfections$reinfected_vacc_ids
  df_reinfected_teachers[t_0, "recovered"] <- length(teacher_reinfections$reinfected_rec_ids)
  df_reinfected_teachers[t_0, "vaccinated"] <- length(teacher_reinfections$reinfected_vacc_ids)
  df_reinfected_teachers[t_0, "total"] <- length(teacher_reinfections$reinfected_ids)
  
  susc_teacher_ids <- df_teach_hist[abs(df_teach_hist$time-t_0)<=tol & df_teach_hist$state=="S", "id"]
  if(!all(reinfected_teacher_ids%in%susc_teacher_ids)){
    print(paste0("Not all reinfected teachers are susceptible at time ", t_0))
    print(reinfected_teacher_ids[!reinfected_teacher_ids%in%susc_teacher_ids])
  }
  susc_tt <- lapply(cont_tt, function(x) intersect(unlist(x), susc_teacher_ids))

  
  # -------------------------------------------------------------------------- #
  # Adherence to screening and risk-based testing
  # -------------------------------------------------------------------------- #
  if(screening_flag){
    df_agent <- assign.screening.adherence(df_agent = df_agent, screening_adherence = screening_adherence, vacc_scr_corr = vacc_scr_corr, only.eligible=T)
    df_teacher <- assign.screening.adherence(df_agent = df_teacher, screening_adherence = screening_adherence, vacc_scr_corr = vacc_scr_corr, only.eligible=T)
    # Sample individuals who will be compliant to isolation and quarantine
    df_agent$iso_compliance[df_agent$adherence==0] <- sample(c(0,1), size=length(which(df_agent$adherence==0)), prob=c(1-compliance_iso, compliance_iso), replace=T)
    df_agent$iso_compliance[df_agent$adherence==1] <- sample(c(0,1), size=length(which(df_agent$adherence==1)), prob=c(1-compliance_iso, compliance_iso), replace=T)
    # Sample individuals who will be compliant to isolation and quarantine
    df_teacher$iso_compliance[df_teacher$adherence==0] <- sample(c(0,1), size=length(which(df_teacher$adherence==0)), prob=c(1-compliance_iso, compliance_iso), replace=T)
    df_teacher$iso_compliance[df_teacher$adherence==1] <- sample(c(0,1), size=length(which(df_teacher$adherence==1)), prob=c(1-compliance_iso, compliance_iso), replace=T)
  } 
  
  if(risk_based_flag){
    df_agent$adherence <- sample(c(0,1), size=length(df_agent$id), prob=c(1-risk_testing_adherence, risk_testing_adherence), replace=T)
    df_teacher$adherence <- sample(c(0,1), size=length(df_teacher$id), prob=c(1-risk_testing_adherence, risk_testing_adherence), replace=T)
    # Sample individuals who will be compliant to isolation and quarantine
    df_agent$quaran_compliance[df_agent$adherence==0] <- sample(c(0,1), size=length(which(df_agent$adherence==0)), prob=c(1-compliance_quaran, compliance_quaran), replace=T)
    df_agent$quaran_compliance[df_agent$adherence==1] <- sample(c(0,1), size=length(which(df_agent$adherence==1)), prob=c(1-compliance_quaran, compliance_quaran), replace=T)
    # Sample individuals who will be compliant to isolation and quarantine
    df_teacher$quaran_compliance[df_teacher$adherence==0] <- sample(c(0,1), size=length(which(df_teacher$adherence==0)), prob=c(1-compliance_quaran, compliance_quaran), replace=T)
    df_teacher$quaran_compliance[df_teacher$adherence==1] <- sample(c(0,1), size=length(which(df_teacher$adherence==1)), prob=c(1-compliance_quaran, compliance_quaran), replace=T)
  }else{ # Compliance to isolation and quarantine when no risk-based testing is implemented
    # Sample individuals who will be compliant to isolation and quarantine
    df_agent$quaran_compliance <- sample(c(0,1), size=length(df_agent$id), prob=c(1-compliance_quaran, compliance_quaran), replace=T)
    df_teacher$quaran_compliance <- sample(c(0,1), size=length(df_teacher$id), prob=c(1-compliance_quaran, compliance_quaran), replace=T)
  }
  
  inf_count <- 0
  infecteds <- NULL
  pos_tested_ids <- pos_tested_teacher_ids <- NULL
  # -------------------------------------------------------------------------- #
  # SIMULATION
  # -------------------------------------------------------------------------- #
  id_students_vacc <- df_agent[df_agent$vaccinated_2==1, "id"]
  id_teachers_vacc <- df_teacher[df_teacher$vaccinated_2==1, "id"]
  id_students_boost <- id_students_vacc[sample(c(F,T), size=length(id_students_vacc), prob=c(1-prop_student_boost, prop_student_boost), replace=T)]
  id_teachers_boost <- id_teachers_vacc[sample(c(F,T), size=length(id_teachers_vacc), prob=c(1-prop_teacher_boost, prop_teacher_boost), replace=T)]
  n_students_boost <- length(id_students_boost)
  n_teachers_boost <- length(id_teachers_boost)
  
  for(t_i in time_steps){
    R0 = R0_vec[abs(time_steps-t_i)<=tol]
    # print(paste("R0=", R0))
    if(t_i%%1==0){
      vacc_cov1_vec[t_i] <- sum(df_agent[df_agent$t_vacc_1!=0, "vaccinated_1"])/nrow(df_agent)
      vacc_cov2_vec[t_i] <- sum(df_agent$vaccinated_2)/nrow(df_agent)
      # ---------------------------------------------------------------------- #
      # Booster vaccination annually at the end of summer
      # Randomly assign a new vaccination time that took place in the previous
      # summer holidays
      # ---------------------------------------------------------------------- #
      if(t_i%in%booster_time){
        if(t_i%in%summer_vacation_1){
          df_agent[df_agent$id%in%id_students_boost, "t_vacc_2"] <- sample(summer_vacation_1, size=n_students_boost, replace=T)
          df_teacher[df_teacher$id%in%id_teachers_boost, "t_vacc_2"] <- sample(summer_vacation_1, size=n_teachers_boost, replace=T)
        }
        if(t_i%in%summer_vacation_2){
          df_agent[df_agent$id%in%id_students_boost, "t_vacc_2"] <- sample(summer_vacation_2, size=n_students_boost, replace=T)
          df_teacher[df_teacher$id%in%id_teachers_boost, "t_vacc_2"] <- sample(summer_vacation_2, size=n_teachers_boost, replace=T)
        }
        if(t_i%in%summer_vacation_3){
          df_agent[df_agent$id%in%id_students_boost, "t_vacc_2"] <- sample(summer_vacation_3, size=n_students_boost, replace=T)
          df_teacher[df_teacher$id%in%id_teachers_boost, "t_vacc_2"] <- sample(summer_vacation_3, size=n_teachers_boost, replace=T)
        }
        # Reset susceptibility value to original value
        boost_vacc_susc <- susc.vacc.scale(n_students_boost, min=1-vacc_eff_2[2]*VE_booster_increase, max=1-vacc_eff_2[1]*VE_booster_increase)
        for(ind_b in 1:n_students_boost){
          b <- id_students_boost[ind_b]
          df_agent[df_agent$id==b, "susc"] <- min(df_agent[df_agent$id==x, "susc"], boost_vacc_susc[ind_b]*df_agent[df_agent$id==b, "init_susc"])
          df_history[df_history$id==b & df_history$time+tol >= t_i, "susc"] <- df_agent[df_agent$id==b, "susc"]
        }
        
        boost_vacc_susc <- susc.vacc.scale(n_teachers_boost, min=1-vacc_eff_teacher_2[2]*VE_booster_increase, max=1-vacc_eff_teacher_2[1]*VE_booster_increase)
        for(ind_b in 1:n_teachers_boost){
          b <- id_teachers_boost[ind_b]
          df_teacher[df_teacher$id==b, "susc"] <- min(df_teacher[df_teacher$id==b, "susc"], boost_vacc_susc[ind_b])
          df_teach_hist[df_teach_hist$id==b & df_teach_hist$time+tol >= t_i, "susc"] <- df_teacher[df_teacher$id==b, "susc"]
        }
        
        
        # df_agent[df_agent$id%in%id_students_boost, "susc"] <- unlist(sapply(id_students_boost, function(x) min(df_agent[df_agent$id==x, "susc"], df_agent[df_agent$id==x, "vacc_susc"]*df_agent[df_agent$id==x, "init_susc"])))
        # df_teacher[df_teacher$id%in%id_teachers_boost, "susc"] <- unlist(sapply(id_teachers_boost, function(x) min(df_teacher[df_teacher$id==x, "susc"], df_teacher[df_teacher$id==x, "vacc_susc"])))
        
        # df_agent[df_agent$id%in%id_students_boost, "susc"] <- df_agent[df_agent$id%in%id_students_boost, "vacc_susc"]*df_agent[df_agent$id%in%id_students_boost, "init_susc"]
        # df_teacher[df_teacher$id%in%id_teachers_boost, "susc"] <- df_teacher[df_teacher$id%in%id_teachers_boost, "vacc_susc"]
      }
      
      
      # ---------------------------------------------------------------------- #
      # Sample contacts for teachers and students
      # Note: Less readable but faster than "for loop"
      # ---------------------------------------------------------------------- #
      # Determine classes that teacher educates
      df_gc_list <- lapply(df_teacher$id, function(id_t) df_teacher_gcs[df_teacher_gcs$id==id_t, c("grade","class")])
      # Students that are present in class
      class_student_ids_list <- lapply(df_gc_list, function(df_grade_class) unique(unlist(apply(df_grade_class, 1, function(x) df_history %>% filter(grade==x["grade"],class==x["class"], iso_state=="P", abs(time-t_i)<=tol, pres==1) %>% summarize(id)))))
      # Sample effective contacts
      # For each teacher, which students contacts?
      cont_ts <- lapply(class_student_ids_list, function(class_student_ids) sample(class_student_ids, size=ceiling(sample_prop*length(class_student_ids))))
      names(cont_ts) <- unique(df_teacher$id)
      # Set the same contacts for students (to ensure symmetric contact matrix)
      # For each student, which teacher contacts?
      cont_teacher <- lapply(1:length(cont_teacher), function(x) sapply(1:length(cont_ts), function(y) ifelse(names(cont_teacher)[[x]]%in%cont_ts[[y]], names(cont_ts)[[y]], NA)))
      cont_teacher <- lapply(cont_teacher, function(x) x[!is.na(x)])
      names(cont_teacher) <- unique(df_agent$id)
      susc_teacher <- lapply(cont_teacher, function(x) setdiff(unlist(x), df_teacher[df_teacher$state!="S","id"]))
      names(susc_teacher) <- unique(df_agent$id)
      susc_teacher <- update.susc.contacts(t_i, df_teach_hist, susc_teacher)
      susc_ts <- update.susc.contacts(t_i, df_history, cont_ts)  
      
      # ------------------------------------------------------------------------ #
      # Introductions from community
      # ------------------------------------------------------------------------ #
      # Students
      if(intro_per_day[[1]][t_i]==1){
        external_inf_students <- external.introductions(t_i, df_history, df_agent, external_prob[1], teacher=F, vaccination_flag=vaccination_flag, vacc_eff_1=vacc_eff_1, vacc_eff_2=vacc_eff_2, vacc_eff_lag = vacc_eff_lag, scaling_factor=scaling_factor, scaling_constant=scaling_constant, intro_fix_per_week=intro_fix_per_week[1])
        df_history <- external_inf_students$df_history
        df_agent <- external_inf_students$df_agent
        external_inf_student_ids <- c(external_inf_student_ids, external_inf_students$external_inf_ids)
      }
      
      # Teachers
      if(intro_per_day[[2]][t_i]==1){
        external_inf_teachers <- external.introductions(t_i, df_teach_hist, df_teacher, external_prob[2], teacher=T, vaccination_flag=vaccination_flag, vacc_eff_1=vacc_eff_1, vacc_eff_2=vacc_eff_2, vacc_eff_lag = vacc_eff_lag, scaling_factor=scaling_factor, scaling_constant=scaling_constant, intro_fix_per_week=intro_fix_per_week[2])
        df_teach_hist <- external_inf_teachers$df_history
        df_teacher <- external_inf_teachers$df_agent
        external_inf_teacher_ids <- c(external_inf_teacher_ids, external_inf_teachers$external_inf_ids)
      }
      
      
      # ------------------------------------------------------------------------ #
      # Update the list of susceptible contacts
      # ------------------------------------------------------------------------ #
      # Add everyone who is in "S" to the susceptible contacts
      susc_close <- update.susc.contacts(t_i, df_history, susc_close)
      susc_class <- update.susc.contacts(t_i, df_history, susc_class)
      susc_grade <- update.susc.contacts(t_i, df_history, susc_grade)
      susc_out_school <- update.susc.contacts(t_i, df_history, susc_out_school)
      susc_ts <- update.susc.contacts(t_i, df_history, susc_ts)
      susc_teacher <- update.susc.contacts(t_i, df_teach_hist, susc_teacher)
      susc_tt <- update.susc.contacts(t_i, df_teach_hist, susc_tt)
      
      # ------------------------------------------------------------------------ #
      # Isolation of symptomatic individuals
      # ------------------------------------------------------------------------ #
      # Students
      if(isolation_flag){
        isolated_students <- isolation(t_i, df_history, df_agent)
        df_history <- isolated_students$df_history
        df_agent <- isolated_students$df_agent
        pos_tested_ids <- isolated_students$pos_tested_ids
        # Teachers
        isolated_teachers <- isolation(t_i, df_teach_hist, df_teacher)
        df_teach_hist <- isolated_teachers$df_history
        df_teacher <- isolated_teachers$df_agent
        pos_tested_teacher_ids <- isolated_teachers$pos_tested_ids
        # ------------------------------------------------------------------------ #
        # Update the list of susceptible contacts
        # ------------------------------------------------------------------------ #
        # Add everyone who is in "S" to the susceptible contacts
        susc_close <- update.susc.contacts(t_i, df_history, susc_close)
        susc_class <- update.susc.contacts(t_i, df_history, susc_class)
        susc_grade <- update.susc.contacts(t_i, df_history, susc_grade)
        susc_out_school <- update.susc.contacts(t_i, df_history, susc_out_school)
        susc_teacher <- update.susc.contacts(t_i, df_teach_hist, susc_teacher)
        susc_tt <- update.susc.contacts(t_i, df_teach_hist, susc_tt)
        susc_ts <- update.susc.contacts(t_i, df_history, susc_ts)
      }
      
      # ------------------------------------------------------------------------ #
      # Release students from quarantine after day 5 if antigen test is negative
      # ------------------------------------------------------------------------ #
      if(quarantine_flag){
        released_students <- test.after.quarantine(t_i, 
                                                   df_history, df_agent, 
                                                   df_teach_hist, df_teacher, 
                                                   cont_close, cont_class, cont_grade, cont_teacher,
                                                   susc_close, susc_class, susc_grade, susc_out_school, 
                                                   susc_teacher, susc_tt, susc_ts,
                                                   tol=tol)
        df_history <- released_students$df_history
        df_agent <- released_students$df_agent
        susc_close <- released_students$susc_close
        susc_class <- released_students$susc_class
        susc_grade <- released_students$susc_grade
        susc_out_school <- released_students$susc_out_school
        susc_ts <- released_students$susc_ts
      }
      
      # ------------------------------------------------------------------------ #
      # Weekly vaccination: 1st dose
      # ------------------------------------------------------------------------ #
      # (Always on Mondays)
      # Number are given in relative number
      # vacc_rate_weekly_1: 1 dose
      # ------------------------------------------------------------------------ #
      vacc_coverage <- sum(df_agent$vaccinated_1==1)/n_students
      if(vacc_weekly_flag==1 & time_names[abs(time_steps-t_i)<=tol]%in%c("Tuesday") & remaining_flag==1 & vacc_coverage <= prop_vaccinated[1]){
        # 1. Determine all remaining unvaccinated students 
        remaining_unvaccinated_id_1 <- df_agent[df_agent$vaccinated_1==0, "id"]
        n_remaining_unvaccinated_1 <- length(remaining_unvaccinated_id_1)
        curr_vacc_cov_1 <- sum(df_agent$vaccinated_1==1)
        # 2. Randomly choose vacc_rate_weekly that get vaccinated
        if(n_remaining_unvaccinated_1>0 & curr_vacc_cov_1 < max_vacc_coverage){
          # Vaccinate vacc_rate_weekly_1 (at most the remaining unvaccinated)
          vacc_rate_1 <- min(vacc_rate_weekly_1, max_vacc_coverage-curr_vacc_cov_1)
          # prob_vacc_weekly_1 <- min(1, vacc_rate_1/n_remaining_unvaccinated_1)
          # vacc_1 <- sample(c(F,T), size=n_remaining_unvaccinated_1, prob=c(1-prob_vacc_weekly_1, prob_vacc_weekly_1), replace=T)
          if(length(remaining_unvaccinated_id_1)==1) vacc_1 <- remaining_unvaccinated_id_1
          else vacc_1 <- sample(remaining_unvaccinated_id_1, size=vacc_rate_1)
          df_agent[df_agent$id%in%vacc_1, "vaccinated_1"] <- 1
          df_agent[df_agent$id%in%vacc_1, "t_vacc_1"] <- t_i
          # df_agent[df_agent$id%in%remaining_unvaccinated_id_1[vacc_1], "vaccinated_1"] <- 1
          # df_agent[df_agent$id%in%remaining_unvaccinated_id_1[vacc_1], "t_vacc_1"] <- t_i
        }else remaining_flag <- 0
        # Update susceptibility vector
        current_vacc_1 <- df_agent[df_agent$vaccinated_1==1 & df_agent$vaccinated_2==0, "id"]
        vacc_vec_1 <- reduced.susc.vacc(t_i, df_agent, current_vacc_1, 
                                        vacc_eff_1 = vacc_eff_1, 
                                        vacc_eff_2 = vacc_eff_2, 
                                        vacc_eff_lag = vacc_eff_lag)
        df_agent[df_agent$id%in%current_vacc_1, "vacc_susc"] <- vacc_vec_1
        df_agent[df_agent$id%in%current_vacc_1, "susc"] <- df_agent[df_agent$id%in%current_vacc_1, "susc"]*vacc_vec_1
      }
      # ------------------------------------------------------------------------ #
      # Weekly vaccination: 2nd dose
      # ------------------------------------------------------------------------ #
      # 1. Go through all vaccinated individuals with exactly one dose
      # 2. Check whether t_i >= t_vacc_1 + t_2nd_dose
      # 3. Vaccinate with 2nd dose
      # ------------------------------------------------------------------------ #
      if(remaining_flag_2==1 & vacc_weekly_flag==1){
        vacc_id_1 <- df_agent[df_agent$vaccinated_1==1 & df_agent$vaccinated_2==0, "id"]
        if(length(vacc_id_1)==0) remaining_flag_2 <- 0
        else{
          t_vaccinated_1 <- df_agent[df_agent$id%in%vacc_id_1, "t_vacc_1"]
          if(t_i<=8){ # This is for immediate second dose
            eligible_2nd_dose <- which(vacc_id_1%in%vacc2dose_now)
          }else{
            eligible_2nd_dose <- t_vaccinated_1 + t_2nd_dose - t_i <= tol
          }
          vacc_2 <- vacc_id_1[eligible_2nd_dose]
          df_agent[df_agent$id%in%vacc_2, "vaccinated_2"] <- 1
          df_agent[df_agent$id%in%vacc_2, "t_vacc_2"] <- t_i
          # Update susceptibility vector 
          current_vacc_2 <- df_agent[df_agent$vaccinated_1==1 & df_agent$vaccinated_2==1, "id"]
          vacc_vec_2 <- reduced.susc.vacc(t_i, df_agent, current_vacc_2, 
                                          vacc_eff_1 = vacc_eff_1, 
                                          vacc_eff_2 = vacc_eff_2, 
                                          vacc_eff_lag = vacc_eff_lag)
          df_agent[df_agent$id%in%current_vacc_2, "vacc_susc"] <- vacc_vec_2
          df_agent[df_agent$id%in%current_vacc_2, "susc"] <- df_agent[df_agent$id%in%current_vacc_2, "susc"]*vacc_vec_2
        }
      }
      
      eff_vacc_cov[t_i] <- sum(df_agent$vacc_susc<1)
      
      
      # ------------------------------------------------------------------------ #
      # Reinfections
      # ------------------------------------------------------------------------ #
      ### Students
      reinfected_students <- reinfection(t_i = t_i,
                                         df_agent = df_agent,
                                         df_history= df_history,
                                         df_time_inf = df_time_inf_students,
                                         df_prob_reinfection = df_prob_reinfection_students,
                                         df_prob_reinfection_2 = df_prob_reinfection_students_2,
                                         lag_reinfection = lag_reinfection,
                                         init_prev_inf = F, 
                                         rel_susc_waning = rel_susc_waning)
      df_agent <- reinfected_students$df_agent
      df_history <- reinfected_students$df_history
      reinfected_student_ids <- reinfected_students$reinfected_ids
      reinfected_students[[t_i]] <- student_reinfections$reinfected_ids
      reinfected_students_rec[[t_i]] <- student_reinfections$reinfected_rec_ids
      reinfected_students_vacc[[t_i]] <- student_reinfections$reinfected_vacc_ids
      df_reinfected_students[t_i, "recovered"] <- length(student_reinfections$reinfected_rec_ids)
      df_reinfected_students[t_i, "vaccinated"] <- length(student_reinfections$reinfected_vacc_ids)
      df_reinfected_students[t_i, "total"] <- length(student_reinfections$reinfected_ids)
      
      if(is.null(df_agent)) print(paste0("After reinfection of students at time ", t_i, ": df_agent is empty."))
      
      # Update susc_close, susc_class using cont_close, cont_class
      susc_student_ids <- df_history[abs(df_history$time-t_i)<=tol & df_history$state=="S" & df_history$pres==1, "id"]
      # Check whether reinfected students are susceptible
      if(!all(reinfected_student_ids%in%susc_student_ids)){
        print(paste0("Not all reinfected students are susceptible at time ", t_i))
        missing_ids <- reinfected_student_ids[!reinfected_student_ids%in%susc_student_ids]
        print(df_agent[df_agent$id%in%missing_ids, ])
        print("History:")
        print(df_history[df_history$id%in%missing_ids & abs(df_history$time-t_i)<=tol, ])
      }
      susc_close <- lapply(cont_close, function(x) intersect(unlist(x), susc_student_ids))
      susc_class <- lapply(cont_class, function(x) intersect(unlist(x), susc_student_ids))
      susc_grade <- lapply(cont_grade, function(x) intersect(unlist(x), susc_student_ids))
      susc_out_school <- lapply(cont_out_school, function(x) intersect(unlist(x), susc_student_ids))
      susc_teacher <- lapply(cont_teacher, function(x) intersect(unlist(x), susc_student_ids))
      susc_ts <- lapply(cont_ts, function(x) intersect(unlist(x), susc_student_ids))
      
      ### Teachers
      reinfected_teachers <- reinfection(t_i = t_i,
                                         df_agent = df_teacher,
                                         df_history = df_teach_hist,
                                         df_time_inf = df_time_inf_teachers,
                                         df_prob_reinfection = df_prob_reinfection_teachers,
                                         df_prob_reinfection_2 = df_prob_reinfection_teachers_2,
                                         lag_reinfection = lag_reinfection,
                                         init_prev_inf = F, 
                                         rel_susc_waning = rel_susc_waning)
      df_teacher <- reinfected_teachers$df_agent
      df_teach_hist <- reinfected_teachers$df_history
      reinfected_teacher_ids <- reinfected_teachers$reinfected_ids
      reinfected_teachers[[t_i]] <- teacher_reinfections$reinfected_ids
      reinfected_teachers_rec[[t_i]] <- teacher_reinfections$reinfected_rec_ids
      reinfected_teachers_vacc[[t_i]] <- teacher_reinfections$reinfected_vacc_ids
      df_reinfected_teachers[t_i, "recovered"] <- length(teacher_reinfections$reinfected_rec_ids)
      df_reinfected_teachers[t_i, "vaccinated"] <- length(teacher_reinfections$reinfected_vacc_ids)
      df_reinfected_teachers[t_i, "total"] <- length(teacher_reinfections$reinfected_ids)
      
      if(is.null(df_teacher)) print(paste0("After reinfection of teachers at time ", t_i, ": df_teacher is empty."))
      
      susc_teacher_ids <- df_teach_hist[abs(df_teach_hist$time-t_i)<=tol & df_teach_hist$state=="S", "id"]
      if(!all(reinfected_teacher_ids%in%susc_teacher_ids)){
        print(paste0("Not all reinfected teachers are susceptible at time ", t_i))
        missing_ids <- reinfected_teacher_ids[!reinfected_teacher_ids%in%susc_teacher_ids]
        print(df_teacher[df_teacher$id%in%missing_ids, ])
        print("History:")
        print(df_teach_hist[df_teach_hist$id%in%missing_ids& abs(df_teach_hist$time-t_i)<=tol, ])
      }
      susc_tt <- lapply(cont_tt, function(x) intersect(unlist(x), susc_teacher_ids))
    } # Bracket t_i%%1==0
    
    # ------------------------------------------------------------------------ #
    # Recovery
    # ------------------------------------------------------------------------ #
    # Students
    rec_students <- recovery(t_i, df_history, df_agent, rec_time)
    df_history <- rec_students$df_history
    df_agent <- rec_students$df_agent
    rec_stud_ids <- rec_students$recovered_ids
    # Teachers
    rec_teachers <- recovery(t_i, df_teach_hist, df_teacher, rec_time)
    df_teach_hist <- rec_teachers$df_history
    df_teacher <- rec_teachers$df_agent
    rec_teacher_ids <- rec_teachers$recovered_ids
    

    # ------------------------------------------------------------------------ #
    # Quarantine of close contacts
    # ------------------------------------------------------------------------ #
    if(quarantine_flag & !(t_i%in%vacation)){
      # Students
      quarantined_students <- quarantine.close.contacts(t_i, 
                                                        df_history, 
                                                        df_agent, 
                                                        df_teach_hist, 
                                                        df_teacher, 
                                                        pos_tested_ids, 
                                                        cont_close, cont_class, cont_grade, cont_teacher,
                                                        susc_close, susc_class, susc_grade, susc_out_school, 
                                                        susc_teacher, susc_tt, susc_ts,
                                                        tol=tol)
      df_history <- quarantined_students$df_history
      df_agent <- quarantined_students$df_agent
      susc_close <- quarantined_students$susc_close
      susc_class <- quarantined_students$susc_class
      susc_grade <- quarantined_students$susc_grade
      susc_out_school <- quarantined_students$susc_out_school
      susc_ts <- quarantined_students$susc_ts
    }
    
    if(quarantine_class_flag & !(t_i%in%vacation)){
      # Students
      quarantined_students <- quarantine.class(t_i, 
                                               df_history, 
                                               df_agent, 
                                               df_teach_hist, 
                                               df_teacher, 
                                               pos_tested_ids, 
                                               cont_close, cont_class, cont_grade, cont_teacher,
                                               susc_close, susc_class, susc_grade, susc_out_school, 
                                               susc_teacher, susc_tt, susc_ts,
                                               quarantine_fully_vacc,
                                               tol=tol)
      df_history <- quarantined_students$df_history
      df_agent <- quarantined_students$df_agent
      susc_close <- quarantined_students$susc_close
      susc_class <- quarantined_students$susc_class
      susc_grade <- quarantined_students$susc_grade
      susc_out_school <- quarantined_students$susc_out_school
      susc_ts <- quarantined_students$susc_ts
    }
    
    # ----------------------------------------------------------------------- #
    # Pre-emptive testing
    # ----------------------------------------------------------------------- #
    if(screening_flag & !(t_i%in%vacation)){
      # Those who are fully vaccinated stop adherence to screening
      df_agent[df_agent$vaccinated_2==1 & t_i + tol >= df_agent$t_vacc_2 + vacc_eff_lag[2] & df_agent$adherence==1, "adherence"] <- 0
      if(t_i%%1==0 & time_names[abs(time_steps-t_i)<=tol]%in%names(test_days[test_days==1])){
        ### Screening of students
        screened_students <- screening(t_i, 
                                       df_history, 
                                       df_agent, 
                                       susc_close, susc_class, susc_grade, 
                                       susc_out_school, susc_teacher, 
                                       susc_tt, susc_ts,
                                       teacher=F, tol=tol)
        df_history <- screened_students$df_history
        df_agent <- screened_students$df_agent
        susc_close <- screened_students$susc_close
        susc_class <- screened_students$susc_class
        susc_grade <- screened_students$susc_grade
        susc_out_school <- screened_students$susc_out_school
        susc_ts <- screened_students$susc_ts
        pos_students <- screened_students$pos_tested
        tested_student_ids <- screened_students$ids_tested
        # screening_list[[t_i]] <- c(screening_list[[t_i]], pos_students)
        df_screening[abs(df_screening$time-t_i)<=tol, "pos_students"] <- length(pos_students)
        df_screening[abs(df_screening$time-t_i)<=tol, "tested_students"] <- length(tested_student_ids)
        ### Screening of teachers
        screened_teachers <- screening(t_i, 
                                       df_teach_hist, 
                                       df_teacher, 
                                       susc_close, susc_class, susc_grade, 
                                       susc_out_school, susc_teacher, 
                                       susc_tt, susc_ts,
                                       teacher=T, tol=tol)
        df_teach_hist <- screened_teachers$df_history
        df_teacher <- screened_teachers$df_agent
        susc_teacher <- screened_teachers$susc_teacher
        susc_tt <- screened_teachers$susc_tt
        pos_teachers <- screened_teachers$pos_tested
        tested_teacher_ids <- screened_teachers$ids_tested
        # screening_list[[t_i]] <- c(screening_list[[t_i]], pos_teachers)
        df_screening[abs(df_screening$time-t_i)<=tol, "pos_teachers"] <- length(pos_teachers)
        df_screening[abs(df_screening$time-t_i)<=tol, "tested_teachers"] <- length(tested_teacher_ids)
        df_screening[abs(df_screening$time-t_i)<=tol, "pos_tested"] <- length(pos_students) + length(pos_teachers)
        if(is.null(df_teach_hist)) print("After screening: df_teach_hist is empty.")
      } # Bracket for screening_flag
    } # Bracket for screening_flag
    
    # Events that take place only on school week days
    if(!time_names[abs(time_steps-t_i)<=tol]%in%c("Saturday", "Sunday") & !(t_i%in%vacation)){
      # If there were symptomatic individuals with positive test: risk-based testing
      if(length(pos_tested_ids)>0){
        # -------------------------------------------------------------------- #
        # Risk-based testing
        # -------------------------------------------------------------------- #
        if(risk_based_flag & contacts_in_school[abs(time_steps-t_i)<=tol]==1){
          risk_tested <- risk_based_testing(t_i, 
                                            df_history, 
                                            df_agent, 
                                            df_teach_hist, 
                                            df_teacher, 
                                            df_teacher_gcs,
                                            pos_tested_ids, 
                                            pos_tested_teacher_ids,
                                            cont_close, cont_class, cont_grade, cont_teacher, cont_tt, cont_ts,
                                            susc_close, susc_class, susc_grade, susc_out_school, 
                                            susc_teacher, susc_tt, susc_ts)
          df_history <- risk_tested$df_history
          df_agent <- risk_tested$df_agent
          df_teach_hist <- risk_tested$df_teach_hist
          df_teacher <- risk_tested$df_teacher
          susc_close <- risk_tested$susc_close
          susc_class <- risk_tested$susc_class
          susc_grade <- risk_tested$susc_grade
          susc_out_school <- risk_tested$susc_out_school
          susc_teacher <- risk_tested$susc_teacher
          susc_tt <- risk_tested$susc_tt
          susc_ts <- risk_tested$susc_ts
          ind_time <- abs(df_risk_testing$time-t_i)<=tol
          df_risk_testing[ind_time, "pos_students"] <- length(risk_tested$det_students)
          df_risk_testing[ind_time, "pos_teachers"] <- length(risk_tested$det_teacher)
          df_risk_testing[ind_time, "pos_tested"] <- df_risk_testing[t_i, "pos_students"] + df_risk_testing[t_i, "pos_teachers"] 
          if(is.null(df_teach_hist)) print("After risk-based testing: df_teach_hist is empty.")
        } # Bracket for Risk-based testing
      }
    } # Bracket for Events only during week days
    # -------------------------------------------------------------------- #
    # Transmission events
    # -------------------------------------------------------------------- #
    if(no_contacts[abs(time_steps-t_i)<=tol]==0 & !(t_i%in%vacation)){
      # -------------------------------------------------------------------#
      # Update the list of susceptible contacts
      # -------------------------------------------------------------------#
      # Add everyone who is in "S" to the susceptible contacts
      susc_close <- update.susc.contacts(t_i, df_history, susc_close)
      susc_class <- update.susc.contacts(t_i, df_history, susc_class)
      susc_grade <- update.susc.contacts(t_i, df_history, susc_grade)
      susc_out_school <- update.susc.contacts(t_i, df_history, susc_out_school)
      susc_teacher <- update.susc.contacts(t_i, df_teach_hist, susc_teacher)
      susc_tt <- update.susc.contacts(t_i, df_teach_hist, susc_tt)
      susc_ts <- update.susc.contacts(t_i, df_history, susc_ts)
      transm <- transmission.events(t_i, 
                                    time_steps, 
                                    time_names,
                                    df_history, df_agent,
                                    df_teach_hist, df_teacher, df_teacher_gcs,
                                    susc_close, susc_class, susc_grade, 
                                    susc_out_school, susc_teacher, 
                                    susc_tt, susc_ts,
                                    infecteds,
                                    contacts_in_school = contacts_in_school, 
                                    contacts_out_school = contacts_out_school,
                                    vaccination_flag = vaccination_flag, 
                                    vacc_eff_1 = vacc_eff_1, 
                                    vacc_eff_2 = vacc_eff_2, 
                                    vacc_eff_lag = vacc_eff_lag,
                                    scaling_factor = scaling_factor,
                                    scaling_constant = scaling_constant,
                                    sample_prop=sample_prop,
                                    cont_susc_scale = cont_susc_scale,
                                    aerosol_susc_scale = aerosol_susc_scale, 
                                    aerosol = aerosol, 
                                    R_0 = R0_vec[abs(time_steps-t_i)<=tol])
      df_history <- transm$df_history
      df_agent <- transm$df_agent
      df_teach_hist <- transm$df_teach_hist
      df_teacher <- transm$df_teacher
      susc_close <- transm$susc_close
      susc_class <- transm$susc_class
      susc_grade <- transm$susc_grade
      susc_out_school <- transm$susc_out_school
      susc_teacher <- transm$susc_teacher
      susc_tt <- transm$susc_tt
      susc_ts <- transm$susc_ts
      infecteds <- transm$infecteds
    } # Transmission events
  } # Time steps
  
  # -------------------------------------------------------------------- #
  # For each index case (i.e., source) determine the number of 
  # (symptomatic) infections in the same class
  # Distinguish between 1st and 2nd infection
  # -------------------------------------------------------------------- #
  # 1. Determine all the index cases (source)
  # 2. For each index, determine class, and introduce class infection counter
  # 3. For each index case, determine all infecteds
  # 4. For each infecteds, check whether in the same class and update class 
  #    infection counter
  
  ### Students
  max_n_inf_students <- max(df_agent$n_inf, na.rm = T)
  max_n_inc_students <- max(df_agent$n_inc, na.rm = T)
  n_same_class_students <- n_no_index_students <- vector(mode="list", length=max_n_inf_students+1)
  # print(paste0("Students: Maximal times of infection: max_n_inf_students = ", max_n_inf_students))
  # print(paste0("Students: Maximal times of symptom onsets: max_n_inc_students = ", max_n_inc_students))
  
  if(max_n_inf_students>0){
    for(n in 1:max_n_inf_students){
      location <- paste0("location_", n)
      source <- paste0("source_", n)
      if(source%in%colnames(df_agent)){
        # Introductions from community 
        intro_ids <- unique(df_agent[df_agent[,eval(location)]==2,"id"])
        index_ids <- unique(df_agent[,eval(source)])
        index_ids <- index_ids[!is.na(index_ids)]
        n_no_index_students[[n]] <- sum(!intro_ids%in%index_ids) # How many introductions did not lead to outbreaks (i.e., how many introductions are not index cases)
        n_no_index_students[[max_n_inf_students+1]] <- c(n_no_index_students[[max_n_inf_students+1]], n_no_index_students[[n]])
        for(id in index_ids){
          class_index <- df_agent[df_agent$id==id, "class"]
          inf_ids <- df_agent[df_agent[,eval(source)]==id, "id"]
          class_inf <- df_agent[df_agent$id%in%inf_ids, "class"]
          n_same_class_students[[n]] <- c(n_same_class_students[[n]], sum(class_inf==class_index))
        }
        n_same_class_students[[max_n_inf_students+1]] <- c(n_same_class_students[[max_n_inf_students+1]], n_same_class_students[[n]])
      }else{
        print(paste0(source, " is not in df_agent."))
        print(head(df_agent))
      }
    } 
  }

  
  ### Teachers
  max_n_inf_teachers <- max(df_teacher$n_inf, na.rm = T)
  max_n_inc_teachers <- max(df_teacher$n_inc, na.rm = T)
  n_same_class_teachers <- n_no_index_teachers <- vector(mode="list", length=max_n_inf_teachers+1)
  # print(paste0("Teachers: Maximal times of infection: max_n_inf_teachers = ", max_n_inf_teachers))
  # print(paste0("Teachers: Maximal times of symptom onsets: max_n_inc_teachers = ", max_n_inc_teachers))
  
  if(max_n_inf_teachers>0){
    for(n in 1:max_n_inf_teachers){
      location <- paste0("location_", n)
      source <- paste0("source_", n)
      if(source%in%colnames(df_teacher)){
        # Introductions from community 
        intro_ids <- unique(df_teacher[df_teacher[,eval(location)]==2,"id"])
        index_ids <- unique(df_teacher[,eval(source)])
        index_ids <- index_ids[!is.na(index_ids)]
        n_no_index_teachers[[n]] <- sum(!intro_ids%in%index_ids) # How many introductions did not lead to outbreaks (i.e., how many introductions are not index cases)
        n_no_index_teachers[[max_n_inf_teachers+1]] <- c(n_no_index_teachers[[max_n_inf_teachers+1]], n_no_index_teachers[[n]])
        for(id in index_ids){
          class_index <- df_teacher[df_teacher$id==id, "class"]
          inf_ids <- df_teacher[df_teacher[,eval(source)]==id, "id"]
          class_inf <- df_teacher[df_teacher$id%in%inf_ids, "class"]
          n_same_class_teachers[[n]] <- c(n_same_class_teachers[[n]], sum(class_inf==class_index))
        }
        n_same_class_teachers[[max_n_inf_teachers+1]] <- c(n_same_class_teachers[[max_n_inf_teachers+1]], n_same_class_teachers[[n]])
      }else{
        print(paste0(source, " is not in df_teacher."))
        print(head(df_teacher))
      }
    }
  }

  # -------------------------------------------------------------------- #
  # Save infection and symptom onset times
  # -------------------------------------------------------------------- #
  ### Students
  t_inf_student_list <- vector(mode="list", length=max_n_inf_students)
  t_inc_student_list <- vector(mode="list", length=max_n_inc_students)
  
  # Infection times
  if(max_n_inf_students>0){
    for(n in 1:max_n_inf_students){
      t_inf_student_list[[n]] <- vector(mode="list", length=3)
      names(t_inf_student_list[[n]]) <- c("Outside school-related", "Within-school", "External")
      location <- paste0("location_", n)
      t_inf <- paste0("t_inf_", n)
      if(t_inf%in%colnames(df_agent)){
        ind_t_inf <- which(df_agent[, eval(t_inf)]>=0)
        ind_0 <-  which(df_agent[, eval(location)]==0)
        ind_1 <- which(df_agent[, eval(location)]==1)
        ind_2 <- which(df_agent[, eval(location)]==2)
        ind_outside <- intersect(ind_t_inf, ind_0)
        ind_within <- intersect(ind_t_inf, ind_1)
        ind_external <- intersect(ind_t_inf, ind_2)
        t_inf_student_list[[n]][["Outside school-related"]] <- df_agent[ind_outside, eval(t_inf)]
        t_inf_student_list[[n]][["Within-school"]] <- df_agent[ind_within, eval(t_inf)]
        t_inf_student_list[[n]][["External"]] <- df_agent[ind_external, eval(t_inf)]
      }else{
        print(paste0(t_inf, " is not in colnames of df_agent."))
        print(head(df_agent))
      }
    }
  }

  
  # Incubation times
  if(max_n_inc_students>0){
    for(n in 1:max_n_inc_students){
      t_inc_student_list[[n]] <- vector(mode="list", length=3)
      names(t_inc_student_list[[n]]) <- c("Outside school-related", "Within-school", "External")
      t_inc <- paste0("t_inc_", n)
      location <- paste0("location_", n)
      if(t_inc%in%colnames(df_agent)){
        ind_t_inf <- which(df_agent[, eval(t_inf)]>=0)
        ind_t_inc <- which(df_agent[, eval(t_inc)]>=0)
        ind_0 <-  which(df_agent[, eval(location)]==0)
        ind_1 <- which(df_agent[, eval(location)]==1)
        ind_2 <- which(df_agent[, eval(location)]==2)
        ind_outside <- intersect(ind_t_inc, ind_0)
        ind_within <- intersect(ind_t_inc, ind_1)
        ind_external <- intersect(ind_t_inc, ind_2)
        t_inc_student_list[[n]][["Outside school-related"]] <- df_agent[ind_outside, eval(t_inc)] + df_agent[ind_outside, eval(t_inf)]
        t_inc_student_list[[n]][["Within-school"]] <- df_agent[ind_within, eval(t_inc)] + df_agent[ind_within, eval(t_inf)]
        t_inc_student_list[[n]][["External"]] <- df_agent[ind_external, eval(t_inc)] + df_agent[ind_external, eval(t_inf)]
      }else{
        print(paste0(t_inc, " is not in colnames of df_agent."))
        print(head(df_agent))
      }
    }
  }

  
  ### Teachers
  t_inf_teacher_list <- vector(mode="list", length=max_n_inf_teachers)
  t_inc_teacher_list <- vector(mode="list", length=max_n_inc_teachers)
  
  # Infection times
  if(max_n_inf_teachers>0){
    for(n in 1:max_n_inf_teachers){
      if(n>length(t_inf_teacher_list)){
        print("n > length(t_inf_teacher_list).")
        print(t_inf_teacher_list)
      }
      t_inf_teacher_list[[n]] <- vector(mode="list", length=3)
      names(t_inf_teacher_list[[n]]) <- c("Outside school-related", "Within-school", "External")
      location <- paste0("location_", n)
      t_inf <- paste0("t_inf_", n)
      if(t_inf%in%colnames(df_teacher)){
        ind_t_inf <- which(df_teacher[, eval(t_inf)]>=0)
        ind_0 <-  which(df_teacher[, eval(location)]==0)
        ind_1 <- which(df_teacher[, eval(location)]==1)
        ind_2 <- which(df_teacher[, eval(location)]==2)
        ind_outside <- intersect(ind_t_inf, ind_0)
        ind_within <- intersect(ind_t_inf, ind_1)
        ind_external <- intersect(ind_t_inf, ind_2)
        t_inf_teacher_list[[n]][["Outside school-related"]] <- df_teacher[ind_outside, eval(t_inf)]
        t_inf_teacher_list[[n]][["Within-school"]] <- df_teacher[ind_within, eval(t_inf)]
        t_inf_teacher_list[[n]][["External"]] <- df_teacher[ind_external, eval(t_inf)]
      }else{
        print(paste0(t_inf, " is not in colnames of df_teacher."))
        print(head(df_teacher))
      }
    }
  }

  
  # Incubation times
  if(max_n_inc_teachers>0){
    for(n in 1:max_n_inc_teachers){
      if(n>length(t_inc_teacher_list)){
        print("n > length(t_inc_teacher_list).")
        print(t_inc_teacher_list)
      }
      t_inc_teacher_list[[n]] <- vector(mode="list", length=3)
      names(t_inc_teacher_list[[n]]) <- c("Outside school-related", "Within-school", "External")
      t_inc <- paste0("t_inc_", n)
      location <- paste0("location_", n)
      if(t_inc%in%colnames(df_teacher)){
        ind_t_inf <- which(df_teacher[, eval(t_inf)]>=0)
        ind_t_inc <- which(df_teacher[, eval(t_inc)]>=0)
        ind_0 <-  which(df_teacher[, eval(location)]==0)
        ind_1 <- which(df_teacher[, eval(location)]==1)
        ind_2 <- which(df_teacher[, eval(location)]==2)
        ind_outside <- intersect(ind_t_inc, ind_0)
        ind_within <- intersect(ind_t_inc, ind_1)
        ind_external <- intersect(ind_t_inc, ind_2)
        t_inc_teacher_list[[n]][["Outside school-related"]] <- df_teacher[ind_outside, eval(t_inc)] + df_teacher[ind_outside, eval(t_inf)]
        t_inc_teacher_list[[n]][["Within-school"]] <- df_teacher[ind_within, eval(t_inc)] + df_teacher[ind_within, eval(t_inf)]
        t_inc_teacher_list[[n]][["External"]] <- df_teacher[ind_external, eval(t_inc)] + df_teacher[ind_external, eval(t_inf)]
      }else{
        print(paste0(t_inc, " is not in colnames of df_teacher."))
        print(head(df_teacher))
      }
    }
  }

  
  # -------------------------------------------------------------------- #
  # Save number of performed self-tests (screening)
  # -------------------------------------------------------------------- #
  screening_stat[["students"]] <- sum(df_screening$tested_students, na.rm=T)
  screening_stat[["teachers"]] <- sum(df_screening$tested_teachers, na.rm=T)
  return(list(df_history = df_history, 
              df_agent = df_agent,
              df_teach_hist = df_teach_hist,
              df_teacher = df_teacher,
              df_teacher_gcs = df_teacher_gcs,
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
              df_screening = df_screening, 
              screening_list = screening_list,
              screening_stat = screening_stat,
              df_risk_testing = df_risk_testing,
              infecteds = infecteds,
              vacc_cov1_vec = vacc_cov1_vec, 
              vacc_cov2_vec = vacc_cov2_vec,
              t_inf_student_list = t_inf_student_list, 
              t_inc_student_list = t_inc_student_list,
              t_inf_teacher_list = t_inf_teacher_list, 
              t_inc_teacher_list = t_inc_teacher_list,
              intro_per_day = intro_per_day, 
              external_inf_student_ids = external_inf_student_ids,
              external_inf_teacher_ids = external_inf_teacher_ids,
              eff_vacc_cov = eff_vacc_cov, 
              n_same_class_students = n_same_class_students, 
              n_same_class_teachers = n_same_class_teachers, 
              n_no_index_students = n_no_index_students,
              n_no_index_teachers = n_no_index_teachers,
              max_n_inf_students = max_n_inf_students,
              max_n_inf_teachers = max_n_inf_teachers,
              max_n_inc_students = max_n_inc_students,
              max_n_inc_teachers = max_n_inc_teachers, 
              reinfected_students = reinfected_students, 
              reinfected_students_rec = reinfected_students_rec,
              reinfected_students_vacc = reinfected_students_vacc, 
              df_reinfected_students = df_reinfected_students, 
              reinfected_teachers = reinfected_teachers, 
              reinfected_teachers_rec = reinfected_teachers_rec,
              reinfected_teachers_vacc = reinfected_teachers_vacc, 
              df_reinfected_teachers = df_reinfected_teachers 
              ))
}

