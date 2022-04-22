# ============================================================================ #
# Risk-based testing
# ---------------------------------------------------------------------------- #
# For all positive tested individuals, class mates, other contacts in school 
# and teachers are tested with an antigen test and isolated if test result is
# positive. 
# 1. Identify all class mates and contacts in school (= cont_grade) who are in
#    the same group (important for half occupancy). 
# 2. Divide the eligible individuals into those who are non infected (important
#    to account for false positive) and those who are actually infected
#    (pre-symptomatically and asymptomatically infected). 
# 3. Perform the testing using the testing function. 
# 4. Account for compliance to isolation: Only those tho are compliant will be
#    isolated. 
# 5. Update the susceptible contact lists (i.e., remove those who are isolated
#    from the list of susceptible contacts). 
# 6. Repeat 1-5 for teachers of the positive tested index case. 
# 7. If teacher is positive tested index case then perform 1-5 for all students
#    educated by the teacher (i.e., the respective classes). 
# ============================================================================ #
risk_based_testing <- function(t_i, 
                               df_history, 
                               df_agent, 
                               df_teach_hist, 
                               df_teacher, 
                               df_teacher_gcs,
                               pos_tested_ids, 
                               pos_tested_teacher_ids=NULL,
                               cont_close, cont_class, cont_grade, cont_teacher, cont_tt, cont_ts,
                               susc_close, susc_class, susc_grade, susc_out_school, 
                               susc_teacher, susc_tt, susc_ts,
                               tol=1e-5){
  for(i in pos_tested_ids){
    ind <- which(as.numeric(names(cont_close))==i)
    # ------------------------------------------------------------------------ #
    # Testing of class mates and teachers of index case
    # ------------------------------------------------------------------------ #
    id_row <- which(df_agent$id==i)
    c <- df_agent[id_row, "class"]
    g <- df_agent[id_row, "grade"]
    gr <- df_agent[id_row, "group"]
    # class_ids <- unlist(df_agent %>% filter(class==c, grade==g, group==gr) %>% summarize(id)) # slower
    class_ids <- df_agent[df_agent$class==c & df_agent$grade==g & df_agent$group==gr, "id"]
    # All contact ids of index case i that need to be tested
    teacher_id <- unlist(cont_teacher[[ind]])
    cont_ids <- c(unlist(cont_class[[ind]]), unlist(cont_grade[[ind]]))
    cont_ids <- unique(intersect(class_ids, cont_ids))
    curr_df_history <- df_history %>% filter(id%in%cont_ids & !iso_state%in%c("Q","IH") & pres==1, abs(time-t_i)<=tol)
    curr_ids <- unique(unlist(curr_df_history$id))
    # Students 
    ids_to_be_tested <- df_agent[df_agent$id%in%curr_ids, "id"]
    curr_to_be_tested <- curr_df_history[curr_df_history$id%in%ids_to_be_tested & abs(curr_df_history$time-t_i)<=tol, ]
    curr_noninf <- curr_to_be_tested[curr_to_be_tested$state%in%c("S","R"), ]
    curr_inf <- curr_to_be_tested[curr_to_be_tested$state%in%c("PS","IA"), ]
    # Account for adherence
    curr_noninf <- df_agent[df_agent$id%in%curr_noninf$id & df_agent$adherence==1, ]
    curr_inf <- df_agent[df_agent$id%in%curr_inf$id & df_agent$adherence==1, ]
    # Testing
    tested <- testing(t_i, df_history, df_agent, curr_noninf, curr_inf)
    ind_students <- c(tested$FP_ind, tested$pos_ind)
    det_students <- tested$pos_tested_ids
    if(length(ind_students)>0){
      df_history[ind_students,"iso_state"] <- "IH"
      ids <- unique(df_history[ind_students,"id"])
      df_agent[df_agent$id%in%ids, "iso_time"] <- rep(t_i, length(ids))
      df_agent[df_agent$id%in%ids, "iso"] <- rep(1, length(ids))
      # Remove isolated individuals from eligible susceptible contacts
      susc_close <- update.susc.contacts(t_i, df_history, susc_close)
      susc_class <- update.susc.contacts(t_i, df_history, susc_class)
      susc_grade <- update.susc.contacts(t_i, df_history, susc_grade)
      susc_out_school <- update.susc.contacts(t_i, df_history, susc_out_school)
      susc_ts <-  update.susc.contacts(t_i, df_history, susc_ts)
    }
    # Teachers
    teacher_to_be_tested <- (df_teacher %>% filter(id%in%teacher_id & state!="IS"))[,"id"]
    curr_df_teach_hist <- df_teach_hist %>% filter(id%in%teacher_to_be_tested, abs(time-t_i)<=tol)
    curr_t_to_be_tested <- curr_df_teach_hist %>% filter(id%in%teacher_to_be_tested)
    curr_t_noninf <- curr_t_to_be_tested %>% filter(state=="S" | state == "R")
    curr_t_inf <- curr_t_to_be_tested %>% filter(state=="PS" | state == "IA")
    # Account for adherence
    curr_t_noninf <- df_teacher[df_teacher$id%in%curr_noninf$id & df_teacher$adherence==1, ]
    curr_t_inf <- df_teacher[df_teacher$id%in%curr_inf$id & df_teacher$adherence==1, ]
    
    tested_teacher <- testing(t_i, df_teach_hist, df_teacher, curr_t_noninf, curr_t_inf)
    ind_teacher <- c(tested_teacher$FP_ind, tested_teacher$pos_ind)
    det_teachers <- tested_teacher$pos_tested_ids
    if(length(ind_teacher)>0){
      df_teach_hist[ind_teacher , "iso_state"] <- "IH"
      ids_teacher <- unique(df_teach_hist[ind_teacher,"id"])
      df_teacher[df_teacher$id%in%ids_teacher, "iso_time"] <- rep(t_i, length(ids_teacher))
      df_teacher[df_teacher$id%in%ids_teacher, "iso"] <- rep(1, length(ids_teacher))
      # Update susceptible contact list
      susc_teacher <- update.susc.contacts(t_i, df_teach_hist, susc_teacher)
      susc_tt <- update.susc.contacts(t_i, df_teach_hist, susc_tt)
    }
  }
  for(i in pos_tested_teacher_ids){
    ind <- which(names(cont_tt)==i)
    # ------------------------------------------------------------------------ #
    # Testing of class mates and teachers of index case
    # ------------------------------------------------------------------------ #
    id_row <- which(df_teacher_gcs$id==i)
    c <- df_teacher_gcs[id_row, "class"]
    g <- df_teacher_gcs[id_row, "grade"]
    gr <- df_teacher_gcs[id_row, "group"]
    # class_ids <- unlist(df_agent %>% filter(class==c, grade==g, group==gr) %>% summarize(id)) # slower
    class_ids <- df_agent[df_agent$class%in%c & df_agent$grade%in%g & df_agent$group%in%gr, "id"]
    # All contact ids of index case i that need to be tested
    teacher_id <- unlist(cont_tt[[ind]])
    cont_ids <- c(unlist(cont_ts[[ind]]), class_ids)
    curr_df_history <- df_history %>% filter(id%in%cont_ids & !iso_state%in%c("Q","IH") & pres==1, abs(time-t_i)<=tol)
    curr_ids <- unique(unlist(curr_df_history$id))
    # Students 
    ids_to_be_tested <- df_agent[df_agent$id%in%curr_ids, "id"]
    curr_to_be_tested <- curr_df_history[curr_df_history$id%in%ids_to_be_tested & abs(curr_df_history$time-t_i)<=tol, ]
    curr_noninf <- curr_to_be_tested[curr_to_be_tested$state%in%c("S","R"), ]
    curr_inf <- curr_to_be_tested[curr_to_be_tested$state%in%c("PS","IA"), ]
    # Account for adherence
    curr_noninf <- df_agent[df_agent$id%in%curr_noninf$id & df_agent$adherence==1, ]
    curr_inf <- df_agent[df_agent$id%in%curr_inf$id & df_agent$adherence==1, ]
    
    tested <- testing(t_i, df_history, df_agent, curr_noninf, curr_inf)
    ind_students <- c(tested$FP_ind, tested$pos_ind)
    compliant_ids <- unlist(df_agent %>% filter(iso_compliance==1) %>% summarize(id))
    compliant_ind <- which(df_history$id%in%compliant_ids)
    ind_students <- intersect(ind_students, compliant_ind)
    det_students <- c(det_students, tested$pos_tested_ids)
    if(length(ind_students)>0){
      df_history[ind_students,"iso_state"] <- "IH"
      ids <- unique(df_history[ind_students,"id"])
      df_agent[df_agent$id%in%ids, "iso_time"] <- rep(t_i, length(ids))
      df_agent[df_agent$id%in%ids, "iso"] <- rep(1, length(ids))
      # Remove isolated individuals from eligible susceptible contacts
      susc_close <- update.susc.contacts(t_i, df_history, susc_close)
      susc_class <- update.susc.contacts(t_i, df_history, susc_class)
      susc_grade <- update.susc.contacts(t_i, df_history, susc_grade)
      susc_out_school <- update.susc.contacts(t_i, df_history, susc_out_school)
      susc_ts <-  update.susc.contacts(t_i, df_history, susc_ts)
    }
    # Teachers
    teacher_to_be_tested <- (df_teacher %>% filter(id%in%teacher_id & state!="IS"))[,"id"]
    curr_df_teach_hist <- df_teach_hist %>% filter(id%in%teacher_to_be_tested, abs(time-t_i)<=tol)
    curr_t_to_be_tested <- curr_df_teach_hist %>% filter(id%in%teacher_to_be_tested)
    curr_t_noninf <- curr_t_to_be_tested %>% filter(state=="S" | state == "R")
    curr_t_inf <- curr_t_to_be_tested %>% filter(state=="PS" | state == "IA")
    # Account for adherence
    curr_t_noninf <- df_teacher[df_teacher$id%in%curr_noninf$id & df_teacher$adherence==1, ]
    curr_t_inf <- df_teacher[df_teacher$id%in%curr_inf$id & df_teacher$adherence==1, ]
    
    tested_teacher <- testing(t_i, df_teach_hist, df_teacher, curr_t_noninf, curr_t_inf)
    ind_teacher <- c(tested_teacher$FP_ind, tested_teacher$pos_ind)
    # Account for compliance to isolation
    compliant_ids <- unlist(df_teacher %>% filter(iso_compliance==1) %>% summarize(id))
    compliant_ind <- which(df_teach_hist$id%in%compliant_ids)
    ind_teacher <- intersect(ind_teacher, compliant_ind)
    det_teachers <- c(det_teachers, tested_teacher$pos_tested_ids)
    if(length(ind_teacher)>0){
      df_teach_hist[ind_teacher , "iso_state"] <- "IH"
      ids_teacher <- unique(df_teach_hist[ind_teacher,"id"])
      df_teacher[df_teacher$id%in%ids_teacher, "iso_time"] <- rep(t_i, length(ids_teacher))
      df_teacher[df_teacher$id%in%ids_teacher, "iso"] <- rep(1, length(ids_teacher))
      # Update susceptible contact list
      susc_teacher <- update.susc.contacts(t_i, df_teach_hist, susc_teacher)
      susc_tt <- update.susc.contacts(t_i, df_teach_hist, susc_tt)
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
              ind_students = ind_students,
              ind_teacher = ind_teacher,
              det_students = det_students,
              det_teachers = det_teachers))
}