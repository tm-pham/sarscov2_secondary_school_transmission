# ============================================================================ # 
# Pre-emptive screening
# ---------------------------------------------------------------------------- #
# 1. Determine which individuals are eligible for testing, i.e., those
#    who are not isolated (since isolated individuals are assumed to have 
#    already been tested). 
# 2. To account for false positives, divide the individuals into those who are
#    not infected (and among those there will be false positives) and those who
#    are actually infected. 
# 3. Perform testing using the testing function. 
# 4. Account for compliance: Only those who are compliant to isolation (fixed at
#    each iteration for each individual) will be isolated. 
# 5. Isolate those who have been tested positive. 
# 6. Update the susceptible contact lists.
screening <- function(t_i, 
                      df_history, df_agent, 
                      susc_close=NULL, susc_class=NULL, susc_grade=NULL,
                      susc_out_school=NULL, susc_teacher=NULL, 
                      susc_tt=NULL, susc_ts=NULL, 
                      teacher=T,
                      tol=1e-5){
  curr_noninf <- curr_inf <- pos_tested <- NULL
  # Regular pre-emptive testing
  curr_df_history <- df_history %>% filter(abs(time-t_i)<=tol)
  curr_ids <- unique(unlist(curr_df_history$id))
  # Determine ids that are eligible for testing
  # Exclude those that were isolated in the past and were therefore infected
  ids_to_be_tested <- df_agent[df_agent$id%in%curr_ids & df_agent$iso==0, "id"]
  curr_to_be_tested <- curr_df_history[curr_df_history$id%in%ids_to_be_tested, ]
  # Ids that actually perform testing (account for adherence)
  ids_tested <- intersect(ids_to_be_tested, df_agent[df_agent$adherence==1, "id"])
  curr_noninf <- curr_to_be_tested %>% filter(state%in%c("S","R"))
  curr_inf <- curr_to_be_tested %>% filter(state%in%c("PS","IA"))
  # Account for adherence to screening
  curr_noninf <- df_agent[df_agent$id%in%curr_noninf$id & df_agent$adherence==1, ]
  curr_inf <- df_agent[df_agent$id%in%curr_inf$id & df_agent$adherence==1, ]
  # Perform testing
  tested <- testing(t_i, df_history, df_agent, curr_noninf, curr_inf)
  # Change status for positive tested (both false- and true positives)
  ind <- c(tested$FP_ind, tested$pos_ind)
  # The following code is not used. We assume that everyone who participates 
  # in screening and is tested positive also adheres to isolation. 
  # # Account for compliance to isolation if positive tested
  # compliant_ids <- unlist(df_agent %>% filter(iso_compliance==1) %>% summarize(id))
  # compliant_ind <- which(df_history$id%in%compliant_ids)
  # ind <- intersect(ind, compliant_ind) # for df_history
  pos_tested <- c(pos_tested, tested$pos_tested_ids)
  if(length(ind)>0){
    df_history[ind,"iso_state"] <- "IH"
    ids <- unique(df_history[ind,"id"])
    df_agent[df_agent$id%in%ids, "iso"] <- rep(1, length(ids))
    # Remove isolated individuals from eligible susceptible contacts
    if(!teacher){ # Screening of students
      susc_close <- update.susc.contacts(t_i, df_history, susc_close)
      susc_class <- update.susc.contacts(t_i, df_history, susc_class)
      susc_grade <- update.susc.contacts(t_i, df_history, susc_grade)
      susc_out_school <- update.susc.contacts(t_i, df_history, susc_out_school)
      susc_ts <- update.susc.contacts(t_i, df_history, susc_ts)
    }else{ # Screening of teachers
      susc_teacher <- update.susc.contacts(t_i, df_history, susc_teacher)
      susc_tt <- update.susc.contacts(t_i, df_history, susc_tt)
    }
  }
  return(list(df_history = df_history, 
              df_agent = df_agent, 
              susc_close = susc_close, 
              susc_class = susc_class, 
              susc_grade = susc_grade, 
              susc_out_school = susc_out_school, 
              susc_teacher = susc_teacher,
              susc_tt = susc_tt, 
              susc_ts = susc_ts,
              curr_noninf = curr_noninf, 
              curr_inf = curr_inf, 
              pos_tested = pos_tested, 
              ids_tested = ids_tested))
}

