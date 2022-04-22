# ============================================================================ #
# Plotting functions
# ============================================================================ #
library("viridis")
library("plyr")
library("gridExtra")
library(stringr)
library(readr)
# ---------------------------------------------------------------------------- #
# Theme for ggplot 
# ---------------------------------------------------------------------------- #
theme_publication <- function(base_size=14, base_family="helvetica") {
  (theme_foundation(base_size=base_size, base_family=base_family)
   + theme_bw() 
   + theme(axis.line = element_line(colour="black"),
           axis.title = element_text(size=24),
           axis.text=element_text(size=22),
           strip.text = element_text(size=22),
           panel.background = element_rect(colour = NA),
           plot.background = element_rect(colour = NA),
           panel.grid.minor=element_line(color="gray94", linetype = 'solid'),
           panel.grid.major=element_line(color="gray94", linetype = 'solid')
   ))
}

`-.gg` <- function(plot, layer) {
  if (missing(layer)) {
    stop("Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?")
  }
  if (!is.ggplot(plot)) {
    stop('Need a plot on the left side')
  }
  plot$layers = c(layer, plot$layers)
  plot
}

# ---------------------------------------------------------------------------- #
# Get legend from a plot
# ---------------------------------------------------------------------------- #
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# ---------------------------------------------------------------------------- #
# Get legend for a set of scenarios
# ---------------------------------------------------------------------------- #
get.legend.scenarios <- function(list, 
                                 scenarios, 
                                 outputPath, 
                                 colors, 
                                 suffix, 
                                 text_size = 30,
                                 width=7.5, height=4){
  max_iter <- max(unlist(lapply(list, length)))
  (moss <- plot.outbreak.size(list, 
                              scenarios=scenarios, 
                              permutation = seq(1:length(scenarios)),
                              iter=max_iter,
                              figuresPath=outputPath, 
                              suffix="students_",
                              occup_suffix="vacc60",
                              title="",
                              title_y="Total number of school-related\ninfected students",
                              colors=colors))
  moss_plot <- moss$plot + theme(axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 legend.title = element_blank(),
                                 legend.text = element_text(size=text_size),
                                 legend.box.background = element_rect(colour = "black", size=2),
                                 legend.position = "right")
  moss_plot <- moss_plot + guides(fill=guide_legend(nrow=3,byrow=F))
  legend <- get_legend(moss_plot)
  ggsave(legend, filename = paste0(outputPath, "legend_scenarios_", suffix, ".pdf"), 
         width=width, height=height)
  return(list(legend=legend))
}


# ---------------------------------------------------------------------------- #
# Create data that counts the number of persons in a specific category per day
# ---------------------------------------------------------------------------- #
n.per.day.per.type <- function(type, n_list, subset=FALSE, days=NULL){
  n_per_day_list <- vector(mode="list", length=length(n_list))
  for(i in 1:length(n_list)){
    if(length(type)>1) temp <- Reduce('+', n_list[[i]][[1]][type])
    else temp <- n_list[[i]][[1]][[type]]
    n_per_day_list[[i]] <-  as.data.frame(cbind(time=1:nrow(n_list[[i]][[1]][[type[1]]]), temp))
    for(j in 1:ncol(n_per_day_list[[i]])){
      n_per_day_list[[i]][which(n_per_day_list[[i]][,j]<0), j] <- 0
    }
  }
  if(subset & !is.null(days)){
    remove <- setdiff(1:nrow(n_per_day_list[[1]]), days)
    n_per_day_list <- lapply(n_per_day_list, function(x) x[-remove,])
  }
  return(n_per_day_list)
}

# ---------------------------------------------------------------------------- #
# Cost benefit analysis
# Calculate the number pf prevented infections (compared to baseline)
# ---------------------------------------------------------------------------- #
prev.inf.per.absent <- function(inf_list, abs_list, ind){
  effect <- vector(mode="list", length=length(ind))
  perc_effect <- vector(mode="list", length=length(ind))
  cost_benefit <- vector(mode="list", length=length(ind))
  for(i in 1:(length(ind))){
    effect[[i]] <- rep(NA, 100)
    perc_effect[[i]] <- rep(NA, 100)
    cost_benefit[[i]] <- rep(NA, 100)
    for(k in 1:10000){
      baseline <- sample(inf_list[[1]], 1)
      intervention <- sample(inf_list[[ind[i]]], 1)
      absent <- sample(abs_list[[ind[i]]], 1) 
      effect[[i]][k] <- baseline-intervention
      cost_benefit[[i]][k] <- effect[[i]][k]/absent
      perc_effect[[i]][k] <- 100*(baseline-intervention)/baseline
    }
  }
  return(list(cost_benefit = cost_benefit, effect=effect, perc_effect = perc_effect))
}

# ---------------------------------------------------------------------------- #
# Cost benefit plot
# Calculate the number pf prevented infections (compared to baseline)
# ---------------------------------------------------------------------------- #
cost.benefit.plot <- function(n_list, type_1 = 2:4, type_2=6:7, ind=12:17,
                              time_periods=list(1:88, 89:242, 243:456, 457:611, 612:840), 
                              names_period = c("Winter 2022", "Summer 2022", "Winter 2022/23", "Summer 2023", "Winter 2023/24"), 
                              scenario_colors){
  df_cost_benefit <- NULL
  for(i in 1:length(time_periods)){
    inf_per_day_list <- n.per.day.per.type(type=type_1, n_list, subset=T, days=time_periods[[i]])
    abs_per_day_list <- n.per.day.per.type(type=type_2, n_list, subset=T, days=time_periods[[i]])
    
    inf_sum_list <- lapply(inf_per_day_list, function(x) unlist(lapply(x[-1], function(y) sum(y))))
    abs_sum_list <- lapply(abs_per_day_list, function(x) unlist(lapply(x[-1], function(y) sum(y))))
    cost_benefit <- prev.inf.per.absent(inf_sum_list, abs_sum_list, ind=ind)
    data_cost_benefit <- as.data.frame(cost_benefit$cost_benefit)
    colnames(data_cost_benefit) <- scenarios[ind]
    df_melted <- reshape2::melt(data_cost_benefit)
    df_melted$time <- names_period[i]
    df_cost_benefit <- rbind(df_cost_benefit, df_melted)
  }
  df_cost_benefit <- df_cost_benefit %>% mutate(time=factor(time, levels=names_period))
  df_summary <- df_cost_benefit %>% dplyr::group_by(variable, time) %>% dplyr::summarize(mean=mean(value), 
                                                                                         median=median(value), 
                                                                                         ci_lower=quantile(value, probs=0.025),
                                                                                         ci_upper=quantile(value, probs=0.975))
  df_summary$time <- factor(df_summary$time, levels=names_period)
  df_summary <- df_summary %>% mutate(time=factor(time, levels=names_period))
  
  plot <- ggplot(df_cost_benefit, aes(x=variable, y=value, group=time, color=variable)) + 
          facet_wrap(time~., ncol=2) + 
          stat_summary(aes(group=1), fun=median, geom="point",group=1,lwd=2.5) + 
          geom_errorbar(data=df_summary, aes(x=variable, y=mean,ymin=ci_lower,ymax=ci_upper),lty=1, lwd=0.9, width=0.05) + 
          scale_color_manual(values=scenario_colors[ind]) + 
          labs(y="Number of prevented infected students per absent student\n(compared to baseline scenario)") + 
          theme_publication() + 
          theme(axis.title.x = element_blank(), 
                axis.text.x = element_blank(), 
                axis.ticks.x = element_blank(),
                legend.title = element_blank(), 
                legend.box.background = element_rect(colour = "black", size=2),
                legend.text = element_text(size=22),
                legend.position = c(0.99,0.1), 
                legend.justification = c(1, 0))
  return(list(plot=plot, data=df_cost_benefit, summary = df_summary))
}



# ---------------------------------------------------------------------------- #
# Combine simulations
# ---------------------------------------------------------------------------- #
combine.iterations <- function(name, path="../results/", no.scenario=1, reduce=F){
  setwd("/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/code/")
  sim_scenario_name = name

  
  files <- list.files(path=paste0(path, sim_scenario_name, "/"), pattern=paste0("*_",no.scenario,"_[0-9]+.RData"))
  max_sim <- length(files)
  print(paste0("Number of files = ", max_sim))
  sim <- vector(mode="list", length=max_sim)
  skip_to_next <- FALSE
  sim_k <- 1
  for(f in files){
    tryCatch({ # This is the "try part
              sim[[sim_k]] <- mget(load(paste0(path, sim_scenario_name, "/", f)))
              }, 
             error=function(cond){
               message(paste("File", f, "could not be loaded."))
               message(cond)
               skip_to_next <<- TRUE
             },
             warning=function(cond){
               message("Warning!")
               message(cond)
               return(NULL)
             }, finally={}
             )
    if(skip_to_next){ next } 
    sim_k <- sim_k + 1
  }
  R_vector <- vector(mode="list", length=no.scenario)
  outbreak_size_students <- vector(mode="list", length=no.scenario)
  outbreak_size_teachers <- vector(mode="list", length=no.scenario)
  outbreak_data_students <- vector(mode="list", length=no.scenario)
  outbreak_data_teachers <- vector(mode="list", length=no.scenario)
  outbreak_per_loc_st <- vector(mode="list", length=no.scenario)
  outbreak_per_loc_teach <- vector(mode="list", length=no.scenario)
  abs_days_students <- vector(mode="list", length=no.scenario)
  absences_students <- vector(mode="list", length=no.scenario)
  sec_cases_students <- vector(mode="list", length=no.scenario)
  abs_days_teachers <- vector(mode="list", length=no.scenario)
  absences_teachers <- vector(mode="list", length=no.scenario)
  sec_cases_teachers <- vector(mode="list", length=no.scenario)
  infector_students <- vector(mode="list", length=no.scenario)
  infector_teachers <- vector(mode="list", length=no.scenario)
  contact_student_teachers <- vector(mode="list", length=no.scenario)
  symptomatic_students <- vector(mode="list", length=no.scenario)
  symptomatic_teachers <- vector(mode="list", length=no.scenario)
  
  n_vacc2doses_students <-  vector(mode="list", length=no.scenario)
  n_vacc2doses_teachers <-  vector(mode="list", length=no.scenario)
  
  ns_list <- vector(mode="list", length=no.scenario)
  nt_list <- vector(mode="list", length=no.scenario)
  
  n_reinfected_students_list <- vector(mode="list", length=no.scenario)
  n_reinfected_teachers_list <- vector(mode="list", length=no.scenario)
  
  len_n_list <- length(sim[[1]]$ns_list[[1]])
  len_n_reinf_list <- ncol(sim[[1]]$df_reinfected_students_list[[1]][[1]])
  for(no in 1:no.scenario){
    ns_list[[no]] = vector(mode="list", length=len_n_list+2)
    nt_list[[no]] = vector(mode="list", length=len_n_list+2)
    n_reinfected_students_list[[no]] = vector(mode="list", length=len_n_reinf_list)
    n_reinfected_teachers_list[[no]] = vector(mode="list", length=len_n_reinf_list)
    for(s in 1:len_n_list){
      ns_list[[no]][[s]] <- sim[[1]]$ns_list[[no]][[s]]
      nt_list[[no]][[s]] <- sim[[1]]$nt_list[[no]][[s]]
    }
    ns_list[[no]][[len_n_list+1]] <- nrow(sim[[1]]$df_agent)-Reduce("+", sim[[1]]$ns_list[[no]][2:5])
    nt_list[[no]][[len_n_list+1]] <- nrow(sim[[1]]$df_teacher)-Reduce("+", sim[[1]]$nt_list[[no]][2:5])
    ns_list[[no]][[len_n_list+2]] <- sapply(1:sim[[1]]$iter, function(x) sim[[1]]$ns_list[[no]][[19]][,x]-(nrow(sim[[1]]$df_agent)-nrow(sim[[1]]$df_agent)*sim[[1]]$vacc_coverage2[[1]][[x]]))
    nt_list[[no]][[len_n_list+2]] <- sapply(1:sim[[1]]$iter, function(x) sim[[1]]$nt_list[[no]][[19]][,x]-(nrow(sim[[1]]$df_teacher)-floor(nrow(sim[[1]]$df_teacher)*sim[[1]]$prop_vaccinated[2])))
    
    
    eff_n_iter <- length(sim[[1]]$df_reinfected_students_list[[no.scenario]])
    for(s in 1:len_n_reinf_list){
      n_reinfected_students_list[[no]][[s]] <- as.data.frame(sim[[1]]$df_reinfected_students_list[[no]][[1]][,s])
      n_reinfected_teachers_list[[no]][[s]] <- as.data.frame(sim[[1]]$df_reinfected_teachers_list[[no]][[1]][,s])
      if(eff_n_iter>1){
        for(iter in 2:eff_n_iter){
          n_reinfected_students_list[[no]][[s]]<- cbind(n_reinfected_students_list[[no]][[s]], sim[[1]]$df_reinfected_students_list[[no]][[iter]][,s]) 
          n_reinfected_teachers_list[[no]][[s]] <- cbind(n_reinfected_teachers_list[[no]][[s]], sim[[1]]$df_reinfected_teachers_list[[no]][[iter]][,s]) 
        }
      }
    }
  }
  
  
  os_weekly <- os_student_weekly <- os_teacher_weekly <- vector(mode="list", length=no.scenario)  
  symp_weekly <- symp_student_weekly <- symp_teacher_weekly <- vector(mode="list", length=no.scenario) 
  intro_weekly <- intro_student_weekly <- intro_teacher_weekly <-vector(mode="list", length=no.scenario)
  n_same_class <- n_same_class_students <- n_same_class_teachers <- vector(mode="list", length=no.scenario)
  
  screening_stats <- vector(mode="list", length=no.scenario)
  det_screening <- vector(mode="list", length=no.scenario)
  
  max_time <- max(df_history$time)
  n_weeks <- max_time/7
  print(paste0("Number of weeks: ", n_weeks))
  print(paste0("Number of months: ", n_weeks/4))
  
  max_os <- max(sapply(1:max_sim, function(x) max(unlist(lapply(sim[[x]]$os_weekly[[1]], length)))))
  max_intro <- max(sapply(1:max_sim, function(x) max(unlist(lapply(sim[[x]]$intro_weekly[[1]], length)))))
  max_symp <- max(sapply(1:max_sim, function(x) max(unlist(lapply(sim[[x]]$symp_weekly[[1]], length)))))
  n_iter <- length(sim[[1]]$os_weekly[[1]])
  os_weekly[[no.scenario]] <- os_student_weekly[[no.scenario]] <- os_teacher_weekly[[no.scenario]] <- vector(mode="list", length=max_os)
  intro_weekly[[no.scenario]] <-intro_student_weekly[[no.scenario]] <-intro_teacher_weekly[[no.scenario]] <- vector(mode="list", length=max_intro)
  symp_weekly[[no.scenario]] <- symp_student_weekly[[no.scenario]] <- symp_teacher_weekly[[no.scenario]] <- vector(mode="list", length=max_symp)
  n_same_class_students[[no.scenario]] <- vector(mode="list", length=max_os)
  n_same_class_teachers[[no.scenario]] <- vector(mode="list", length=max_os)
  
  n_no_index_students[[no.scenario]] <- unlist(lapply(sim, function(y) lapply(y$n_no_index_students_list[[1]], function(x) x[[length(x)]])))
  n_no_index_teachers[[no.scenario]] <- unlist(lapply(sim, function(y) lapply(y$n_no_index_teachers_list[[1]], function(x) x[[length(x)]])))
  
  for(sim_k in seq(1,max_sim)){
    
    ### Number of vaccianted students
    n_vacc2doses_students[[no.scenario]] <- c(n_vacc2doses_students[[no.scenario]], unlist(lapply(sim[[sim_k]]$vacc_coverage2[[1]], function(x) x[1])))
    
    ### Reproduction number per day
    R_vector[[no.scenario]] <- rbind(R_vector[[no.scenario]], sim[[sim_k]]$R0_vec[seq(1, length(sim[[sim_k]]$time_steps), by=3)])
    
    ### Outbreak sizes  
    outbreak_size_students[[no.scenario]] <- c(outbreak_size_students[[no.scenario]], sim[[sim_k]]$outbreak_size_students[[no.scenario]])
    outbreak_size_teachers[[no.scenario]] <- c(outbreak_size_teachers[[no.scenario]], sim[[sim_k]]$outbreak_size_teachers[[no.scenario]])
    outbreak_data_students[[no.scenario]] <- c(outbreak_data_students[[no.scenario]], sim[[sim_k]]$outbreak_data_students[[no.scenario]])
    outbreak_data_teachers[[no.scenario]] <- c(outbreak_data_teachers[[no.scenario]], sim[[sim_k]]$outbreak_data_teachers[[no.scenario]])
    outbreak_per_loc_st[[no.scenario]] <- rbind(outbreak_per_loc_st[[no.scenario]], sim[[sim_k]]$outbreak_per_loc_st[[no.scenario]])
    outbreak_per_loc_teach[[no.scenario]] <- rbind(outbreak_per_loc_teach[[no.scenario]], sim[[sim_k]]$outbreak_per_loc_teach[[no.scenario]])
    abs_days_students[[no.scenario]] <- rbind(abs_days_students[[no.scenario]], sim[[sim_k]]$abs_days_students[[no.scenario]])
    absences_students[[no.scenario]] <- rbind(absences_students[[no.scenario]], sim[[sim_k]]$absences_students[[no.scenario]])
    sec_cases_students[[no.scenario]] <- c(sec_cases_students[[no.scenario]], sim[[sim_k]]$sec_cases_students[[no.scenario]])
    abs_days_teachers[[no.scenario]] <- rbind(abs_days_teachers[[no.scenario]], sim[[sim_k]]$abs_days_teachers[[no.scenario]])
    absences_teachers[[no.scenario]] <- rbind(absences_teachers[[no.scenario]], sim[[sim_k]]$absences_teachers[[no.scenario]])
    sec_cases_teachers[[no.scenario]] <- c(sec_cases_teachers[[no.scenario]], sim[[sim_k]]$sec_cases_teachers[[no.scenario]])
    infector_students[[no.scenario]] <- append(infector_students[[no.scenario]], sim[[sim_k]]$infector_students[[no.scenario]])
    infector_teachers[[no.scenario]] <- append(infector_teachers[[no.scenario]], sim[[sim_k]]$infector_teachers[[no.scenario]])
    contact_student_teachers[[no.scenario]] <- append(contact_student_teachers[[no.scenario]] , sim[[sim_k]]$contact_student_teachers[[no.scenario]])
    symptomatic_students[[no.scenario]] <- c(symptomatic_students[[no.scenario]], sim[[sim_k]]$symptomatic_students[[no.scenario]])
    symptomatic_teachers[[no.scenario]] <- c(symptomatic_teachers[[no.scenario]], sim[[sim_k]]$symptomatic_teachers[[no.scenario]])
    
    # ### Weekly new infections
    eff_n_iter <- length(sim[[sim_k]]$os_weekly[[no.scenario]])
    for(os_iter in 1:max_os){
      for(iter in 1:eff_n_iter){
        temp_n_same_class <- 0
        if(length(sim[[sim_k]]$os_weekly[[no.scenario]][[iter]])>=os_iter){
          if(length(sim[[sim_k]]$os_weekly[[no.scenario]][[iter]][[os_iter]])>0){
            os_weekly[[no.scenario]][[os_iter]] <- rbind(os_weekly[[no.scenario]][[os_iter]], sim[[sim_k]]$os_weekly[[no.scenario]][[iter]][[os_iter]])
          }else{
            os_weekly[[no.scenario]][[os_iter]] <- rbind(os_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
          }
        }else{
          os_weekly[[no.scenario]][[os_iter]] <- rbind(os_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
        }

        if(length(sim[[sim_k]]$os_student_weekly[[no.scenario]][[iter]])>=os_iter){
          if(length(sim[[sim_k]]$os_student_weekly[[no.scenario]][[iter]][[os_iter]])>0){
            os_student_weekly[[no.scenario]][[os_iter]] <- rbind(os_student_weekly[[no.scenario]][[os_iter]], sim[[sim_k]]$os_student_weekly[[no.scenario]][[iter]][[os_iter]])
          }else{
            os_student_weekly[[no.scenario]][[os_iter]] <- rbind(os_student_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
          }
        }else{
          os_student_weekly[[no.scenario]][[os_iter]] <- rbind(os_student_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
        }

        if(length(sim[[sim_k]]$os_teacher_weekly[[no.scenario]][[iter]])>=os_iter){
          if(length(sim[[sim_k]]$os_teacher_weekly[[no.scenario]][[iter]][[os_iter]])>0){
            os_teacher_weekly[[no.scenario]][[os_iter]] <- rbind(os_teacher_weekly[[no.scenario]][[os_iter]], sim[[sim_k]]$os_teacher_weekly[[no.scenario]][[iter]][[os_iter]])
          }else{
            os_teacher_weekly[[no.scenario]][[os_iter]] <- rbind(os_teacher_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
          }
        }else{
          os_teacher_weekly[[no.scenario]][[os_iter]] <- rbind(os_teacher_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
        }

        if(length(sim[[sim_k]]$n_same_class_students_list[[no.scenario]][[iter]])>=os_iter){
          if(length(sim[[sim_k]]$n_same_class_students_list[[no.scenario]][[iter]][[os_iter]]>0)){
            temp_n_same_class_students <- sim[[sim_k]]$n_same_class_students_list[[no.scenario]][[iter]][[os_iter]]
            n_same_class_students[[no.scenario]][[os_iter]] <- c(n_same_class_students[[no.scenario]][[os_iter]], temp_n_same_class)
          }
        }

        if(length(sim[[sim_k]]$n_same_class_teachers_list[[no.scenario]][[iter]])>=os_iter){
          if(length(sim[[sim_k]]$n_same_class_teachers_list[[no.scenario]][[iter]][[os_iter]])){
            temp_n_same_class_teachers <- sim[[sim_k]]$n_same_class_teachers_list[[no.scenario]][[iter]][[os_iter]]
            n_same_class_teachers[[no.scenario]][[os_iter]] <- c(n_same_class_teachers[[no.scenario]][[os_iter]], temp_n_same_class_teachers)
          }
        }
        
      } # End iter
    } # End os_iter (new infections)
    
    # ### Weekly introductions
    eff_n_iter <- length(sim[[sim_k]]$intro_weekly[[no.scenario]])
    for(os_iter in 1:max_intro){
      for(iter in 1:eff_n_iter){
        if(length(sim[[sim_k]]$intro_weekly[[no.scenario]][[iter]])>=os_iter){
          if(length(sim[[sim_k]]$intro_weekly[[no.scenario]][[iter]][[os_iter]])>0){
            intro_weekly[[no.scenario]][[os_iter]] <- rbind(intro_weekly[[no.scenario]][[os_iter]], sim[[sim_k]]$intro_weekly[[no.scenario]][[iter]][[os_iter]])
          }else{
            intro_weekly[[no.scenario]][[os_iter]] <- rbind(intro_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
          }
        }else{
          intro_weekly[[no.scenario]][[os_iter]] <- rbind(intro_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
        }
        
        if(length(sim[[sim_k]]$intro_student_weekly[[no.scenario]][[iter]])>=os_iter){
          if(length(sim[[sim_k]]$intro_student_weekly[[no.scenario]][[iter]][[os_iter]])>0){
            intro_student_weekly[[no.scenario]][[os_iter]] <- rbind(intro_student_weekly[[no.scenario]][[os_iter]], sim[[sim_k]]$intro_student_weekly[[no.scenario]][[iter]][[os_iter]])
          }else{
            intro_student_weekly[[no.scenario]][[os_iter]] <- rbind(intro_student_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
          }
        }else{
          intro_student_weekly[[no.scenario]][[os_iter]] <- rbind(intro_student_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
        }
        
        if(length(sim[[sim_k]]$intro_teacher_weekly[[no.scenario]][[iter]])>=os_iter){
          if(length(sim[[sim_k]]$intro_teacher_weekly[[no.scenario]][[iter]][[os_iter]])>0){
            intro_teacher_weekly[[no.scenario]][[os_iter]] <- rbind(intro_teacher_weekly[[no.scenario]][[os_iter]], sim[[sim_k]]$intro_teacher_weekly[[no.scenario]][[iter]][[os_iter]])
          }else{
            intro_teacher_weekly[[no.scenario]][[os_iter]] <- rbind(intro_teacher_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
          }
        }else{
          intro_teacher_weekly[[no.scenario]][[os_iter]] <- rbind(intro_teacher_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
        }
      }
    } # End os_iter (introductions)
    # 
    # ### Weekly new symptomatic infections
    eff_n_iter <- length(sim[[sim_k]]$symp_weekly[[no.scenario]])
    for(os_iter in 1:max_symp){
      for(iter in 1:eff_n_iter){
        if(length(sim[[sim_k]]$symp_weekly[[no.scenario]][[iter]])>=os_iter){
          if(length(sim[[sim_k]]$symp_weekly[[no.scenario]][[iter]][[os_iter]])>0){
            symp_weekly[[no.scenario]][[os_iter]] <- rbind(symp_weekly[[no.scenario]][[os_iter]], sim[[sim_k]]$symp_weekly[[no.scenario]][[iter]][[os_iter]])
          }else{
            symp_weekly[[no.scenario]][[os_iter]] <- rbind(symp_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
          }
        }else{
          symp_weekly[[no.scenario]][[os_iter]] <- rbind(symp_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
        }
        
        
        if(length(sim[[sim_k]]$symp_student_weekly[[no.scenario]][[iter]])>=os_iter){
          if(length(sim[[sim_k]]$symp_student_weekly[[no.scenario]][[iter]][[os_iter]])>0){
            symp_student_weekly[[no.scenario]][[os_iter]] <- rbind(symp_student_weekly[[no.scenario]][[os_iter]], sim[[sim_k]]$symp_student_weekly[[no.scenario]][[iter]][[os_iter]])
          }else{
            symp_student_weekly[[no.scenario]][[os_iter]] <- rbind(symp_student_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
          }
        }else{
          symp_student_weekly[[no.scenario]][[os_iter]] <- rbind(symp_student_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
        }
        
        
        if(length(sim[[sim_k]]$symp_teacher_weekly[[no.scenario]][[iter]])>=os_iter){
          if(length(sim[[sim_k]]$symp_teacher_weekly[[no.scenario]][[iter]][[os_iter]])>0){
            symp_teacher_weekly[[no.scenario]][[os_iter]] <- rbind(symp_teacher_weekly[[no.scenario]][[os_iter]], sim[[sim_k]]$symp_teacher_weekly[[no.scenario]][[iter]][[os_iter]])
          }else{
            symp_teacher_weekly[[no.scenario]][[os_iter]] <- rbind(symp_teacher_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
          }
        }else{
          symp_teacher_weekly[[no.scenario]][[os_iter]] <- rbind(symp_teacher_weekly[[no.scenario]][[os_iter]], rep(0, length=n_weeks))
        }
      }
    }# End os_iter (symptomatic infections)
    
    screening_stats[[no.scenario]] <- rbind(screening_stats[[no.scenario]], sim[[sim_k]]$screening_stats[[no.scenario]])
    det_screening[[no.scenario]] <- rbind(det_screening[[no.scenario]], sim[[sim_k]]$det_screening[[no.scenario]])
  }
  
  
  for(sim_k in 2:max_sim){
    sim_k_iter <- sim[[sim_k]]$iter
    for(s in 1:len_n_list){
      ns_list[[no.scenario]][[s]] <- cbind(ns_list[[no.scenario]][[s]], sim[[sim_k]]$ns_list[[no.scenario]][[s]])
      nt_list[[no.scenario]][[s]] <- cbind(nt_list[[no.scenario]][[s]], sim[[sim_k]]$nt_list[[no.scenario]][[s]])
    }
    ns_list[[no.scenario]][[len_n_list+1]] <- cbind(ns_list[[no.scenario]][[len_n_list+1]], nrow(sim[[sim_k]]$df_agent)-Reduce("+", sim[[sim_k]]$ns_list[[no.scenario]][2:5]))
    nt_list[[no.scenario]][[len_n_list+1]] <- cbind(nt_list[[no.scenario]][[len_n_list+1]], nrow(sim[[sim_k]]$df_teacher)-Reduce("+",sim[[sim_k]]$nt_list[[no.scenario]][2:5]))
    
    ns_list[[no.scenario]][[len_n_list+2]] <- cbind(ns_list[[no.scenario]][[len_n_list+2]], sapply(1:sim_k_iter, function(x) sim[[sim_k]]$ns_list[[no.scenario]][[19]][,x]-(nrow(sim[[sim_k]]$df_agent)-nrow(sim[[sim_k]]$df_agent)*sim[[sim_k]]$vacc_coverage2[[1]][[x]])))
    nt_list[[no.scenario]][[len_n_list+2]] <- cbind(nt_list[[no.scenario]][[len_n_list+2]], sapply(1:sim_k_iter, function(x) sim[[sim_k]]$nt_list[[no.scenario]][[19]][,x]-(nrow(sim[[sim_k]]$df_teacher)-floor(nrow(sim[[sim_k]]$df_teacher)*sim[[sim_k]]$prop_vaccinated[2]))))
    
    ### Reinfections
    eff_n_iter <- length(sim[[sim_k]]$df_reinfected_students_list[[no.scenario]])
    for(s in 1:len_n_reinf_list){
      for(iter in 1:eff_n_iter){
        n_reinfected_students_list[[no.scenario]][[s]]<- cbind(n_reinfected_students_list[[no.scenario]][[s]], sim[[sim_k]]$df_reinfected_students_list[[no.scenario]][[iter]][,s]) 
        n_reinfected_teachers_list[[no.scenario]][[s]] <- cbind(n_reinfected_teachers_list[[no.scenario]][[s]], sim[[sim_k]]$df_reinfected_teachers_list[[no.scenario]][[iter]][,s]) 
      }
      
    }
  }
  
  for(s in 1:len_n_reinf_list){
    colnames(n_reinfected_teachers_list[[no.scenario]][[s]]) <- paste0("iter_", seq(1, ncol(n_reinfected_teachers_list[[no.scenario]][[s]])))
    colnames(n_reinfected_students_list[[no.scenario]][[s]]) <- paste0("iter_", seq(1, ncol(n_reinfected_students_list[[no.scenario]][[s]])))
  }

  iter <- length(outbreak_size_students[[no.scenario]])
  print(paste0("Number of iterations = ", iter))
  scenario <- scenarios
  # Saving to file
  if(reduce){
    print(paste0("Saving to",path, sim_scenario_name, "/", sim_scenario_name, "_",no.scenario,"_reduced.RData"))
    save(R_vector,
         n_reinfected_students_list,
         n_reinfected_teachers_list,
         n_vacc2doses_students,
         os_weekly,
         os_student_weekly,
         os_teacher_weekly,
         symp_weekly,
         symp_student_weekly,
         symp_teacher_weekly,
         intro_weekly,
         intro_student_weekly,
         intro_teacher_weekly,
         n_same_class_students, 
         n_same_class_teachers,
         n_no_index_students, 
         n_no_index_teachers,
         ns_list, 
         nt_list,
         outbreak_size_students,  
         outbreak_size_teachers,  
         outbreak_data_students,  
         outbreak_data_teachers,  
         outbreak_per_loc_st,  
         outbreak_per_loc_teach,  
         abs_days_students,  
         absences_students,  
         sec_cases_students,  
         abs_days_teachers,  
         absences_teachers,  
         sec_cases_teachers,  
         infector_students,  
         infector_teachers,  
         contact_student_teachers,  
         symptomatic_students,  
         symptomatic_teachers,  
         screening_stats, 
         det_screening,
         scenario, 
         max_time, 
         n_weeks, 
         n_iter,
         max_sim,
         max_os, 
         max_intro, 
         max_symp,
         R0_vec,
         df_agent, 
         df_history, 
         df_teacher, 
         df_teach_hist,
         file = paste0(path, sim_scenario_name, "/", sim_scenario_name, "_",no.scenario, "_reduced.RData"))
  }else{
    print(paste0("Saving to",path, sim_scenario_name, "/", sim_scenario_name, "_",no.scenario,".RData"))
    save(list=ls(all.names=TRUE), 
         file = paste0(path, sim_scenario_name, "/", sim_scenario_name, "_",no.scenario, ".RData"), 
         envir = environment())
  }

  return(paste0(path, sim_scenario_name, "/", sim_scenario_name,  "_",no.scenario,".RData"))
}


# ---------------------------------------------------------------------------- #
# Combine simulations
# ---------------------------------------------------------------------------- #
combine.simulations <- function(name, no_scenarios=6, output_path="../results/", only_one=F){
  setwd("/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/code/")
  scenario_name = name
  epi = vector(mode="list", length=no_scenarios)
  for(sim_i in seq(1,no_scenarios)){
    epi[[sim_i]] = mget(load(paste0(output_path, scenario_name, "/", scenario_name, "_", sim_i, ".RData")))
  }

  print(paste0(output_path, scenario_name, "/", scenario_name, "_1.RData"))
  load(paste0(output_path, scenario_name, "/", scenario_name, "_1.RData"))
  for(sim_i in 2:no_scenarios){
    outbreak_size_students[[sim_i]] <- epi[[sim_i]]$outbreak_size_students[[sim_i]]
    outbreak_size_teachers[[sim_i]] <- epi[[sim_i]]$outbreak_size_teachers[[sim_i]]
    outbreak_data_students[[sim_i]] <- epi[[sim_i]]$outbreak_data_students[[sim_i]]
    outbreak_data_teachers[[sim_i]] <- epi[[sim_i]]$outbreak_data_teachers[[sim_i]]
    outbreak_per_loc_st[[sim_i]] <- epi[[sim_i]]$outbreak_per_loc_st[[sim_i]]
    outbreak_per_loc_teach[[sim_i]] <- epi[[sim_i]]$outbreak_per_loc_teach[[sim_i]]
    abs_days_students[[sim_i]] <- epi[[sim_i]]$abs_days_students[[sim_i]]
    absences_students[[sim_i]] <- epi[[sim_i]]$absences_students[[sim_i]]
    sec_cases_students[[sim_i]] <- epi[[sim_i]]$sec_cases_students[[sim_i]]
    ns_list[[sim_i]] <- epi[[sim_i]]$ns_list
    nt_list[[sim_i]] <- epi[[sim_i]]$nt_list
    abs_days_teachers[[sim_i]] <- epi[[sim_i]]$abs_days_teachers[[sim_i]]
    absences_teachers[[sim_i]] <- epi[[sim_i]]$absences_teachers[[sim_i]]
    sec_cases_teachers[[sim_i]] <- epi[[sim_i]]$sec_cases_teachers[[sim_i]]
    infector_students[[sim_i]] <- epi[[sim_i]]$infector_students[[sim_i]]
    infector_teachers[[sim_i]] <- epi[[sim_i]]$infector_teachers[[sim_i]]
    contact_student_teachers[[sim_i]] <- epi[[sim_i]]$contact_student_teachers[[sim_i]]
    symptomatic_students[[sim_i]] <- epi[[sim_i]]$symptomatic_students[[sim_i]]
    symptomatic_teachers[[sim_i]] <- epi[[sim_i]]$symptomatic_teachers[[sim_i]]
    
    intro_student_weekly[[sim_i]] <- epi[[sim_i]]$intro_student_weekly[[sim_i]]
    intro_teacher_weekly[[sim_i]] <- epi[[sim_i]]$intro_teacher_weekly[[sim_i]]
    intro_weekly[[sim_i]] <- epi[[sim_i]]$intro_weekly[[sim_i]]
    
    os_student_weekly[[sim_i]] <- epi[[sim_i]]$os_student_weekly[[sim_i]]
    os_teacher_weekly[[sim_i]] <- epi[[sim_i]]$os_teacher_weekly[[sim_i]]
    os_weekly[[sim_i]] <- epi[[sim_i]]$os_weekly[[sim_i]]
    
    screening_stats[[sim_i]] <- epi[[sim_i]]$screening_stats[[sim_i]]
    det_screening[[sim_i]] <- epi[[sim_i]]$det_screening[[sim_i]]
  }
  
  print(paste0("Saving to", output_path, scenario_name, "/", scenario_name, "_1-", no_scenarios, ".RData"))
  save(list=ls(all.names=TRUE), 
       file = paste0(output_path, scenario_name, "/", scenario_name, "_1-", no_scenarios, ".RData"), 
       envir = environment())
}



# ---------------------------------------------------------------------------- #
# Combine simulations
# ---------------------------------------------------------------------------- #
create.data.for.plotting <- function(file_path = "/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/results/long_sim/",
                                     figuresPath = "/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/figures/", 
                                     file_suffix = "townsend_fast_fastrecSlow_",
                                     # Scenarios
                                     scenarios = c("noInterv_townsend_redSusc_redInf", 
                                                   "noInterv_townsend_fullSusc_redInf", 
                                                   "noInterv_townsend_redSusc_fullInf",
                                                   "noInterv_townsend_redSusc_redInf",
                                                   "noInterv_fast_redSusc_redInf", 
                                                   "noInterv_fast_fullSusc_redInf", 
                                                   "noInterv_fast_redSusc_fullInf",
                                                   "noInterv_fast_redSusc_redInf",
                                                   "noInterv_fast_recSlow_redSusc_redInf", 
                                                   "noInterv_fast_recSlow_fullSusc_redInf", 
                                                   "noInterv_fast_recSlow_redSusc_fullInf"),
                                     names_scenarios = c("townsend: redSusc=0.75, redInf=0.5, R_w=1.2", 
                                                         "townsend: fullSusc=1.0, redInf=0.5, R_w=1.2", 
                                                         "townsend: redSusc=0.75, fullInf=1.0, R_w=1.2",
                                                         "townsend: redSusc=0.75, redInf=0.5, R_w=2.0",
                                                         "fast: redSusc=0.75, redInf=0.5, R_w=1.2", 
                                                         "fast: fullSusc=1.0, redInf=0.5, R_w=1.2", 
                                                         "fast: redSusc=0.75, fullInf=1.0, R_w=1.2",
                                                         "fast: redSusc=0.75, redInf=0.5, R_w=2.0",
                                                         "natImm slow: redSusc=0.75, redInf=0.5, R_w=1.2", 
                                                         "natImm slow: fullSusc=1.0, redInf=0.5, R_w=1.2", 
                                                         "natImm slow: redSusc=0.75, fullInf=1.0, R_w=1.2"),
                                     suffix_scenarios = c(rep("_R1-2", 3), "_R2-0", rep("_R1-2", 3), "_R2-0",rep("_R1-2", 3)),
                                     suffix = "_vacc60_18m",
                                     # Colors
                                     asymptmatic_col = "#f6b26b", 
                                     presymptomatic_col = "#ff8000", 
                                     symptomatic_col = "#cc0000", 
                                     scenario_colors = c("#07b4f9", "#1c4587","#674ea7", "#0000ff",
                                                         "#e1359d", "#f19ac0", "#be3679","#cc0000", 
                                                         "#6ccf43", "#339700", "#274e13")){
  # Loading functions
  source("model_code/covid19school_packages.R")
  source("plotting_code/covid19school_plotting_functions.R")
  
  # Colors for asymptomatic, presymptomatic, symptomatic infections
  inf_colors = c(symptomatic_col, presymptomatic_col, asymptmatic_col)
  
  # Create names
  prefix = "sim_"
  vacc_cov = parse_number(str_extract(suffix, "vacc[0-9]+"))
  if(is.na(vacc_cov)) parse_number(str_extract(scenarios[1], "vacc[0-9]+"))
  print(paste0("Vaccination coverage = ", vacc_cov, "%"))
  # Folder names
  names_list = paste0(prefix, scenarios, suffix_scenarios, suffix)
  print(paste0("names_list=", names_list))
  
  file_name <- paste0(prefix, scenarios[1], suffix_scenarios[1], suffix)
  print(paste0("file_name=", file_name))
  # Output folder
  current_folder = paste0("sim_", file_suffix, format(Sys.Date(), "%d%m%Y"))
  print(paste0("Current folder: ", current_folder))
  outputPath = paste0(figuresPath, current_folder, "/")
  if(!file.exists(outputPath)){
    print("Creating folder for the first time.")
    dir.create(outputPath)
  }else{
    print("Folder already exists. Possibly overwriting files.")
    line <- readline(prompt="Press [enter] to continue. Press Q to stop.")
    if(line == "Q") stop()
  }
  
  # Definition of output
  R_vector_list <- vector(mode="list", length=length(scenarios))
  os_weekly_total_list <- vector(mode="list", length=length(scenarios))
  os_students_weekly_total_list <- vector(mode="list", length=length(scenarios))
  os_teachers_weekly_total_list <- vector(mode="list", length=length(scenarios))
  
  os_weekly_list = vector(mode="list", length=length(names_list))
  os_students_weekly_list = vector(mode="list", length=length(names_list))
  os_teachers_weekly_list = vector(mode="list", length=length(names_list))
  
  intro_weekly_total_list <- vector(mode="list", length=length(scenarios))
  intro_students_weekly_total_list <- vector(mode="list", length=length(scenarios))
  intro_teachers_weekly_total_list <- vector(mode="list", length=length(scenarios))
  
  intro_weekly_list = vector(mode="list", length=length(names_list))
  intro_students_weekly_list = vector(mode="list", length=length(names_list))
  intro_teachers_weekly_list = vector(mode="list", length=length(names_list))
  
  symp_weekly_list = vector(mode="list", length=length(names_list))
  symp_students_weekly_list =  vector(mode="list", length=length(names_list))
  symp_teachers_weekly_list =  vector(mode="list", length=length(names_list))
  
  n_students_list = vector(mode="list", length=length(names_list))
  n_teachers_list = vector(mode="list", length=length(names_list))
  
  school_outbreak_students_list = vector(mode="list", length=length(names_list))
  school_outbreak_teachers_list = vector(mode="list", length=length(names_list))
  
  screening_stats_list = vector(mode="list", length=length(names_list))
  det_screening_list = vector(mode="list", length=length(names_list))
  
  for(ind_name in 1:length(names_list)){
    current_name <- names_list[[ind_name]]
    load(paste0(file_path, current_name, '/', current_name, '_1_reduced.RData'))
    print(paste0(file_path, current_name, '/', current_name, '_1_reduced.RData'))
    
    # Reproduction number
    R_vector_list[[ind_name]] <- R_vector
    
    # Number of new infections (per generation of infection, i.e., primary infection etc.)
    os_weekly_list[[ind_name]] <- os_weekly
    os_students_weekly_list[[ind_name]] <- os_student_weekly
    os_teachers_weekly_list[[ind_name]] <- os_teacher_weekly
    
    # Number of new symptomatic infections
    symp_weekly_list[[ind_name]] <- symp_weekly
    symp_students_weekly_list[[ind_name]] <- symp_student_weekly
    symp_teachers_weekly_list[[ind_name]] <- symp_teacher_weekly
    
    # Number of new introductions form community
    # intro_weekly_list[[ind_name]] <- intro_weekly
    intro_students_weekly_list[[ind_name]] <- intro_student_weekly
    intro_teachers_weekly_list[[ind_name]] <- intro_teacher_weekly
    
    # Number of susceptibles etc
    n_students_list[[ind_name]] <- ns_list
    n_teachers_list[[ind_name]] <- nt_list
    
    # Total outbreak size
    school_outbreak_students_list[[ind_name]] <- unlist(outbreak_size_students)
    school_outbreak_teachers_list[[ind_name]] <- unlist(outbreak_size_teachers)
    
    
    # Weekly total number of infections 
    (max_row <- max(unlist(sapply(1:length(scenarios), function(s) unlist(lapply(os_weekly_list[[s]][[1]], nrow)))), na.rm = T))
    for(s in 1:length(scenarios)){
      os_weekly_total_list[[s]] <- matrix(0, nrow=max_row, ncol=n_weeks)
      os_students_weekly_total_list[[s]] <- matrix(0, nrow=max_row, ncol=n_weeks)
      os_teachers_weekly_total_list[[s]] <- matrix(0, nrow=max_row, ncol=n_weeks)
      for(i in 1:length(os_weekly_list[[s]][[1]])){
        nrow <- max_row-nrow(os_weekly_list[[s]][[1]][[i]])
        if(length(nrow)>0){
          temp <- rbind(os_weekly_list[[s]][[1]][[i]], matrix(0, nrow=max_row-nrow(os_weekly_list[[s]][[1]][[i]]), ncol=n_weeks))
          os_weekly_total_list[[s]] <- os_weekly_total_list[[s]] + temp
        }
      }
      for(i in 1:length(os_students_weekly_list[[s]][[1]])){
        nrow <- max_row-nrow(os_students_weekly_list[[s]][[1]][[i]])
        if(length(nrow)>0){
          temp <- rbind(os_students_weekly_list[[s]][[1]][[i]], matrix(0, nrow=max_row-nrow(os_students_weekly_list[[s]][[1]][[i]]), ncol=n_weeks))
          os_students_weekly_total_list[[s]] <- os_students_weekly_total_list[[s]] + temp
        }
      }
      for(i in 1:length(os_teachers_weekly_list[[s]][[1]])){
        nrow <- max_row-nrow(os_teachers_weekly_list[[s]][[1]][[i]])
        if(length(nrow)>0){
          temp <- rbind(os_teachers_weekly_list[[s]][[1]][[i]], matrix(0, nrow=max_row-nrow(os_teachers_weekly_list[[s]][[1]][[i]]), ncol=n_weeks))
        }else temp <- 0
        os_teachers_weekly_total_list[[s]] <- os_teachers_weekly_total_list[[s]] + temp
      }
    }
    
    # Weekly total number of introductions
    (max_row <- max(unlist(sapply(1:length(scenarios), function(s) unlist(lapply(intro_students_weekly_list[[s]][[1]], nrow))))))
    for(s in 1:length(scenarios)){
      intro_weekly_total_list[[s]] <- matrix(0, nrow=max_row, ncol=n_weeks)
      intro_students_weekly_total_list[[s]] <- matrix(0, nrow=max_row, ncol=n_weeks)
      intro_teachers_weekly_total_list[[s]] <- matrix(0, nrow=max_row, ncol=n_weeks)
      for(i in 1:length(intro_weekly_list[[s]][[1]])){
        nrow <- max_row-nrow(intro_weekly_list[[s]][[1]][[i]])
        if(length(nrow)>0){
          temp <- rbind(intro_weekly_list[[s]][[1]][[i]], matrix(0, nrow=max_row-nrow(intro_weekly_list[[s]][[1]][[i]]), ncol=n_weeks))
          intro_weekly_total_list[[s]] <- intro_weekly_total_list[[s]] + temp
        }
      }
      for(i in 1:length(intro_students_weekly_list[[s]][[1]])){
        nrow <- max_row-nrow(intro_students_weekly_list[[s]][[1]][[i]])
        if(length(nrow)>0){
          temp <- rbind(intro_students_weekly_list[[s]][[1]][[i]], matrix(0, nrow=max_row-nrow(intro_students_weekly_list[[s]][[1]][[i]]), ncol=n_weeks))
          intro_students_weekly_total_list[[s]] <- intro_students_weekly_total_list[[s]] + temp
        }
      }
      for(i in 1:length(intro_teachers_weekly_list[[s]][[1]])){
        nrow <- max_row-nrow(intro_teachers_weekly_list[[s]][[1]][[i]])
        if(length(nrow)>0){
          temp <- rbind(intro_teachers_weekly_list[[s]][[1]][[i]], matrix(0, nrow=max_row-nrow(intro_teachers_weekly_list[[s]][[1]][[i]]), ncol=n_weeks))
        }else temp <- 0
        intro_teachers_weekly_total_list[[s]] <- intro_teachers_weekly_total_list[[s]] + temp
      }
    }
    
    
    
    # Screening statistics
    screening_stats_list[[ind_name]] <- unlist(lapply(lapply(screening_stats, function(x) apply(x, 1, function(y) sum(y, na.rm=T))), mean))
    det_screening_list[[ind_name]] <- unlist(lapply(lapply(det_screening, function(x) apply(x, 1, function(y) sum(y, na.rm=T))), mean))
  }
  

  
  
  scenarios_suffix = scenarios
  scenarios = names_scenarios
  colors = scenario_colors
  
  permutation = seq(1, length(scenarios))
  file_name = paste0("long_simulations_", file_suffix, length(scenarios), ".RData")
  print(paste0("File name: ", file_name))
  save(list=ls(all.names=TRUE), 
       file = paste0(file_path, file_name), 
       envir = environment())
  return(paste0(file_path, file_name))
}

# ---------------------------------------------------------------------------- #
# Plot number of susceptibles per day
# ---------------------------------------------------------------------------- #
n.per.day.fill <- function(n_list,
                           time_unit=1, 
                           time_unit_name="day",
                           type = c(3,2,4), 
                           type_names = c("presymptomatic", "symptomatic", "asymptomatic"),
                           scenarios, 
                           figuresPath="/figures/", 
                           suffix="students",
                           occup_suffix="half_occup",
                           title="STUDENTS",
                           title_y="Total outbreak size", 
                           start_date = "1/11/2021",
                           date_format = "%d/%m/%Y",
                           date_label= "%b/%y",
                           colors=colors_scenarios,
                           legend_position = c(0.85, 0.35),
                           percentage=F, 
                           width=18, height=10){
  ### Test
  if(length(type) > 1  & length(type_names)!=length(type)){
    print("Length of type names does not coincide with length of type!")
    return(NA)
  }
  
  n_days <- nrow(n_list[[1]][[1]][[1]])
  n_unit <- n_days/time_unit
  t_start <- seq(1, n_days, by=time_unit)
  t_end <- seq(t_start[2]-1, n_days, by=time_unit)
  n_per_day_list <- vector(mode="list", length=length(n_list))
  for(i in 1:length(n_list)){
    if(length(type)==1){
      n_per_day_list[[i]] <-  as.data.frame(cbind(time=1:nrow(n_list[[i]][[1]][[type[1]]]), n_list[[i]][[1]][[type]]))
      for(j in 1:ncol(n_per_day_list[[i]])){
        n_per_day_list[[i]][which(n_per_day_list[[i]][,j]<0), j] <- 0
      }
    }else{
      for(k in 1:length(type)){
        n_per_day_list[[i]][[k]] <- as.data.frame(cbind(time=1:nrow(n_list[[i]][[1]][[type[1]]]), n_list[[i]][[1]][[type[k]]]))
      }
      names(n_per_day_list[[i]]) <- type_names
    }
  }
  names(n_per_day_list) <- scenarios
  
  ### Number of individuals per unit of time (e.g., per week, per month, etc.)
  n_per_unit_list <- n_per_day_list
  if(time_unit>1) n_per_unit_list <- lapply(1:length(n_per_day_list), function(i) lapply(n_per_day_list[[i]], function(scenario_list) cbind(time=1:n_unit, t(sapply(1:n_unit, function(j) apply(scenario_list[t_start[j]:t_end[j],-1], 2, function(x) Reduce('max', x)))))))
  
  names(n_per_unit_list) <- scenarios
  df_n_per_unit <- reshape2::melt(n_per_unit_list, id="time")
  if(ncol(df_n_per_unit)==4) colnames(df_n_per_unit) <- c("time", "variable", "value", "scenario")
  if(ncol(df_n_per_unit)==5) colnames(df_n_per_unit) <- c("time", "variable", "value", "state", "scenario")
  
  date_seq <- seq(as.Date(start_date, format=date_format), by=time_unit_name, length.out=max(df_n_per_unit$time))
  time_nr <- unique(df_n_per_unit$time)
  df_n_per_unit$time <- do.call("c", lapply(df_n_per_unit$time, function(x) date_seq[which(time_nr==x)]))
  df_n_per_unit$scenario <- factor(df_n_per_unit$scenario, levels=scenarios)
  df_n_per_unit$state <- factor(df_n_per_unit$state, levels=type_names)
  
  if(percentage){
    df_n_per_unit <- df_n_per_unit %>% dplyr::group_by(time, variable, scenario) %>% dplyr::summarise(n=sum(value), value=ifelse(value/n<0 | is.na(value/n), 0, value/n)*100, state=state)
    
  }
  
  df_summary <- df_n_per_unit %>% dplyr::group_by(time, state, scenario) %>% dplyr::summarise(mean=mean(value), 
                                                                                             median=median(value), 
                                                                                             ci_lower=quantile(value, probs=0.025),
                                                                                             ci_upper=quantile(value, probs=0.975))

  ### Plot
  (plot <- ggplot(df_summary, aes(x=time, y=mean, fill=state)) +
      facet_wrap(.~scenario) + 
      geom_area(stat="identity") + 
      labs(y=title_y, x="Time (days)") + 
      scale_colour_manual(values=colors) + 
      scale_fill_manual(values=colors) +
      scale_x_date(date_breaks = '3 months', date_labels = date_label)+
      guides(colour=guide_legend(nrow=ifelse(legend_position=="bottom", 1, length(scenarios)))) + 
      theme_publication() + 
      theme(legend.position = legend_position, 
            legend.title = element_blank(), 
            legend.text = element_text(size=24), 
            legend.background = element_rect(colour = "black"),
            axis.text.x = element_text(angle=45, hjust=0.9), 
            strip.text = element_text(size=18)))

  ### Saving plot to file
  ggsave(plot, file=paste0(figuresPath, "n_per_day_fill_per_state_", suffix, "_",occup_suffix,"_plot.pdf"), 
         width=width, height=height)
  return(list(data=df_summary, plot=plot))
}


# ---------------------------------------------------------------------------- #
# Plot no. of individuals of a certain type (susceptible, infected,...) per week
# Assume the maximum in the week
# ---------------------------------------------------------------------------- #
n.per.unit <- function(n_list,
                       time_unit = 7,
                       time_unit_name = "week",
                       type = 1, 
                       scenarios, 
                       figuresPath="/figures/", 
                       suffix="students",
                       occup_suffix="half_occup",
                       title="STUDENTS",
                       title_y="Total outbreak size", 
                       start_date = "1/11/2021",
                       date_format = "%d/%m/%Y",
                       date_label = "%b %y",
                       colors=colors_scenarios,
                       legend_position = c(0.85, 0.15),
                       legend_title=NULL,
                       facet_title = "", 
                       percentage = F, 
                       n_total = 944,
                       vacc_eff = F, 
                       lower_ci = 0.025, upper_ci=0.975, 
                       width=18, height=10){
  n_per_day_list <- vector(mode="list", length=length(n_list))
  n_days <- nrow(n_list[[1]][[1]][[1]])
  n_unit <- n_days/time_unit
  t_start <- seq(1, n_days, by=time_unit)
  t_end <- seq(t_start[2]-1, n_days, by=time_unit)
  
  for(i in 1:length(n_list)){
    if(length(type)>1) temp <- Reduce('+', n_list[[i]][[1]][type])
    else temp <- n_list[[i]][[1]][[type]]
    n_per_day_list[[i]] <-  as.data.frame(cbind(time=1:nrow(n_list[[i]][[1]][[type[1]]]), temp))
    for(j in 1:ncol(n_per_day_list[[i]])){
      n_per_day_list[[i]][which(n_per_day_list[[i]][,j]<0), j] <- 0
    }
  }
  names(n_per_day_list) <- scenarios
  df_n_per_day <- reshape2::melt(n_per_day_list, id="time")
  colnames(df_n_per_day) <- c("time", "variable", "value", "scenario")
  if(percentage) df_n_per_day$value <- df_n_per_day$value/n_total
  
  
  ### Number of individuals per unit of time (e.g., per week, per month, etc.)  
  n_per_unit_list <- lapply(1:length(n_per_day_list), function(i) cbind(time=1:n_unit, t(sapply(1:n_unit, function(j) apply(n_per_day_list[[i]][t_start[j]:t_end[j],-1], 2, function(x) Reduce('max', x))))))
  names(n_per_unit_list) <- scenarios
  df_n_per_unit <- reshape2::melt(n_per_unit_list, id="time")
  if(percentage) df_n_per_unit$value <- 100*df_n_per_unit$value/n_total
  colnames(df_n_per_unit) <- c("time", "variable", "value", "scenario")
  
  ### Date sequence (depending on time unit)
  date_seq <- seq(as.Date(start_date, format=date_format), by=time_unit_name, length.out=max(df_n_per_unit$time))
  time_nr <- unique(df_n_per_unit$time)
  df_n_per_unit$time <- do.call("c", lapply(df_n_per_unit$time, function(x) date_seq[which(time_nr==x)]))
  df_n_per_unit$scenario <- factor(df_n_per_unit$scenario, levels=scenarios)
  if(facet_title!=""){
    use_facet <- T
    df_n_per_unit$facet <- facet_title
  }
  else use_facet <- F
  
  
  df_summary <- df_n_per_unit %>% dplyr::group_by(time, scenario) %>% dplyr::summarise(mean=mean(value), 
                                                                                       median=median(value), 
                                                                                       ci_lower=quantile(value, probs=lower_ci),
                                                                                       ci_upper=quantile(value, probs=upper_ci))
  ### Plot
  if(use_facet){
    print("Using facets.")
    (plot <- ggplot(df_n_per_unit, aes(x=time, y=value, color=scenario, group=scenario)) + 
       facet_wrap(~facet) + 
       geom_ribbon(data=df_summary, aes(x=time, y=mean, ymin=ci_lower, ymax=ci_upper, fill=scenario),
                   alpha=0.2, colour = NA) +
       geom_point(stat = "summary", fun = "mean", size=2.5) + 
       geom_line(stat = "summary", fun = "mean", size=1.5) + 
       labs(y=title_y, x=paste0("Time (", time_unit_name, "s)")) + 
       scale_colour_manual(values=colors, name=legend_title) + 
       scale_fill_manual(values=colors, name=legend_title) +
       scale_size_manual(values = c(2, rep(1.5, length(scenarios)-1))) +
       scale_x_date(date_breaks = "2 month", date_labels = date_label)+
       theme_publication() + 
       {if(legend_position%in%c("bottom","top")) guides(colour=guide_legend(nrow=1))} + 
       theme(legend.position = legend_position, 
             # legend.title = element_blank(),
             legend.title = if(is.null(legend_title)) element_blank() else element_text(size=28),
             legend.text = element_text(size=26), 
             legend.background = element_rect(colour = "black"),
             axis.text.x = element_text(size=28, angle=45, hjust=0.9),
             axis.title.y=element_text(size=29), 
             axis.title.x=element_blank(), 
             strip.text =element_text(size=34)))
  }else{
    print("No facets.")
    (plot <- ggplot(df_n_per_unit, aes(x=time, y=value, color=scenario, group=scenario)) + 
       geom_ribbon(data=df_summary, aes(x=time, y=mean, ymin=ci_lower, ymax=ci_upper, fill=scenario),
                   alpha=0.2, colour = NA) +
       geom_point(stat = "summary", fun = "mean", size=2.5) + 
       geom_line(stat = "summary", fun = "mean", size=1.5) + 
       labs(y=title_y, x=paste0("Time (", time_unit_name, "s)")) + 
       scale_colour_manual(values=colors, name=legend_title) + 
       scale_fill_manual(values=colors, name=legend_title) +
       scale_size_manual(values = c(2, rep(1.5, length(scenarios)-1))) +
       scale_x_date(date_breaks = "2 month", date_labels = date_label)+
       theme_publication() + 
       {if(legend_position%in%c("bottom","top")) guides(colour=guide_legend(nrow=1))} + 
       theme(legend.position = legend_position, 
             # legend.title = element_blank(),
             legend.title = if(is.null(legend_title)) element_blank() else element_text(size=28),
             legend.text = element_text(size=26), 
             legend.background = element_rect(colour = "black"),
             axis.text.x = element_text(size=28, angle=45, hjust=0.9),
             axis.title.y=element_text(size=29), 
             axis.title.x=element_blank()))
  }

  ### Saving to file
  plot_prefix <- ifelse(percentage, "perc_per_", "n_per_")
  ggsave(plot, file=paste0(figuresPath, plot_prefix, time_unit_name, "_",suffix, "_",occup_suffix,"_plot.pdf"), 
         width=width, height=height)
  return(list(data=df_summary, plot=plot))
}



# ---------------------------------------------------------------------------- #
# Plot no. of individuals of a certain type (susceptible, infected,...) per day
# ---------------------------------------------------------------------------- #
n.per.day <- function(n_list,
                      type = 1, 
                      scenarios, 
                      figuresPath="/figures/", 
                      suffix="students",
                      occup_suffix="half_occup",
                      title="STUDENTS",
                      title_y="Total outbreak size", 
                      start_date = "1/11/2021",
                      date_format = "%d/%m/%Y",
                      colors=colors_scenarios,
                      legend_position = c(0.85, 0.15),
                      legend_title="",
                      percentage = F, 
                      n_total = 944,
                      vacc_eff = F, 
                      width=18, height=10){
  n_per_day_list <- vector(mode="list", length=length(n_list))
  
  for(i in 1:length(n_list)){
    if(length(type)>1) temp <- Reduce('+', n_list[[i]][[1]][type])
    else temp <- n_list[[i]][[1]][[type]]
    n_per_day_list[[i]] <-  as.data.frame(cbind(time=1:nrow(n_list[[i]][[1]][[type[1]]]), temp))
    for(j in 1:ncol(n_per_day_list[[i]])){
      n_per_day_list[[i]][which(n_per_day_list[[i]][,j]<0), j] <- 0
    }
  }


  names(n_per_day_list) <- scenarios
  df_n_per_day <- reshape2::melt(n_per_day_list, id="time")
  colnames(df_n_per_day) <- c("time", "variable", "value", "scenario")
  if(percentage) df_n_per_day$value <- 100*df_n_per_day$value/n_total
  
  date_seq <- seq(as.Date(start_date, format=date_format), by="day", length.out=max(df_n_per_day$time))
  time_nr <- unique(df_n_per_day$time)
  df_n_per_day$time <- do.call("c", lapply(df_n_per_day$time, function(x) date_seq[which(time_nr==x)]))
  df_n_per_day$scenario <- factor(df_n_per_day$scenario, levels=scenarios)
  
  df_summary <- df_n_per_day %>% dplyr::group_by(time, scenario) %>% dplyr::summarise(mean=mean(value), 
                                                                                      median=median(value), 
                                                                                      ci_lower=quantile(value, probs=0.025),
                                                                                      ci_upper=quantile(value, probs=0.975))
  ### Plot
  (plot <- ggplot(df_n_per_day, aes(x=time, y=value, color=scenario, group=scenario)) + 
    geom_ribbon(data=df_summary, aes(x=time, y=mean, ymin=ci_lower, ymax=ci_upper, fill=scenario), 
                alpha=0.2, colour = NA) +
    geom_line(stat = "summary", fun = "mean", size=1.2) + 
    labs(y=title_y, x="Time (days)") + 
    scale_colour_manual(values=colors, name=legend_title) + 
    scale_fill_manual(values=colors, name=legend_title) +
    scale_x_date(date_breaks = '1 month', date_labels = '%b %y')+
    theme_publication() + 
    {if(legend_position%in%c("bottom","top")) guides(colour=guide_legend(nrow=1))} + 
    theme(legend.position = legend_position, 
          # legend.title = element_blank(),
          legend.title = element_text(size=28),
          legend.text = element_text(size=26), 
          legend.background = element_rect(colour = "black"),
          axis.text.x = element_text(angle=45, hjust=0.9)))
  ### Saving to file
  ggsave(plot, file=paste0(figuresPath, "n_per_day_",suffix, "_",occup_suffix,"_plot.pdf"), 
         width=width, height=height)
  return(list(data=df_summary, plot=plot))
}



# ---------------------------------------------------------------------------- #
# Time to extinction
# ---------------------------------------------------------------------------- #
plot.time.extinction <- function(data,
                                 figuresPath="/figures/", 
                                 suffix="",
                                 occup_suffix="", 
                                 color_plot){
  df_plot <- as.data.frame(cbind(value=apply(data, 2, function(x) min(which(x<=0)))))
  infinite_ind <- is.infinite(df_plot$value)
  df_plot <- as.data.frame(cbind(value=df_plot$value[!infinite_ind]))
  
  plot <- ggplot(data=df_plot, aes(x=value, y=..density..)) + 
            geom_histogram(bins = 50, fill=color_plot, col="black") + 
            geom_vline(xintercept=quantile(unlist(df_plot), probs=0.5), col="red", lty=2) + 
            geom_vline(xintercept=mean(unlist(df_plot)), col="darkred", lty=2) + 
            scale_x_continuous(breaks=seq(0, max(df_plot$value), by=20)) + 
            labs(x="Time to extinction", y="Probability density") + 
            theme_publication()
  
  ggsave(plot, file=paste0(figuresPath, "time_to_extinction_",suffix, occup_suffix,"_plot.pdf"), 
         width=18, height=10)
  return(list(plot=plot, summary=summary(df_plot), infinite.length=sum(infinite_ind)))
}


# ---------------------------------------------------------------------------- #
# Plot outbreak size over time
# ---------------------------------------------------------------------------- #
plot.outbreak.size.weekly.per.infection <- function(data_weekly, 
                                                    scenarios, 
                                                    group = list(1, 2:length(data_weekly)),
                                                    figuresPath="/figures/", 
                                                    suffix="students",
                                                    occup_suffix="",
                                                    title="STUDENTS",
                                                    title_y="Total outbreak size", 
                                                    title_x="Week",
                                                    legend_position = "none",
                                                    start_date = "1/11/2021", 
                                                    date_format = "%d/%m/%Y",
                                                    colors = NULL, 
                                                    use.mean = T, 
                                                    line.plot = F,
                                                    facets = F, 
                                                    scaling_factor = 1, 
                                                    width=16, height=9){
  
  remove_ind <- NULL
  for(i in 1:length(scenarios)){
    if(ncol(data_weekly[[i]])!=0){
      colnames(data_weekly[[i]]) <- seq(1,ncol(data_weekly[[i]]))
    }else{
      remove_ind <- c(remove_ind, i)
    } 
  }
  if(length(remove_ind)>0) {
    data_weekly <- data_weekly[-remove_ind]
    names(data_weekly) <- scenarios[-remove_ind]
  }else{
    data_weekly <- data_weekly
    names(data_weekly) <- scenarios
  }
  
  if(!is.null(group)){
    data_weekly_grouped <- vector(mode="list", length=length(group))
    for(i in 1:length(group)){
      data_weekly_grouped[[i]] <- Reduce("+", data_weekly[group[[i]]])
    }
    names(data_weekly_grouped) <- scenarios
  }else{
    data_weekly_grouped <- data_weekly
  }
  
  
  inf <- reshape2::melt(data_weekly_grouped)
  if(ncol(inf)==3) colnames(inf) <- c("week", "value", "scenario")
  if(ncol(inf)==4) colnames(inf) <- c("iter","week", "value", "scenario")
  if(ncol(inf)==5){
    inf <- inf[, -which(colnames(inf)=="variable")]
    colnames(inf) <- c("iter","week", "value", "scenario")
  }
  inf$scenario <- factor(inf$scenario, levels=scenarios)
  inf$week <- as.numeric(inf$week)
  inf$value <- as.numeric(inf$value)
  n_weeks <- max(inf$week)
  
  date_seq <- seq(as.Date(start_date, format=date_format), by="week", length.out=n_weeks)
  week_nr <- unique(inf$week)
  inf$week <- do.call("c", lapply(inf$week, function(x) date_seq[which(week_nr==x)]))
  
  
  df_plot <- inf %>% dplyr::group_by(scenario, week) %>% dplyr::summarize(mean=mean(value), 
                                                                          median=median(value), 
                                                                          ci_lower=quantile(value, probs=0.025),
                                                                          ci_upper=quantile(value, probs=0.975))
  df_plot$scenario <- factor(df_plot$scenario, levels=scenarios)
  
  if(line.plot){
    plot <- ggplot(data=inf, aes(x=week, y=scaling_factor*value, fill=scenario, color=scenario)) + 
      facet_wrap(scenario~., strip.position="top", nrow=3) + 
      
      stat_summary(aes(group=1), fun=mean, geom="line",group=1,lwd=1.5) + 
      stat_summary(aes(group=1), fun=mean, geom="point",group=1,lwd=2.5) + 
      # geom_bar(position="dodge", stat = "summary", fun = "mean", width=0.9) +
      geom_errorbar(data=df_plot, aes(x=week, y=mean,ymin=ci_lower,ymax=ci_upper),lty=1, lwd=0.9, width=0.05)+
      labs(y=title_y, x=title_x) +
      scale_x_date(date_breaks = '1 month', date_labels = '%b %y')+
      # scale_colour_manual(values=colors) + 
      # scale_fill_manual(values=colors) +
      theme_publication() + 
      theme(legend.position="none",
            axis.text.x = element_text(angle=45, hjust=0.9))
  }else{
    if(facets){
      plot <- ggplot(data=inf, aes(x=week, y=scaling_factor*value, fill=scenario, color=scenario)) + 
        facet_wrap(scenario~., strip.position="top", nrow=length(scenarios)) +
        annotate("rect", xmin=min(df_plot$week),xmax=as.Date("30/03/2022", format=date_format),ymin=-Inf,ymax=Inf,
                 fill="gray77", color="black", linetype=29, alpha=0.3) +
        annotate("rect",xmin=as.Date("01/04/2022", format=date_format),xmax=as.Date("30/09/2022", format=date_format),ymin=-Inf,ymax=Inf,
                 fill="gray88", color="NA", linetype=29, alpha=0.3) +
        annotate("rect", xmin=as.Date("01/10/2022", format=date_format),xmax=as.Date("30/03/2023", format=date_format),ymin=-Inf,ymax=Inf,
                 fill="gray77", color="black", linetype=29, alpha=0.3) +
        annotate("rect", xmin=as.Date("01/04/2023", format=date_format),xmax=as.Date("30/09/2023", format=date_format),ymin=-Inf,ymax=Inf,
                 fill="gray88", color="NA", linetype=29, alpha=0.3) +
        annotate("rect", xmin=as.Date("01/10/2023", format=date_format),xmax=as.Date("30/03/2024", format=date_format),ymin=-Inf,ymax=Inf,
                 fill="gray77", color="black", linetype=29, alpha=0.3) + 
        stat_summary(aes(group=1), fun=mean, geom="line",group=1,lwd=1.5) + 
        stat_summary(aes(group=1), fun=mean, geom="point",group=1,lwd=2.5) + 
        # geom_bar(position="dodge", stat = "summary", fun = "mean", width=0.9) +
        geom_ribbon(data=df_plot, aes(x=week, y=mean, ymin=ci_lower, ymax=ci_upper, fill=scenario), 
                    alpha=0.2, colour = NA) +
        labs(y=title_y, x=title_x) +
        scale_x_date(date_breaks = '2 months', date_labels = '%b %y')+
        # scale_colour_manual(values=colors) + 
        # scale_fill_manual(values=colors) +
        theme_publication() + 
        theme(legend.position="none",
              axis.title.y = element_text(size=30),
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle=45, hjust=0.9))
    }else{
      plot <- ggplot(data=inf, aes(x=week, y=scaling_factor*value, fill=scenario, color=scenario)) + 
        annotate("rect", xmin=min(df_plot$week),xmax=as.Date("30/03/2022", format=date_format),ymin=-Inf,ymax=Inf,
                 fill="gray77", color="black", linetype=29, alpha=0.3) +
        annotate("rect",xmin=as.Date("01/04/2022", format=date_format),xmax=as.Date("30/09/2022", format=date_format),ymin=-Inf,ymax=Inf,
                 fill="gray88", color="NA", linetype=29, alpha=0.3) +
        annotate("rect", xmin=as.Date("01/10/2022", format=date_format),xmax=as.Date("30/03/2023", format=date_format),ymin=-Inf,ymax=Inf,
                 fill="gray77", color="black", linetype=29, alpha=0.3) +
        annotate("rect", xmin=as.Date("01/04/2023", format=date_format),xmax=as.Date("30/09/2023", format=date_format),ymin=-Inf,ymax=Inf,
                 fill="gray88", color="NA", linetype=29, alpha=0.3) +
        annotate("rect", xmin=as.Date("01/10/2023", format=date_format),xmax=as.Date("30/03/2024", format=date_format),ymin=-Inf,ymax=Inf,
                 fill="gray77", color="black", linetype=29, alpha=0.3) + 
        stat_summary(aes(group=1), fun=mean, geom="line",group=1,lwd=1.5) + 
        stat_summary(aes(group=1), fun=mean, geom="point",group=1,lwd=2.5) + 
        # geom_bar(position="dodge", stat = "summary", fun = "mean", width=0.9) +
        geom_ribbon(data=df_plot, aes(x=week, y=mean, ymin=ci_lower, ymax=ci_upper, fill=scenario), 
                    alpha=0.2, colour = NA) +
        labs(y=title_y, x=title_x) +
        scale_x_date(date_breaks = '2 months', date_labels = '%b %y')+
        # scale_colour_manual(values=colors) +
        # scale_fill_manual(values=colors) +
        theme_publication() + 
        theme(legend.title = element_blank(),
              legend.text = element_text(size=30),
              legend.position=legend_position,
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle=45, hjust=0.9))
    }

  }
  ggsave(plot, file=paste0(figuresPath, "os_weekly_",suffix, "_", occup_suffix,"_plot.pdf"), 
         width=width, height=height)
  df_plot[,-c(1,2)] <- apply(df_plot[,-c(1,2)], 2, function(x) round(x,1))
  return(list(plot=plot, df=df_plot))
}





# ---------------------------------------------------------------------------- #
# Plot outbreak size over time
# ---------------------------------------------------------------------------- #
plot.outbreak.size.weekly <- function(data_weekly, 
                                      scenarios, 
                                      figuresPath="/figures/", 
                                      suffix="students",
                                      occup_suffix="",
                                      title="STUDENTS",
                                      title_y="Total outbreak size", 
                                      title_x="Week",
                                      start_date = "1/11/2021", 
                                      date_format = "%d/%m/%Y",
                                      colors=NULL, 
                                      use.mean=T, 
                                      violin=F, 
                                      line.plot=F,
                                      scaling_factor=1,
                                      percentage=F,
                                      n_total = 944,
                                      width=16, height=9){
  for(i in 1:length(scenarios)){
    colnames(data_weekly[[i]]) <- seq(1,ncol(data_weekly[[i]]))
  }
  names(data_weekly) <- scenarios
  inf <- reshape2::melt(data_weekly)
  if(ncol(inf)==3) colnames(inf) <- c("week", "value", "scenario")
  if(ncol(inf)==4) colnames(inf) <- c("iter","week", "value", "scenario")
  inf$scenario <- factor(inf$scenario, levels=scenarios)
  inf$week <- as.numeric(inf$week)
  inf$value <- as.numeric(inf$value)
  n_weeks <- max(inf$week)
  
  date_seq <- seq(as.Date(start_date, format=date_format), by="week", length.out=max(inf$week))
  week_nr <- unique(inf$week)
  inf$week <- do.call("c", lapply(inf$week, function(x) date_seq[which(week_nr==x)]))

  if(use.mean){
    value <- lapply(data_weekly, function(x) apply(x, 2, function(y) mean(y)))
  }else value <- lapply(data_weekly, function(x) apply(x, 2, function(y) median(y)))
  ci_lower <- lapply(data_weekly, function(x) apply(x, 2, function(y) quantile(y, probs=0.025)))
  ci_upper <- lapply(data_weekly, function(x) apply(x, 2, function(y) quantile(y, probs=0.975)))
  
  if(percentage) inf$value <- 100*inf$value/944
  
  df_plot <- inf %>% dplyr::group_by(scenario, week) %>% dplyr::summarise(mean=mean(value), 
                                                                          median=median(value), 
                                                                          ci_lower=quantile(value, probs=0.025),
                                                                          ci_upper=quantile(value, probs=0.975))
  df_plot$scenario <- factor(df_plot$scenario, levels=scenarios)

  # df_plot <- cbind(week = seq(1,ncol(data_weekly[[1]])), melt(value))
  # colnames(df_plot) <- c("week", "mean", "scenario")
  # df_plot <- df_plot[, c("scenario", "week", "mean")]
  # df_plot$scenario <- factor(df_plot$scenario, levels=scenarios)
  # df_plot <- as.data.frame(cbind(df_plot,  ci_lower=melt(ci_lower)[,1], ci_upper=melt(ci_upper)[,1]))
  # df_plot[,-c(1,2)] <- apply(df_plot[,-c(1,2)], 2, function(x) as.numeric(x)*scaling_factor)
  
  if(violin){
    plot <- ggplot(data=inf, aes(x=week, y=scaling_factor*value, fill=scenario, color=scenario)) + 
              facet_wrap(scenario~., strip.position="top", nrow=3) + 
              geom_bar(position="dodge", stat = "summary", fun = "mean", width=0.9) +
              geom_violin(alpha=0.2, color="darkgrey") + 
              labs(y=title_y) +
              scale_colour_manual(values=colors) + 
              scale_fill_manual(values=colors) +
              theme_publication() + 
              theme(legend.position="none", 
                    axis.title.x = element_blank())
  }else{
    plot <- ggplot(data=inf, aes(x=week, y=scaling_factor*value, fill=scenario, color=scenario)) + 
              facet_wrap(scenario~., strip.position="top", nrow=3) + 
              geom_bar(position="dodge", stat = "summary", fun = "mean", width=0.9) +
              geom_errorbar(data=df_plot, aes(x=week, y=mean,ymin=ci_lower,ymax=ci_upper),lty=1, lwd=0.9, width=0.05, color="black")+
              labs(y=title_y, x=title_x) +
              scale_colour_manual(values=colors) + 
              scale_fill_manual(values=colors) +
              scale_x_date(date_breaks = '1 month', date_labels = '%b %y')+
              theme_publication() + 
              theme(legend.position="none", 
                    axis.title.x = element_blank(), 
                    axis.text.x = element_text(angle=45, hjust=1.0))
  }
  if(line.plot){
    plot <- ggplot(data=inf, aes(x=week, y=scaling_factor*value, fill=scenario, color=scenario)) + 
      facet_wrap(scenario~., strip.position="top", nrow=3) + 
      stat_summary(aes(group=1), fun=mean, geom="line",group=1,lwd=1) + 
      stat_summary(aes(group=1), fun=mean, geom="point",group=1,lwd=3) + 
      # geom_bar(position="dodge", stat = "summary", fun = "mean", width=0.9) +
      geom_errorbar(data=df_plot, aes(x=week, y=mean,ymin=ci_lower,ymax=ci_upper),lty=1, lwd=0.9, width=0.05)+
      labs(y=title_y) +
      scale_colour_manual(values=colors) + 
      scale_fill_manual(values=colors) +
      scale_x_date(date_breaks = '1 week', date_labels = '%d %b %y')+
      theme_publication() + 
      theme(legend.position="none", 
            axis.title.x = element_blank(), 
            axis.text.x = element_text(angle=45, hjust=1.0))
  }
  ggsave(plot, file=paste0(figuresPath, "outbreak_size_",suffix, "_", occup_suffix,"_plot.pdf"), 
         width=width, height=height)
  df_plot[,-c(1,2)] <- apply(df_plot[,-c(1,2)], 2, function(x) round(x,1))
  return(list(plot=plot, df=df_plot))
}


# ---------------------------------------------------------------------------- #
# Plot outbreak size over time
# ---------------------------------------------------------------------------- #
plot.outbreak.size.weekly.combined <- function(data_weekly_list, 
                                               scenarios, 
                                               figuresPath="/figures/", 
                                               suffix="students",
                                               occup_suffix="half_occup",
                                               title="STUDENTS",
                                               title_y="Total outbreak size", 
                                               title_x="Week",
                                               colors=NULL, 
                                               start_date = NULL, 
                                               date_format = "%d/%m/%Y",
                                               width=20, height=10){
  names(data_weekly_list) <- scenarios
  for(i in 1:length(data_weekly_list)){
    if(is.null(ncol(data_weekly_list[[i]]))) data_weekly_list[[i]] <- data_weekly_list[[i]][[1]]
    colnames(data_weekly_list[[i]]) <- seq(1, ncol(data_weekly_list[[i]]))
  }
  inf <- reshape2::melt(data_weekly_list)
  if(ncol(inf)==3) colnames(inf) <- c("week", "value", "scenario")
  if(ncol(inf)==4) colnames(inf) <- c("iter","week", "value", "scenario")
  inf$scenario <- factor(inf$scenario, levels=scenarios)
  inf$week <- as.numeric(inf$week)
  inf$value <- as.numeric(inf$value)
  n_weeks <- max(inf$week, na.rm = T)
  
  date_seq <- seq(as.Date(start_date, format=date_format), by="week", length.out=n_weeks)
  week_nr <- unique(inf$week)
  inf$week <- do.call("c", lapply(inf$week, function(x) date_seq[which(week_nr==x)]))
  
  df_plot <- inf %>% dplyr::group_by(scenario, week) %>% dplyr::summarise(mean=mean(value), 
                                                                          median=median(value), 
                                                                          ci_lower=quantile(value, probs=0.025),
                                                                          ci_upper=quantile(value, probs=0.975))
  df_plot$scenario <- factor(df_plot$scenario, levels=scenarios)
  
  plot <- ggplot(data=inf, aes(x=week, y=value, fill=scenario, color=scenario, group=scenario)) + 
    geom_ribbon(data=df_plot, aes(x=week, y=median, ymin=ci_lower, ymax=ci_upper, fill=scenario), 
                alpha=0.2, colour = NA) + 
    geom_point(data=inf, aes(x=week, y=value), stat = "summary", fun = "mean") +
    geom_line(data=inf, aes(x=week, y=value), stat = "summary", fun = "mean") + 
    labs(y=title_y) +
    scale_colour_manual(values=colors) + 
    scale_fill_manual(values=colors) +
    scale_x_date(date_breaks = '1 month', date_labels = '%b %y')+
    theme_publication() + 
    theme(axis.text.x = element_text(angle=45, hjust=0.9),
          axis.title.x = element_blank(),
          legend.text = element_text(size=20),
          legend.title = element_blank(), 
          legend.position = c(0.8,0.8), 
          legend.background = element_rect(colour = "black"))
  plot
  ggsave(plot, file=paste0(figuresPath, "outbreak_size_weekly_",suffix, occup_suffix,"_plot.pdf"), 
         width=width, height=height)
  df_plot[,-c(1,2)] <- apply(df_plot[,-c(1,2)], 2, function(x) round(x,1))
  return(list(plot=plot, df=df_plot))
}

# ---------------------------------------------------------------------------- #
# Plot outbreak probability over time
# ---------------------------------------------------------------------------- #
plot.outbreak.prob.weekly <- function(os_weekly, 
                                      intro_weekly,
                                      scenarios, 
                                      permutation=NULL,
                                      figuresPath="/figures/", 
                                      suffix="students",
                                      occup_suffix="half_occup",
                                      title="",
                                      title_y="Outbreak probability (%)", 
                                      colors=NULL){
  if(length(permutation)>0){
    os_weekly <- os_weekly[permutation]
    colors <- colors_scenarios[permutation]
  }else{
    colors <- rev(heat.colors(length(os_weekly)))
  }
  
  outbreak_prob_list <- vector(mode="list", length=length(scenarios))
  for(i in 1:length(os_weekly)){
    for(j in 1:ncol(os_weekly[[i]])){
      outbreak_prob_list[[i]][j] <- sum(os_weekly[[i]][, j]>intro_weekly[[i]][,j], na.rm=T)/length(os_weekly[[i]][, j])           
    } 
  }
  names(outbreak_prob_list) <- scenarios
  
  df_plot <- cbind(week=paste("Week", seq(1, ncol(os_weekly[[1]]))), reshape2::melt(outbreak_prob_list))
  colnames(df_plot) <- c("week", "value", "scenario")
  df_plot$value <- df_plot$value*100
  
  plot <- ggplot(data=df_plot, aes(x=week, y=value, fill=scenario, color=scenario)) + 
    facet_wrap(scenario~., strip.position="top", nrow=3) + 
    geom_bar(position="dodge", stat = "identity", width=0.9) +
    labs(y=title_y) +
    scale_y_continuous(breaks=seq(0,max(df_plot$value), by=10))+
    scale_colour_manual(values=colors) + 
    scale_fill_manual(values=colors) +
    theme_publication() + 
    theme(legend.position="none", 
          axis.title.x = element_blank())
  ggsave(plot, file=paste0(figuresPath, "outbreak_prob_",suffix, occup_suffix,"_plot.pdf"), 
         width=16, height=9)
  outbreak_prob_list <- lapply(outbreak_prob_list, function(x) round(x*100, 1))
  return(list(plot=plot, df=outbreak_prob_list))
}


# ---------------------------------------------------------------------------- #
# Plot vaccination coverage over time
# ---------------------------------------------------------------------------- #
plot.vacc.cov <- function(vacc_vec, weeks=6, y_lab="Proportion of vaccination students\nwith at least one dose"){
  time <- seq(0,weeks, by=1)
  df_vacc_cov <- as.data.frame(cbind(time, vacc_cov=vacc_vec))
  plot <- ggplot(df_vacc_cov, aes(x=time, y=vacc_cov)) + 
            geom_bar(stat="identity") + 
            labs(y=y_lab, x="week") + 
            scale_x_continuous(breaks=df_vacc_cov$time) + 
            scale_y_continuous(breaks=seq(0,ceiling(max(df_vacc_cov$vacc_cov)),by=0.1)) +
            geom_text(aes(label = round(vacc_cov,2)), vjust = 1.5, colour = "white", size=10) +
            theme_publication()
  return(list(df=df_vacc_cov, plot=plot))
}


# ---------------------------------------------------------------------------- #
# Plot outbreak size for scenarios over range of introduction probabilities
# ---------------------------------------------------------------------------- #
plot.lines.outbreak.vs.intro <- function(school_outbreak_students_list, 
                                         school_outbreak_teachers_list,
                                         intro_students_list,
                                         intro_teachers_list,
                                         scenarios=c("Full occupancy", 
                                                     "Full occupancy (no quarantine)",
                                                     "Risk-based testing",
                                                     "Risk-based_testing (no quarantine)", 
                                                     "Screening 2x weekly", 
                                                     "Screening 2x weekly (no quarantine)"), 
                                         x_axis_labels=scenarios_plot,
                                         permutation=NULL,
                                         iter=1, 
                                         figuresPath="/figures/", 
                                         suffix="students",
                                         occup_suffix="full_occup",
                                         title="STUDENTS",
                                         title_y="Total outbreak size", 
                                         colors=NULL,
                                         mean=T,
                                         file_name = "total_outbreak_vs_intro_line_students_smooth_plot_all"){
  len <- length(school_outbreak_students_list)
  df_inf_students_list <- vector(mode="list", length=len)
  inf_students_list <- vector(mode="list", length=len)
  inf_summary_list <-  vector(mode="list", length=len)
  df_inf_teachers_list <- vector(mode="list", length=len)
  inf_teachers_list <- vector(mode="list", length=len)
  if(mean) fun_plot <- ifelse(mean, "mean", "median")
  names(school_outbreak_students_list) <- scenarios
  names(school_outbreak_teachers_list) <- scenarios
  names(intro_students_list) <- scenarios
  names(intro_teachers_list) <- scenarios
  
  for(k in 1:len){
    df_inf_students_list[[k]] <- data.frame(matrix(0, nrow=length(school_outbreak_students_list[[k]][[1]]), ncol=length(scenarios_plot)))
    df_inf_teachers_list[[k]] <- data.frame(matrix(0, nrow=length(school_outbreak_teachers_list[[k]][[1]]), ncol=length(scenarios_plot)))
    for(i in 1:length(scenarios_plot)){
      df_inf_students_list[[k]][,i] <- school_outbreak_students_list[[k]][[i]] + intro_students_list[[k]][[i]]
      df_inf_teachers_list[[k]][,i] <- school_outbreak_teachers_list[[k]][[i]] + intro_teachers_list[[k]][[i]]
    }
    inf_students_list[[k]] <- reshape2::melt(df_inf_students_list[[k]])
    inf_students_list[[k]]$value <- as.numeric(inf_students_list[[k]]$value)
    if(mean) inf_summary_list[[k]] <- inf_students_list[[k]]%>%group_by(variable)%>%summarize(value=mean(value))
    else inf_summary_list[[k]] <- inf_students_list[[k]]%>%group_by(variable)%>%summarize(value=median(value))
  }
  
  names(inf_summary_list) <- scenarios
  data_inf_plot <- melt(inf_summary_list)
  # data_inf_plot <- data_inf_plot[,-2]
  names(data_inf_plot) <- c("variable", "value","scenario")
  data_inf_plot$variable <- x_axis_labels
  
  # temp <- as.data.frame(matrix(unlist(lapply(inf_summary_list, function(x) 100*round(x$value,2))), ncol=4))
  # temp <- cbind(round(x_axis_labels*944*28), temp)
  # colnames(temp) <- c("intro", scenarios)
  # temp
  # write.csv(temp, file=paste0("../results/tab_convert_community_incidence.csv"))
  
  inf_plot <- ggplot(data_inf_plot, aes(x=variable, y=value*100, group=scenario, color=scenario)) + 
    # geom_point() +
    # geom_line() +
    # geom_smooth()+
    geom_smooth(se=FALSE, method = "lm") +
    geom_dl(aes(label=scenario), method = list(dl.trans(x = x + 1.6), "last.bumpup")) +
    labs(x="Mean number of cases introduced from community (per month, per school)", 
         y=paste("Mean number of infected students within one month\n(school-related and from community, per 100.000)")) + 
    scale_x_continuous(breaks=seq(min(ceiling(data_inf_plot$variable)), max(ceiling(data_inf_plot$variable)))) +
    scale_y_continuous(breaks=seq(0, max(data_inf_plot$value*100), by=500)) +
    scale_color_manual(values=colors) + 
    theme_publication() + 
    theme(legend.position="none",
          axis.text.x = element_text(size=16,angle=45,hjust=1.0),
          axis.title = element_text(size=20),
          plot.margin = unit(c(0.1, 8, 0.1, 0.1), "cm"))
  gt1 <- ggplotGrob(inf_plot)
  gt1$layout$clip[gt1$layout$name == "panel"] <- "off"
  grid::grid.draw(gt1)
  
  ggsave(gt1, file=paste0("../results/", file_name,".pdf"), width=16, height=9)
  data <- data_inf_plot
  data$value <- data$value*100
  return(list(data=data_inf_plot, plot=gt1, inf_plot=inf_plot))
}


# ---------------------------------------------------------------------------- #
# Outbreak probability
# ---------------------------------------------------------------------------- #
plot.outbreak.prob <- function(outbreak_size_students, 
                               outbreak_size_teachers, 
                               outbreak_external_st,
                               outbreak_external_teach,
                               external_prob,
                               scenarios, 
                               permutation=NULL,
                               iter=1, 
                               figuresPath="/figures/low_external_foi/", 
                               suffix="students",
                               occup_suffix="full_occup",
                               title="STUDENTS",
                               title_y="Outbreak probability", 
                               colors=NULL){
  if(length(permutation)>0){
    outbreak_size <- outbreak_size[permutation]
    outbreak_external <- outbreak_external[permutation]
    colors <- colors_scenarios[permutation]
  }else{
    colors <- rev(heat.colors(length(outbreak_size_students)))
  }
  
  # df <- as.data.frame(cbind(external_prob, outbreak_prob=sapply(1:max, function(x) sum(outbreak_size_students[[x]]+outbreak_size_teachers[[x]]>0 & (unlist(outbreak_external_st[[x]])>0 | unlist(outbreak_external_teach[[x]])>0))/length(outbreak_size_students[[x]]))))
  df <- as.data.frame(cbind(external_prob, outbreak_prob=sapply(1:max, function(x) sum(outbreak_size_students[[x]]+outbreak_size_teachers[[x]]>0)/length(outbreak_size_students[[x]]))))
  # df$external_prob <- as.numeric(ceiling(df$external_prob))
  plot <- ggplot(df, aes(x=as.numeric(external_prob), y=outbreak_prob, fill=external_prob)) + 
            geom_bar(stat="identity", position="dodge") + 
            labs(y="Outbreak probability", x="Mean number of cases introduced from community (per month, per school)") + 
            scale_fill_gradient(low = "#f19ac0", high = "#4c1130", na.value = NA) + 
            scale_x_continuous(breaks=df$external_prob, labels=ceiling(df$external_prob*944*28)) +
            # scale_x_continuous(breaks=ceiling(df$external_prob*944*28), labels=ceiling(df$external_prob*944*28)) +
            theme_publication() + 
            theme(axis.text.x = element_text(size=20,angle=45,hjust=1.0),
                  legend.position = "none")
  ggsave(plot, file=paste0(figuresPath, "outbreak_prob_",suffix, occup_suffix,"_plot.pdf"), 
         width=16, height=9)
  return(list(data=df, plot=plot))
}

plot.pirate.outbreak.size <- function(outbreak_size, 
                                      scenarios, 
                                      permutation=NULL,
                                      iter=1, 
                                      figuresPath="/figures/low_external_foi/", 
                                      suffix="students",
                                      occup_suffix="half_occup",
                                      title="STUDENTS",
                                      title_y="Total outbreak size", 
                                      colors=NULL, 
                                      mean=T){
  if(length(permutation)>0){
    outbreak_size <- outbreak_size[permutation]
    colors <- colors_scenarios[permutation]
  }else{
    colors <- rev(heat.colors(length(outbreak_size)))
  }
  
  df_inf <- data.frame(matrix(0, nrow=iter, ncol=length(scenarios)))
  colnames(df_inf) <- scenarios
  for(i in 1:length(scenarios)){
    df_inf[,i] <- outbreak_size[[i]]
  }
  inf <- reshape2::melt(df_inf)
  inf$value <- as.numeric(inf$value)
  
  if(mean){
    value <- apply(df_inf, 2, function(x) mean(x))
  }else value <- apply(df_inf, 2, function(x) median(x))
  ci_lower <- apply(df_inf, 2, function(x) quantile(x, probs=0.025))
  ci_upper <- apply(df_inf, 2, function(x) quantile(x, probs=0.975))
  df_plot <- cbind(value, ci_lower, ci_upper)
  df_plot <- as.data.frame(cbind(variable = rownames(df_plot), df_plot))
  rownames(df_plot) <- NULL
  df_plot[,-1] <- apply(df_plot[,-1], 2, function(x) as.numeric(x))
  
  plot <- ggplot(data = inf, aes(x = variable, y = value, fill=variable, color=variable))+
            geom_bar(position="dodge", stat = "summary", fun = "mean", width=0.9) +
            geom_violin(alpha=0.2, color="darkgrey")+
            # geom_jitter(shape = 1, width = .1, color="black", alpha=0.3) +
            geom_errorbar(data=df_plot, aes(y=value,ymin=ci_lower,ymax=ci_upper),lty=1, lwd=0.9, width=0.05, color="black")+
            labs(y=title_y) + 
            scale_colour_manual(values=colors) + 
            scale_fill_manual(values=colors) +
            scale_x_discrete(labels=scenarios_plot) + 
            theme_publication() + 
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_text(size=24, face="bold"),
                  axis.text.y = element_text(size=24),
                  axis.title.y = element_text(size=24),
                  legend.position = "none")
  ggsave(plot, file=paste0(figuresPath, "outbreak_size_",suffix, occup_suffix,"_plot.pdf"), 
         width=16, height=9)
  df_plot[,-1] <- apply(df_plot[,-1], 2, function(x) round(x,1))
  return(list(plot=plot, df=df_plot))
}


# ---------------------------------------------------------------------------- #
# Outbreak size 
# ---------------------------------------------------------------------------- #
plot.outbreak.size <- function(outbreak_size, 
                               scenarios, 
                               permutation=NULL,
                               iter=1, 
                               figuresPath="../results/new_results/", 
                               suffix="students",
                               occup_suffix="half_occup",
                               title="STUDENTS",
                               title_y="Total outbreak size", 
                               colors=NULL){
  if(length(permutation)>0){
    outbreak_size <- outbreak_size[permutation]
    colors <- colors[permutation]
  }else{
    colors <- rev(heat.colors(length(outbreak_size)))
  }
  
  df_inf <- data.frame(matrix(0, nrow=iter, ncol=length(scenarios)))
  colnames(df_inf) <- scenarios
  for(i in 1:length(scenarios)){
    df_inf[,i] <- c(outbreak_size[[i]], rep(NA, iter-length(outbreak_size[[i]])))
  }
  inf <- reshape2::melt(df_inf)
  inf$value <- as.numeric(inf$value)
  
  inf_plot <- ggplot(inf, aes(x=variable, y=value, fill=variable)) + 
                geom_violin(width=1.0) + 
                geom_boxplot(width=0.1, fill="white") + 
                labs(y=title_y, title = title) + 
                scale_fill_manual(values=colors) +
                scale_x_discrete(labels=scenarios) + 
                theme_publication() + 
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      legend.title = element_blank(),
                      legend.text = element_text(size=24),
                      legend.position = "right")
  ggsave(inf_plot, file=paste0(figuresPath, "outbreak_", suffix, occup_suffix,"_plot.pdf"), 
         width=22, height=9)
  return(list(data=inf,
              plot = inf_plot, 
              file_name = paste0(figuresPath, "outbreak_", suffix, occup_suffix,"_plot.pdf")))
}


plot.mean.line.outbreak.size <- function(outbreak_size, 
                                    scenarios, 
                                    external_prob,
                                    permutation=NULL,
                                    iter=1, 
                                    figuresPath="/figures/low_external_foi/", 
                                    suffix="students",
                                    occup_suffix="half_occup",
                                    title="STUDENTS",
                                    title_y="Total outbreak size", 
                                    colors=NULL,
                                    mean=T){
  if(length(permutation)>0){
    outbreak_size <- outbreak_size[permutation]
    colors <- colors_scenarios[permutation]
  }else colors <- rev(heat.colors(length(outbreak_size)))
  
  if(mean) fun_plot <- ifelse(mean, "mean", "median")
  
  df_inf <- data.frame(matrix(0, nrow=iter, ncol=length(scenarios)))
  colnames(df_inf) <- scenarios
  for(i in 1:length(scenarios)){
    df_inf[,i] <- outbreak_size[[i]]
  }
  inf <- reshape2::melt(df_inf)
  inf$value <- as.numeric(inf$value)
  if(mean) inf_summary <- inf%>%group_by(variable)%>%summarize(value=mean(value))
  else inf_summary <- inf%>%group_by(variable)%>%summarize(value=median(value))
  
  inf_plot <- ggplot(inf_summary, aes(x=variable, y=value, group=1)) +
              geom_point() + 
              geom_line() + 
              labs(y=title_y, title = title) + 
              scale_fill_manual(values=colors) +
              scale_x_discrete(labels=external_prob) + 
              theme_publication() + 
              theme(axis.title.x = element_blank(),
                    axis.text.x = element_text(size=20,angle=45,hjust=1.0),
                    legend.position = "none")
  inf_plot
  ggsave(inf_plot, file=paste0(figuresPath, "outbreak_line_",suffix, occup_suffix,"_plot.pdf"), 
         width=16, height=9)
  return(inf_plot)
}



plot.mean.outbreak.size <- function(outbreak_size, 
                               scenarios, 
                               permutation=NULL,
                               iter=1, 
                               figuresPath="/figures/low_external_foi/", 
                               suffix="students",
                               occup_suffix="half_occup",
                               title="STUDENTS",
                               title_y="Total outbreak size", 
                               colors=NULL,
                               mean=T){
  if(length(permutation)>0){
    outbreak_size <- outbreak_size[permutation]
    colors <- colors_scenarios[permutation]
  }else colors <- rev(heat.colors(length(outbreak_size)))
  
  if(mean) fun_plot <- ifelse(mean, "mean", "median")
  
  df_inf <- data.frame(matrix(0, nrow=iter, ncol=length(scenarios)))
  colnames(df_inf) <- scenarios
  for(i in 1:length(scenarios)){
    df_inf[,i] <- outbreak_size[[i]]
  }
  inf <- reshape2::melt(df_inf)
  inf$value <- as.numeric(inf$value)
  
  inf_plot <- ggplot(inf, aes(x=variable, y=value, fill=variable)) + 
    geom_bar(position="dodge", stat = "summary", fun = fun_plot) + 
    labs(y=title_y, title = title) + 
    scale_fill_manual(values=colors) + 
    theme_publication() + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size=20,angle=45,hjust=1.0),
          legend.position = "none")
  ggsave(inf_plot, file=paste0(figuresPath, "outbreak_",suffix, occup_suffix,"_plot.pdf"), 
         width=16, height=9)
  return(inf_plot)
}


plot.mean.error.outbreak.size <- function(outbreak_size, 
                                    scenarios, 
                                    permutation=NULL,
                                    iter=1, 
                                    figuresPath="/figures/low_external_foi/", 
                                    suffix="students",
                                    occup_suffix="half_occup",
                                    title="STUDENTS",
                                    title_y="Total outbreak size", 
                                    colors=NULL,
                                    mean=T){
  if(length(permutation)>0){
    outbreak_size <- outbreak_size[permutation]
    colors <- colors_scenarios[permutation]
  }else{
    colors <- rev(heat.colors(length(outbreak_size)))
  }
  
  df_inf <- data.frame(matrix(0, nrow=iter, ncol=length(scenarios)))
  colnames(df_inf) <- scenarios
  for(i in 1:length(scenarios)){
    df_inf[,i] <- outbreak_size[[i]]
  }
  inf <- reshape2::melt(df_inf)
  inf$value <- as.numeric(inf$value)
  
  if(mean) value <- apply(df_inf, 2, function(x) mean(x))
  else value <- apply(df_inf, 2, function(x) median(x))
  ci_lower <- apply(df_inf, 2, function(x) quantile(x, probs=0.025))
  ci_upper <- apply(df_inf, 2, function(x) quantile(x, probs=0.975))
  df_plot <- cbind(value, ci_lower, ci_upper)
  df_plot <- as.data.frame(cbind(scenario = rownames(df_plot), df_plot))
  rownames(df_plot) <- NULL
  df_plot[,-1] <- apply(df_plot[,-1], 2,as.numeric)

  inf_plot <- ggplot(df_plot, aes(x=scenario, fill=scenario)) + 
    geom_bar(position="dodge", stat = "identity", aes(y=value)) + 
    geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper), width=0.15, position = position_dodge(.9), 
                  color="black")+
    labs(y=title_y, title = title) + 
    scale_x_discrete(labels=scenarios_plot) +
    scale_fill_manual(values=colors) + 
    theme_publication() + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size=24),
          axis.text.x = element_text(size=16,face="bold"),
          legend.position = "none")
  ggsave(inf_plot, file=paste0(figuresPath, "outbreak_barplot_",suffix, occup_suffix,"_plot.pdf"), 
         width=20, height=12)
  return(inf_plot)
}

# ---------------------------------------------------------------------------- #
# Outbreak size per introduction case
# ---------------------------------------------------------------------------- #
plot.outbreak.size.per.case <- function(outbreak_data, 
                                        scenarios, 
                                        figuresPath="/figures/low_external_foi/", 
                                        suffix="students",
                                        occup_suffix="half_occup",
                                        title="STUDENTS", 
                                        title_y="Mean outbreak size per introduction"){
  names(outbreak_data) <- scenarios
  df_outbreak <- reshape2::melt(lapply(outbreak_data, function(x) unlist(lapply(x, function(y) mean(y, na.rm=T)))))
  df_outbreak <- as.data.frame(df_outbreak)
  colnames(df_outbreak) <- c("value", "scenario")
  df_outbreak$scenario <- factor(df_outbreak$scenario, levels=names(outbreak_data))
  
  plot <- ggplot(df_outbreak, aes(y=value, x=scenario, fill=scenario)) + 
            geom_violin(width=1.0) + 
            geom_boxplot(width=0.1, fill="white") + 
            labs(y=title_y, title=title) + 
            scale_fill_manual(values=colors_scenarios[permutation]) + 
            theme_publication() + 
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_text(angle=45, hjust=1, size=18),
                  legend.position = "none")
  plot
  ggsave(plot, file=paste0(figuresPath, "outbreak_size_per_case_", suffix, occup_suffix, "_plot.pdf"), width=16, height=9)
  return(plot)
}

# ---------------------------------------------------------------------------- #
# Scatterplot outbreak size vs introductions from community
# ---------------------------------------------------------------------------- #
plot.outbreak.size.vs.external <- function(outbreak_size, 
                                           outbreak_external,
                                           scenarios, 
                                           permutation=c(1,2,3,4,5,6), 
                                           figuresPath="/figures/sim/", 
                                           suffix="students",
                                           occup_suffix="half_occup",
                                           title="STUDENTS",
                                           title_y="Number of scool-related infected", 
                                           colors=colors_scenarios){
  if(!is.null(permutation)){
    colors <- colors_scenarios[permutation]
  }else{
    colors <- rev(heat.colors(length(outbreak_size)))
  }
  names(outbreak_size) <- scenarios
  names(outbreak_external) <- scenarios
  df <- cbind(melt(outbreak_size)[2], melt(outbreak_size)[1], melt(outbreak_external)[2])
  colnames(df) <- c("scenario", "outbreak_size", "external")
  df <- as.data.frame(df)
  df$scenario <- factor(df$scenario, levels=scenarios)
  plot <- ggplot(df, aes(x=external,y=outbreak_size, color=scenario)) + 
    facet_wrap(~scenario) + 
    # geom_point(shape=21,fill="white",size=1) + 
    geom_point() + 
    geom_abline(slope=1, intercept=0) + 
    labs(y = paste(title_y, suffix), 
         x="Total number of introductions from community",
         title=title, colour="Strategies") + 
    scale_x_continuous(limits=c(0, max(df$outbreak_size))) +
    scale_color_manual(values=colors) +
    theme_publication() + 
    theme(legend.position = "none", 
          strip.text = element_text(size=14))
  plot
  ggsave(plot, file=paste0(figuresPath, "outbreak_size_vs_external_", suffix, occup_suffix,"_plot.pdf"), 
         width=12, height=9)
  return(plot)
}




# ---------------------------------------------------------------------------- #
# Reduction of outbreak size with respect to first scenario in list
# ---------------------------------------------------------------------------- #
plot.red.outbreak.size <- function(outbreak_size, 
                                   scenarios, 
                                   permutation=NULL,
                                   figuresPath="/figures/low_external_foi/", 
                                   suffix="students",
                                   occup_suffix="half_occup",
                                   title="STUDENTS",
                                   title_y="Total outbreak size",
                                   colors=colors_scenarios){
  if(length(permutation)>0){
    outbreak_size <- outbreak_size[permutation]
    colors <- colors_scenarios[permutation][-1]
  }else{
    colors <- colors[-1]
  }
  mean_outbreak_size <- unlist(lapply(outbreak_size, mean))
  red_os <- (mean_outbreak_size-mean_outbreak_size[1])/mean_outbreak_size[1]
  
  df_red_os <- as.data.frame(cbind(scenario=scenarios, value=red_os))
  df_red_os$scenario <- factor(df_red_os$scenario, levels=scenarios)
  df_red_os <- df_red_os[-1,]
  
  plot <- ggplot(df_red_os, aes(x=scenario, y=100*as.numeric(value), fill=scenario)) + 
            geom_bar(stat="identity") + 
            labs(y=paste("Relative change in mean number of school-related \ninfected",suffix,"(%, compared to full occupancy)")) + 
            scale_fill_manual(values=colors) + 
            scale_x_discrete(labels=scenarios_plot[-1]) + 
            theme_publication() + 
            theme(legend.position = "none",
                  axis.title.x = element_blank(), 
                  axis.text.x = element_text(size=18,face="bold"))
  plot
  ggsave(plot, file=paste0(figuresPath, "red_outbreak_size_",suffix, occup_suffix,"_plot.pdf"), 
         width=16, height=9)
  df_red_os$value <- round(as.numeric(df_red_os$value)*100, 2)
  return(list(plot=plot, df_red_os=df_red_os))
}


# ---------------------------------------------------------------------------- #
# Additional outbreak size with respect to first scenario in list
# ---------------------------------------------------------------------------- #
plot.add.outbreak.size <- function(outbreak_size, 
                                   scenarios, 
                                   permutation=NULL,
                                   figuresPath="/figures/low_external_foi/", 
                                   suffix="students",
                                   occup_suffix="half_occup",
                                   title="STUDENTS",
                                   title_y="Total outbreak size", 
                                   colors=colors_scenarios,
                                   mean=T){
  if(length(permutation)>0){
    outbreak_size <- outbreak_size[permutation]
    colors <- colors_scenarios[permutation][-1]
  }else{
    colors <- colors[-1]
  }
  if(mean){
    mean_outbreak_size <- unlist(lapply(outbreak_size, mean))
    title.add <- "mean"
  }else{
    mean_outbreak_size <- unlist(lapply(outbreak_size, median))
    title.add <- "median"
  }
  add_os <- (mean_outbreak_size-mean_outbreak_size[1])*1000
  
  df_add_os <- as.data.frame(cbind(scenario=scenarios, value=add_os))
  df_add_os$scenario <- factor(df_add_os$scenario, levels=scenarios)
  df_add_os <- df_add_os[-1,]
  
  plot <- ggplot(df_add_os, aes(x=scenario, y=as.numeric(value), fill=scenario)) + 
    geom_bar(stat="identity") + 
    labs(y=paste("Absolute change in", title.add,  "number of school-related\ninfected", suffix, "(comp. to full occupancy, for 1000 schools)")) + 
    scale_fill_manual(values=colors) + 
    scale_x_discrete(labels=scenarios_plot[-1]) + 
    theme_publication() + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(size=18, face="bold"),
          # axis.title.y = element_text(size=20),
          legend.position = "none")
  plot
  ggsave(plot, file=paste0(figuresPath, "add_outbreak_size_",suffix, occup_suffix,"_plot.pdf"), 
         width=16, height=9)
  return(list(plot=plot, add_os=add_os))
}

# ---------------------------------------------------------------------------- #
# Plot number of symptomatic cases 
# ---------------------------------------------------------------------------- #
plot.symptomatic <- function(symptomatic, 
                             scenarios, 
                             permutation=NULL, 
                             figuresPath, 
                             suffix="combined"){
  if(length(permutation)>0){
    symptomatic <- symptomatic[permutation]
    colors <- colors_scenarios[permutation]
  }else colors <- rainbow(length(symptomatic))
  names(symptomatic) <- scenarios
  df_symptomatic <- melt(symptomatic)
  names(df_symptomatic) <- c("value", "scenario")
  df_symptomatic$scenario <- factor(df_symptomatic$scenario, levels=scenarios)
  df_symptomatic$value <- as.numeric(df_symptomatic$value)
  
  plot <- ggplot(df_symptomatic, aes(x=scenario, y=value, fill=scenario)) + 
            geom_violin(width=1.0) + 
            geom_boxplot(width=0.1, fill="white") + 
            labs(y=paste("Number of symptomatic",suffix)) + 
            scale_fill_manual(values=colors) + 
            scale_y_continuous(breaks=seq(0,max(df_symptomatic$value), by=15))+
            scale_x_discrete(labels=scenarios_plot) + 
            theme_publication() + 
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_text(size=18, face="bold"),
                  axis.ticks.x = element_blank(),
                  legend.position = "none")
  plot
  ggsave(plot, file=paste0(figuresPath, "symptomatic_",suffix,"_plot.pdf"), 
         width=16, height=9)
  return(plot)
}


# ---------------------------------------------------------------------------- #
# Plot number of symptomatic cases 
# ---------------------------------------------------------------------------- #
plot.new.symp <- function(symp, 
                          data=c(2.3, 4.6, 0.7, 2.7), 
                          time_steps, 
                          figuresPath, 
                          suffix=""){
  week1 <- apply(symp, 2, function(x) max(x[time_steps<=7])-min(x[time_steps<=7]))
  week2 <- apply(symp, 2, function(x) max(x[time_steps>7 & time_steps<=14])-min(x[time_steps>7 & time_steps<=14]))
  week3 <- apply(symp, 2, function(x) max(x[time_steps>14 & time_steps<=21])-min(x[time_steps>14 & time_steps<=21]))
  week4 <- apply(symp, 2, function(x) max(x[time_steps>21 & time_steps<=28])-min(x[time_steps>21 & time_steps<=28]))
  
  df_symp <- cbind(week1, week2, week3, week4, sum=week1+week2+week3+week4)
  median <- apply(df_symp, 2, mean)
  ci_lower <- apply(df_symp, 2, function(x) quantile(x, probs=0.025))
  ci_upper <- apply(df_symp, 2, function(x) quantile(x, probs=0.975))
  
  symp <- cbind(c("Week 1", "Week 2", "Week 3", "Week 4", "Total"), melt(median), melt(ci_lower), melt(ci_upper))
  colnames(symp) <- c("week", "median", "ci_lower", "ci_upper")
  symp <- cbind(symp, c(data, sum(data)))
  colnames(symp) <- c("week", "median", "ci_lower", "ci_upper", "data")
  symp <- reshape::melt(symp)
  symp <- cbind(symp, group=c(rep("Number of simulated symptomatic infections",5*3), rep("Number of notified positive cases in pilot study", 5)))
  symp$ci_lower[symp$variable=="ci_lower"] <- symp$value[symp$variable=="ci_lower"]
  symp$ci_upper[symp$variable=="ci_lower"] <- symp$value[symp$variable=="ci_upper"]
  symp$ci_lower[!symp$variable%in%c("ci_lower","ci_upper")] <- symp[!symp$variable%in%c("ci_lower","ci_upper"),"value"]
  symp$ci_upper[!symp$variable%in%c("ci_lower","ci_upper")] <- symp[!symp$variable%in%c("ci_lower","ci_upper"),"value"]
  symp$agent <- suffix
  
  plot <- ggplot(symp, aes(x=week, y=value, shape=group, color=group)) + 
            geom_point(data=symp[symp$variable=="median",], size=3) + 
            geom_point(data=symp[symp$variable=="data",], size=5) + 
            geom_linerange(aes(ymin=ci_lower, ymax=ci_upper)) + 
            scale_color_manual(values = c("black",colors_scenarios[4])) + 
            scale_shape_manual(values = c(18,19)) + 
            labs(y=paste("Number of",suffix)) + 
            theme_publication() + 
            theme(axis.title.x=element_blank(),
                  legend.position = c(.97, .95),
                  legend.justification = c("right", "top"),
                  legend.title = element_blank(),
                  legend.text = element_text(size=22),
                  legend.box.background = element_rect(color="black", size=1))
  plot
  # ggsave(plot, file="symptomatic_per_week_plot.pdf", width=16, height=9)
  ggsave(plot, file=paste0(figuresPath, "symptomatic_per_week_",suffix,"_plot.pdf"),
         width=12, height=9)
  return(list(plot=plot, symp=symp))
}


plot.new.symp.combined <- function(ns, 
                                   nt, 
                                   time_steps, 
                                   figuresPath){
  ps <- plot.new.symp(ns, data=c(2.3, 4.6, 0.7, 2.7), time_steps, figuresPath, suffix="students")
  pt <- plot.new.symp(nt, data=c(0, 0, 0.9, 1.4), time_steps, figuresPath, suffix="teachers")
  symp <- rbind(ps$symp, pt$symp)
  symp$agent[symp$agent=="students"] <- "Students"
  symp$agent[symp$agent=="teachers"] <- "Teachers"
  symp$group <- as.factor(symp$group)
  
  plot <- ggplot(symp, aes(x=week, y=value, shape=group, color=group)) + 
            facet_grid(~agent, scales="free") + 
            geom_point(data=symp[symp$variable=="data",], size=5) + 
            geom_point(data=symp[symp$variable=="median",], size=3) + 
            geom_linerange(aes(ymin=ci_lower, ymax=ci_upper)) + 
            scale_color_manual(values = c("black",colors_scenarios[4])) + 
            scale_shape_manual(values = c(18,19)) + 
            labs(y=paste("Number of individuals")) + 
            guides(col = guide_legend(nrow = 2)) + 
            theme_publication() + 
            theme(axis.title.x=element_blank(),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size=20),
                  legend.box.background = element_rect(color="black", size=1))
  plot 
  ggsave(plot, file=paste0(figuresPath, "symptomatic_per_week_plot.pdf"),
         width=16, height=9)
  return(list(plot=plot, symp=symp))
}


# ---------------------------------------------------------------------------- #
# Number of infections per agent type
# ---------------------------------------------------------------------------- #
plot.inf.per.agent <- function(infector, 
                               scenarios, 
                               permutation=NULL, 
                               figuresPath, 
                               suffix=""){
  if(length(permutation)!=0){
    infector <- infector[permutation]
    names(infector) <- scenarios
    colors_scenarios <- colors_scenarios[permutation]
  }
  inf_list <- infectors_st <- ind_case_student <- ind_case_teacher <- prop_ind_student <- prop_ind_teacher <- vector(mode="list", length=length(infector))
  inf_list<-  lapply(infector, function(x) unlist(x))
  for(k in 1:length(infector)){
    infectors_st[[k]] <- names(inf_list[[k]])
    ind_case_teacher[[k]] <- sum(inf_list[[k]][stringr::str_detect(infectors_st[[k]], "T")])
    ind_case_student[[k]] <- sum(inf_list[[k]][!stringr::str_detect(infectors_st[[k]], "T")])
    prop_ind_student[[k]] <- ind_case_student[[k]]/(ind_case_student[[k]] + ind_case_teacher[[k]])
    prop_ind_teacher[[k]] <- 1-prop_ind_student[[k]]
  }
  names(prop_ind_student) <- names(prop_ind_teacher) <- scenarios
  df_ind_student <- melt(prop_ind_student)
  df_ind_teacher <- melt(prop_ind_teacher)
  names(df_ind_student) <- names(df_ind_teacher) <- c("value", "scenario")
  df <- cbind(scenario=df_ind_student$scenario, student=df_ind_student$value, teacher=df_ind_teacher$value)
  df <- as.data.frame(df)
  df[,-1] <- apply(df[,-1], 2, as.numeric)
  df$scenario <- factor(df$scenario, levels=scenarios)
  df_plot <- melt(df)
  plot <- ggplot(df_plot, aes(x=variable, y=value, fill=scenario)) + 
            facet_wrap(~scenario) + 
            labs(y=paste0("Proportion of index cases\n(", suffix,")"))+ 
            geom_bar(stat="identity") + 
            scale_fill_manual(values=colors_scenarios) + 
            theme_publication() + 
            theme(axis.title.x = element_blank(), 
                  legend.position = "none")
  plot
  ggsave(plot, file=paste0(figuresPath, "index_case_per_agent_plot_",suffix,".pdf"), 
         width=16, height=9)
  return(plot)
}



# ---------------------------------------------------------------------------- #
# Number of infections per location
# ---------------------------------------------------------------------------- #
plot.inf.per.location <- function(outbreak_per_loc, 
                                  scenarios, 
                                  figuresPath="/figures/low_external_foi/", 
                                  suffix="students",
                                  occup_suffix="half_occup",
                                  title="STUDENTS", 
                                  title_y="Number of infections"){
  # outbreak_per_loc <- list(outbreak_per_loc[[1:length(scenarios)]])
  names(outbreak_per_loc) <- scenarios
  df_outbreak_per_loc <- melt(outbreak_per_loc)
  colnames(df_outbreak_per_loc) <- c("variable", "value", "scenario")
  df_outbreak_per_loc$scenario <- factor(df_outbreak_per_loc$scenario, levels=names(outbreak_per_loc))
  plot <- ggplot(df_outbreak_per_loc, aes(x=scenario, y=value, fill=scenario)) + 
                 geom_violin(width=1.0) + 
                 geom_boxplot(width=0.1, fill="white") + 
                 labs(y=title_y, title = title) + 
                 scale_fill_manual(values=colors_scenarios[permutation]) + 
                 theme_publication() + 
                 theme(axis.title.x = element_blank(),
                       axis.text.x = element_text(angle=45, hjust=1,size=18),
                       legend.position = "none")
  ggsave(plot, file=paste0(figuresPath, "inf_community_",suffix, occup_suffix,"_plot.pdf"), 
         width=16, height=9)
  return(plot)
}


# ---------------------------------------------------------------------------- #
# Plot absent days per individual
# ---------------------------------------------------------------------------- #
plot.abs.days <- function(abs_days, 
                          scenarios, 
                          figuresPath="/figures/low_external_foi/", 
                          suffix="student",
                          occup_suffix="half_occup",
                          title="STUDENTS", 
                          title_y="Number of absent days per student",
                          no_ind, 
                          mean=T,
                          pirate=F){
  names(abs_days) <- scenarios
  abs_days <- lapply(abs_days, function(x) x/no_ind)
  df_abs_days <- melt(abs_days)
  df_abs_days$value <- df_abs_days$value
  colnames(df_abs_days) <- c("variable", "value", "scenario")
  df_abs_days$scenario <- factor(df_abs_days$scenario, levels=names(abs_days))
  if(pirate){
    if(mean) value <- unlist(lapply(abs_days, function(x) mean(x[,"Total absent"])))
    else value <- unlist(lapply(abs_days, function(x) median(x[,"Total absent"])))
    ci_lower <- unlist(lapply(abs_days, function(x) quantile(x[,"Total absent"], probs=0.025)))
    ci_upper <- unlist(lapply(abs_days, function(x) quantile(x[,"Total absent"], probs=0.975)))
    df_plot <- cbind(value, ci_lower, ci_upper)
    df_plot <- as.data.frame(cbind(scenario = rownames(df_plot), df_plot))
    rownames(df_plot) <- NULL
    df_plot[,-1] <- apply(df_plot[,-1], 2,as.numeric)
    
    plot <- ggplot(data = df_abs_days%>%filter(variable=="Total absent"), aes(x = scenario, y = value, fill=scenario, color=scenario))+
      geom_bar(position="dodge", stat = "summary", fun = "mean", width=0.9) +
      geom_violin(alpha=0.2, color="darkgrey")+
      # geom_jitter(shape = 1, width = .1, color="black", alpha=0.3) +
      geom_errorbar(data=df_plot, aes(y=value,ymin=ci_lower,ymax=ci_upper),lty=1, lwd=0.9, width=0.05, color="black")+
      labs(y=title_y) + 
      scale_colour_manual(values=colors_scenarios) + 
      scale_fill_manual(values=colors_scenarios) +
      scale_x_discrete(labels=scenarios_plot) + 
      theme_publication() + 
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size=18,face="bold"),
            legend.position = "none")
  }else{
    plot <- ggplot(df_abs_days%>%filter(variable=="Total absent"), aes(x=scenario, y=value, fill=scenario)) + 
      geom_violin(width=1.0) + 
      geom_boxplot(width=0.1, fill="white") + 
      labs(y=title_y) + 
      scale_fill_manual(values=colors_scenarios[permutation]) + 
      scale_x_discrete(labels=scenarios_plot) + 
      theme_publication() + 
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size=18,face="bold"),
            legend.position = "none")
  }
  plot
  ggsave(plot, file=paste0(figuresPath, "absent_days_per_", suffix, occup_suffix, "_plot.pdf"),         width=16, height=9)
  return(list(plot=plot, df_abs_days=df_plot))
}


# ---------------------------------------------------------------------------- #
# Plot absent individuals
# ---------------------------------------------------------------------------- #
plot.abs <- function(absences, 
                     scenarios, 
                     figuresPath="/figures/low_external_foi/", 
                     suffix="students",
                     occup_suffix="half_occup",
                     title="STUDENTS", 
                     title_y="Number of absent students", 
                     mean=T, 
                     pirate=F){
  names(absences) <- scenarios
  df_abs <- melt(absences)
  df_abs <- df_abs[df_abs$variable=="Total absent",]
  colnames(df_abs) <- c("variable", "value", "scenario")
  df_abs$scenario <- factor(df_abs$scenario, levels=names(absences))
  
  if(pirate){
    if(mean) value <- unlist(lapply(absences, function(x) mean(x[,"Total absent"])))
    else value <- unlist(lapply(absences, function(x) median(x[,"Total absent"])))
    ci_lower <- unlist(lapply(absences, function(x) quantile(x[,"Total absent"], probs=0.025)))
    ci_upper <- unlist(lapply(absences, function(x) quantile(x[,"Total absent"], probs=0.975)))
    df_plot <- cbind(value, ci_lower, ci_upper)
    df_plot <- as.data.frame(cbind(scenario = rownames(df_plot), df_plot))
    rownames(df_plot) <- NULL
    df_plot[,-1] <- apply(df_plot[,-1], 2,as.numeric)
    
    plot <- ggplot(data = df_abs%>%filter(variable=="Total absent"), aes(x = scenario, y = value, fill=scenario, color=scenario))+
      geom_bar(position="dodge", stat = "summary", fun = "mean", width=0.9) +
      geom_violin(alpha=0.2, color="darkgrey")+
      # geom_jitter(shape = 1, width = .1, color="black", alpha=0.3) +
      geom_errorbar(data=df_plot, aes(y=value,ymin=ci_lower,ymax=ci_upper),lty=1, lwd=0.9, width=0.05, color="black")+
      labs(y=title_y) + 
      scale_colour_manual(values=colors_scenarios) + 
      scale_fill_manual(values=colors_scenarios) +
      scale_x_discrete(labels=scenarios_plot) + 
      theme_publication() + 
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(size=18,face="bold"),
            legend.position = "none")
  }else{
    plot <- ggplot(df_abs%>%filter(variable=="Total absent"), aes(x=scenario, y=value, fill=scenario)) + 
              geom_violin(width=1.0) + 
              geom_boxplot(width=0.1, fill="white") + 
              labs(y=title_y) + 
              scale_fill_manual(values=colors_scenarios[permutation]) + 
              theme_publication() + 
              theme(axis.title.x = element_blank(),
                    axis.text.x = element_text(size=18,face="bold"),
                    legend.position = "none")
  }
  plot
  ggsave(plot, file=paste0(figuresPath, "absent_", suffix, occup_suffix, "_plot.pdf"), width=16, height=9)
  return(list(plot=plot, df_absences = df_plot))
}



# ---------------------------------------------------------------------------- #
# Plot secondary cases
# ---------------------------------------------------------------------------- #
plot.sec.cases <- function(sec_cases, 
                           scenarios, 
                           figuresPath="/figures/low_external_foi/", 
                           suffix="students",
                           occup_suffix="half_occup",
                           title="STUDENTS", 
                           title_y="Number of secondary cases"){
  names(sec_cases) <- scenarios
  sec_data <- lapply(sec_cases, function(x) unlist(lapply(x, function(y) mean(y, na.rm=T))))
  df_sec <- as.data.frame(reshape2::melt(sec_data))
  colnames(df_sec) <- c("value", "scenario")
  df_sec$scenario <- factor(df_sec$scenario, levels=names(sec_cases))
  plot <- ggplot(df_sec, aes(y=value, x=scenario, fill=scenario)) + 
            geom_violin(width=1.0) + 
            geom_boxplot(width=0.1, fill="white") + 
            labs(y=title_y, title=title) + 
            scale_fill_manual(values=colors_scenarios[permutation]) + 
            theme_publication() + 
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_text(size=18,face="bold"),
                  legend.position = "none")
  plot
  ggsave(plot, file=paste0(figuresPath, "sec_cases_", suffix, occup_suffix, "_plot.pdf"), width=16, height=9)
  return(plot)
}


plot.sec.cases.combined <- function(sec_cases, 
                                     scenarios, 
                                     figuresPath="/figures/low_external_foi/", 
                                     suffix="students",
                                     occup_suffix="half_occup",
                                     title="STUDENTS", 
                                     title_y="Number of secondary cases"){
  names(sec_cases) <- scenarios
  sec_data <- lapply(sec_cases, function(x) unlist(x))
  mean_sec_data <- lapply(sec_data, function(x) mean(x))
  cl_sec_data <- lapply(sec_data, function(x) quantile(x, probs=0.025))
  cu_sec_data <- lapply(sec_data, function(x) quantile(x, probs=0.975))
  df_sec <- as.data.frame(reshape2::melt(mean_sec_data))
  colnames(df_sec) <- c("value", "scenario")
  df_sec$scenario <- factor(df_sec$scenario, levels=names(sec_cases))
  plot <- ggplot(df_sec, aes(y=value, x=scenario, fill=scenario)) + 
    # facet_wrap(~scenario) +
    # geom_violin(width=1.0) +
    geom_boxplot(width=0.1) + 
    labs(y=title_y, title=title) + 
    scale_fill_manual(values=colors_scenarios[permutation]) + 
    theme_publication() + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle=45, hjust=1, size=18),
          axis.ticks.x = element_blank(),
          legend.position = "none")
  plot
  ggsave(plot, file=paste0(figuresPath, "sec_cases_combined_", suffix, occup_suffix, "_plot.pdf"), width=16, height=9)
  return(plot)
}

prop.outbreak.external <- function(outbreak_size, 
                                   outbreak_external,
                                   scenarios, 
                                   permutation,
                                   figuresPath="/figures/sim/", 
                                   suffix="students",
                                   occup_suffix="half_occup",
                                   title="STUDENTS",
                                   title_y="Number of scool-related infected", 
                                   colors=colors_scenarios){
  
  if(length(permutation)!=0){
    outbreak_size <- outbreak_size[permutation]
    outbreak_external <- outbreak_external[permutation]
    colors <- colors_scenarios[permutation]
  }else colors <- "grey"
  df_ratio <- lapply(1:length(outbreak_size), function(x) outbreak_size[[x]]/outbreak_external[[x]])
  for(i in 1:length(outbreak_size)) df_ratio[[i]][is.na(df_ratio[[i]][,1])| is.infinite(df_ratio[[i]][,1]),1] <- 0
  df_prop <- lapply(1:length(outbreak_size), function(x) 1-sum(df_ratio[[x]]<=1)/nrow(df_ratio[[x]]))
  names(df_prop) <- scenarios
  df_plot <- melt(df_prop)
  names(df_plot) <- c("value", "scenario")
  df_plot$scenario <- factor(df_plot$scenario, levels=scenarios)
  plot <- ggplot(df_plot, aes(x=scenario, y=value, fill=scenario)) + 
          geom_bar(stat="identity") + 
          theme_publication() + 
          labs(y="Probability that outbreak size >= number of introductions") + 
          scale_x_discrete(labels=scenarios_plot) + 
          scale_fill_manual(values=colors)+
          theme(legend.position = "none",
                axis.title.x=element_blank(),
                axis.title.y = element_text(size=20),
                axis.text.y = element_text(size=16),
                axis.text.x = element_text(size=18, face="bold"))
  plot
  ggsave(plot, file=paste0(figuresPath, "prop_outbreak_size_external_", suffix, occup_suffix, "_plot.pdf"), width=16, height=9)
  df_plot[,1] <- round(df_plot[,1],2)
  df_plot <- df_plot[,c("scenario","value")]
  return(list(plot, df_plot))
}


prop.outbreak.external.range <- function(outbreak_size, 
                                         outbreak_external,
                                         scenarios, 
                                         permutation,
                                         figuresPath="/figures/sim/", 
                                         suffix="students",
                                         occup_suffix="half_occup",
                                         title="STUDENTS",
                                         title_y="Number of scool-related infected", 
                                         colors=colors_scenarios){
  
  if(length(permutation)!=0){
    outbreak_size <- outbreak_size[permutation]
    outbreak_external <- outbreak_external[permutation]
    colors <- colors_scenarios[permutation]
  }else colors <- rep("darkgrey",length(outbreak_size))
  df_ratio <- lapply(1:length(outbreak_size), function(x) outbreak_size[[x]]/outbreak_external[[x]])
  for(i in 1:length(outbreak_size)) df_ratio[[i]][is.na(df_ratio[[i]][,1])| is.infinite(df_ratio[[i]][,1]),1] <- 0
  df_prop <- lapply(1:length(outbreak_size), function(x) sum(df_ratio[[x]]<=1)/nrow(df_ratio[[x]]))
  names(df_prop) <- scenarios
  df_plot <- melt(df_prop)
  names(df_plot) <- c("value", "scenario")
  df_plot$scenario <- factor(df_plot$scenario, levels=scenarios)
  x_axis_label <- ceiling(scenarios_plot*944*28)
  plot <- ggplot(df_plot, aes(x=scenario, y=1-value, fill=scenario)) + 
    geom_bar(stat="identity") + 
    theme_publication() + 
    labs(y="Probability that student outbreak size > number of introductions",
         x="Mean number of cases introduced from community (per month, per school)") + 
    scale_x_discrete(labels=x_axis_label) + 
    scale_y_continuous(breaks=seq(0,1,by=0.1)) + 
    scale_fill_manual(values=rep("#be3679", length(external_prob)))+
    theme(legend.position = "none",
          axis.title = element_text(size=20),
          axis.text.y = element_text(size=16),
          axis.text.x = element_text(size=16))
  plot
  ggsave(plot, file=paste0(figuresPath, "prop_outbreak_size_external_", suffix, occup_suffix, "_plot.pdf"), width=16, height=9)
  return(list(plot, df_plot))
}

ratio.outbreak.external <- function(outbreak_size, 
                                    outbreak_external,
                                    scenarios, 
                                    permutation,
                                    figuresPath="/figures/sim/", 
                                    suffix="students",
                                    occup_suffix="half_occup",
                                    title="STUDENTS",
                                    title_y="Number of scool-related infected", 
                                    colors=colors_scenarios){
  
  if(length(permutation)!=0){
    outbreak_size <- outbreak_size[permutation]
    outbreak_external <- outbreak_external[permutation]
    colors <- colors_scenarios[permutation]
  }
  df_ratio <- lapply(1:length(outbreak_size), function(x) outbreak_size[[x]]/outbreak_external[[x]])
  df_prop <- df_ratio
  names(df_prop) <- scenarios
  df_plot <- melt(df_prop)
  names(df_plot) <- c("variable","value", "scenario")
  df_plot$scenario <- factor(df_plot$scenario, levels=scenarios)
  plot <- ggplot(df_plot, aes(x=scenario, y=value, fill=scenario)) + 
    geom_violin(width=1.0) +
    geom_boxplot(width=0.1, fill="white") + 
    theme_publication() + 
    labs(y="Ratio of outbreak size vs\nnumber of community introductions") + 
    scale_fill_manual(values=colors_scenarios[permutation]) + 
    theme(legend.position = "none",
          axis.title.x = element_blank(), 
          axis.title.y = element_text(size=18),
          axis.text.y = element_text(size=16),
          axis.text.x = element_text(size=16,angle=45,hjust=1.0))
  plot
  ggsave(plot, file=paste0(figuresPath, "ratio_outbreak_size_external_", suffix, occup_suffix, "_plot.pdf"), width=16, height=9)
  return(plot)
}

roi.outbreak.external <- function(outbreak_size, 
                                  outbreak_external,
                                  scenarios, 
                                  permutation,
                                  figuresPath="/figures/sim/", 
                                  suffix="students",
                                  occup_suffix="half_occup",
                                  title="STUDENTS",
                                  title_y="Number of scool-related infected", 
                                  colors=colors_scenarios){
  
  if(length(permutation)!=0){
    outbreak_size <- outbreak_size[permutation]
    outbreak_external <- outbreak_external[permutation]
    colors <- colors_scenarios[permutation]
  }
  df_roi <- lapply(1:length(outbreak_size), function(x) (outbreak_size[[x]]-outbreak_external[[x]])/outbreak_external[[x]])
  df_prop <- lapply(1:length(outbreak_size), function(x) sum(df_roi[[x]]>=1)/nrow(df_roi[[x]]))
  names(df_prop) <- scenarios
  df_plot <- melt(df_prop)
  names(df_plot) <- c("value", "scenario")
  df_plot$scenario <- factor(df_plot$scenario, levels=scenarios)
  plot <- ggplot(df_plot, aes(x=scenario, y=value, fill=scenario)) + 
    geom_bar(stat="identity") + 
    theme_publication() + 
    labs(y="Proportion of simulations where ROI>=1") + 
    scale_fill_manual(values=colors_scenarios[permutation]) + 
    theme(legend.position = "none",
          axis.title.x = element_blank(), 
          axis.title.y = element_text(size=18),
          axis.text.y = element_text(size=16),
          axis.text.x = element_text(size=16,angle=45,hjust=1.0))
  plot
  ggsave(plot, file=paste0(figuresPath, "roi_outbreak_size_external_", suffix, occup_suffix, "_plot.pdf"), width=16, height=9)
  return(plot)
}

