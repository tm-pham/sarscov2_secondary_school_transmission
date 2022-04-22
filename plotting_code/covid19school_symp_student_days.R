# ============================================================================ #
# Symptomatic infections
# ============================================================================ #
rm(list=ls())
path <- "/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/"
setwd(paste0(path, "code/"))
source(paste0(path, "code/plotting_code/covid19school_plotting_functions.R"))
source(paste0(path, "code/plotting_code/covid19school_omicron_scenarios.R"))
# ============================================================================ #
# Symptomatic student days
# ============================================================================ #
n_list <- n_students_list
n_per_day_list <- vector(mode="list", length=length(n_list))
type = 3
for(i in 1:length(n_list)){
  if(length(type)>1) temp <- Reduce('+', n_list[[i]][[1]][type])
  else temp <- n_list[[i]][[1]][[type]]
  n_per_day_list[[i]] <-  as.data.frame(cbind(time=1:nrow(n_list[[i]][[1]][[type[1]]]), temp))
  for(j in 1:ncol(n_per_day_list[[i]])){
    n_per_day_list[[i]][which(n_per_day_list[[i]][,j]<0), j] <- 0
  }
}

symp_student_days_list <- lapply(n_per_day_list, function(x) lapply(x[-1], function(y) sum(y)))
symp_student_days <- lapply(symp_student_days_list, unlist)
lapply(symp_student_days, summary)

ind <- c(1,2,3,4,5,9,10,7,8,12,13,14,15,17)
(moss <- plot.outbreak.size(symp_student_days[ind], 
                            scenarios=scenarios[ind], 
                            permutation = seq(1:length(scenarios)),
                            iter=101,
                            figuresPath=outputPath, 
                            suffix="total_symp_students",
                            occup_suffix="",
                            title="",
                            title_y="Total number of symptomatic student days",
                            colors=scenario_colors[ind]))
# ============================================================================ #
# Calculate reduction in symptomatic student days with respect to the baseline 
# ============================================================================ #
# Bootstrapping
ind <- c(2,3,4,5,9,10,7,8,12,13,14,15,16,17)
effect <- vector(mode="list", length=length(ind))
for(i in 1:(length(ind))){
  effect[[i]] <- rep(NA, 100)
  for(k in 1:10000){
    baseline <- sample(symp_student_days[[1]], 1)
    intervention <- sample(symp_student_days[[ind[i]]], 1)
    effect[[i]][k] <- 100*(baseline-intervention)/baseline
  }
}

# ============================================================================ #
# Plot "intervention effect"
# Simulation scenarios
# ============================================================================ #
ind_sim <- c(2,3,4,5,9,10,7,8)
(eff_plot_1 <- plot.outbreak.size(effect[1:length(ind_sim)], 
                            scenarios=scenarios[ind_sim], 
                            permutation = seq(1:length(scenarios)),
                            iter=10000,
                            figuresPath=outputPath, 
                            suffix="red_total_symp_students",
                            occup_suffix="",
                            title="",
                            title_y="Reduction in number of symptomatic student days\n(%, compared to baseline scenario)",
                            colors=scenario_colors[ind_sim]))
ggsave(eff_plot_1$plot, file="../figures/fig_red_symp_student_days_sim_scenarios.pdf",
       width=20, height=9)


# ============================================================================ #
# Plot "intervention effect"
# Intervention scenarios
# ============================================================================ #
ind_interv <- c(12,13,14,15,17)
(eff_plot_2 <- plot.outbreak.size(effect[ind_interv-3], 
                            scenarios=scenarios[ind_interv], 
                            permutation = seq(1:length(scenarios)),
                            iter=10000,
                            figuresPath=outputPath, 
                            suffix="red_total_symp_students",
                            occup_suffix="",
                            title="",
                            title_y="Reduction in number of symptomatic student days\n(%, compared to baseline scenario)",
                            colors=scenario_colors[ind_interv]))
ggsave(eff_plot_2$plot, file="../figures/fig_red_symp_student_days_interv_scenarios.pdf",
       width=20, height=9)


eff_plot <- cowplot::plot_grid(eff_plot_1$plot, eff_plot_2$plot, 
                               nrow=2, align="v", 
                               labels="AUTO", label_size=30)
ggsave(eff_plot, file= "../figures/fig_red_symp_student_days_combined.pdf", 
       width=20, height=18)

# ============================================================================ #
# Remove first part of the year without when booster campaign hasn't started yet
# ============================================================================ #
# Number of days to be removed
remove_days <- 1:242
n_per_day_list_2 <- lapply(n_per_day_list, function(x) x[-c(1:242),])

symp_student_days_list_2 <- lapply(n_per_day_list_2, function(x) lapply(x[-1], function(y) sum(y)))
symp_student_days_2 <- lapply(symp_student_days_list_2, unlist)
lapply(symp_student_days_2, summary)

# Bootstrapping
effect_2 <- vector(mode="list", length=length(ind))
for(i in 1:(length(ind))){
  effect_2[[i]] <- rep(NA, 100)
  for(k in 1:10000){
    baseline <- sample(symp_student_days_2[[1]], 1)
    intervention <- sample(symp_student_days_2[[ind[i]]], 1)
    effect_2[[i]][k] <- 100*(baseline-intervention)/baseline
  }
}


# ============================================================================ #
# Plot "intervention effect"
# Intervention scenarios
# ============================================================================ #
ind_interv <- c(12,13,14,15,17)
(eff_plot_2 <- plot.outbreak.size(effect_2[ind_interv-3], 
                                  scenarios=scenarios[ind_interv], 
                                  permutation = seq(1:length(scenarios)),
                                  iter=10000,
                                  figuresPath=outputPath, 
                                  suffix="red_total_symp_students",
                                  occup_suffix="",
                                  title="",
                                  title_y="Reduction in number of symptomatic student days\nafter 1st September 2022\n(%, compared to baseline scenario)",
                                  colors=scenario_colors[ind_interv]))
ggsave(eff_plot_2$plot, file="../figures/figure_red_symp_student_days_interv_scenarios.pdf",
       width=20, height=9)

eff_plot <- cowplot::plot_grid(eff_plot_1$plot, eff_plot_2$plot, 
                               nrow=2, align="v", 
                               labels="AUTO", label_size=30)
ggsave(eff_plot, file= "../figures/Figure8.pdf", 
       width=26, height=18)

# ============================================================================ #
# Output in table
# ============================================================================ #
summary <- rbind(eff_plot_1$data, eff_plot_2$data) %>% group_by(variable) %>% summarize(mean=mean(value), 
                                                                                        median=median(value), 
                                                                                        ci_lower=quantile(value, probs=0.025),
                                                                                        ci_upper=quantile(value, probs=0.975))
write.csv(summary, file=paste0(outputPath, "summary_red_symp_student_days.csv"), 
          row.names = F)

