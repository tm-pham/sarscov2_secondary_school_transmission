# ============================================================================ #
# Figures 1-5 for main text
# ============================================================================ #
rm(list=ls())
source(paste0("plotting_code/covid19school_plotting_functions.R"))
source(paste0("plotting_code/covid19school_omicron_scenarios.R"))

figurePath <- "figures/main_text/"
panel_label_size = 30

# ---------------------------------------------------------------------------- #
# Figure 2: Baseline plots
# ---------------------------------------------------------------------------- #
# A: Total number of infections
# B: Symptomatically infected 
# C: Students at risk (weighted by susceptibility)
# D: Students absent due to isolation and quarantine
# ---------------------------------------------------------------------------- #
y_max = 0.36*100
baseline_plot <- cowplot::plot_grid(inf_students_5_plot + 
                                      scale_y_continuous(limits=c(0,y_max)) + 
                                      theme(axis.text.x = element_blank(), 
                                            axis.ticks.x = element_blank(),
                                            axis.title.y = element_text(size=33),
                                            axis.text.y = element_text(size=30)), 
                                    symp_inf_students_5_plot + 
                                      scale_y_continuous(limits=c(0,y_max)) + 
                                      theme(axis.title.y = element_text(size=33),
                                            axis.text.y = element_text(size=30),
                                            axis.text.x = element_blank(),
                                            axis.ticks.x = element_blank()), 
                                    weighted_risk_inf_students_5_plot + 
                                      scale_y_continuous(limits=c(0.5,y_max)) + 
                                      theme(axis.title.y = element_text(size=33),
                                            axis.text.y = element_text(size=30)),
                                    absent_students_5_plot + 
                                      theme(axis.title.y = element_text(size=33),
                                            axis.text.y = element_text(size=30)),
                                    nrow=2, labels= "AUTO", label_size = 30, align="v")
ggsave(baseline_plot, file=paste0(figurePath, "Figure2.pdf"), 
       width=32, height=18)



# ---------------------------------------------------------------------------- #
# Figure 3
# ---------------------------------------------------------------------------- #
# Total number of infections for simulation scenarios
# ---------------------------------------------------------------------------- #
y_max = 0.55*100
ind <- c(1,2,3,4,5,6,7,8,9)
legend_figure3 <- get.legend.scenarios(school_outbreak_students_list[ind], 
                                       scenarios=scenarios[ind], 
                                       outputPath = outputPath, 
                                       colors=scenario_colors[ind], 
                                       suffix = "sim_scenarios", 
                                       width=6.5, height=1.3)

combined_inf_plot <- cowplot::plot_grid(inf_students_1_plot + 
                                          scale_y_continuous(limits=c(0,y_max)) + 
                                          theme(axis.text.x = element_blank(), 
                                                axis.ticks.x = element_blank(),
                                                axis.title.y = element_text(size=33),
                                                axis.text.y = element_text(size=30)), 
                                        inf_students_2_plot + 
                                          scale_y_continuous(limits=c(0,y_max)) + 
                                          theme(axis.title.y = element_blank(),
                                                axis.text.y = element_text(size=30),
                                                axis.text.x = element_blank(),
                                                axis.ticks.x = element_blank()), 
                                        inf_students_3_plot + 
                                          scale_y_continuous(limits=c(0,y_max)) + 
                                          theme(axis.title.y = element_text(size=33),
                                                axis.text.y = element_text(size=30)),
                                        inf_students_4_plot + 
                                          scale_y_continuous(limits=c(0,y_max)) +
                                          theme(axis.title.y = element_blank(),
                                                axis.text.y = element_text(size=30)),
                                        nrow=2, labels= "AUTO", label_size = 30, align="v")

combined_plot <- cowplot::plot_grid(combined_inf_plot, legend_figure3$legend, nrow=2, rel_heights = c(1, 0.1))
ggsave(combined_plot, file=paste0(figurePath, "Figure3.pdf"), width=32, height=20)

# ============================================================================ #
# Figure 4
# ---------------------------------------------------------------------------- #
# All interventions
# ============================================================================ #
ind <- c(1,10,11,12,13,14)
legend <- get.legend.scenarios(school_outbreak_students_list[ind], 
                               scenarios = scenarios[ind],
                               outputPath = outputPath, 
                               colors=scenario_colors[ind], 
                               suffix = "interventions", 
                               text_size=36)


y_max = 0.4*100
plot <- cowplot::plot_grid(inf_students_6_plot + 
                             scale_y_continuous(limits=c(0,y_max)) + 
                             theme(axis.text.x = element_blank(), 
                                   axis.ticks.x = element_blank(),
                                   axis.title.y = element_text(size=33),
                                   axis.text.y = element_text(size=30)), 
                           absent_students_6_plot + 
                             theme(axis.text.x = element_blank(), 
                                   axis.ticks.x = element_blank(),
                                   axis.title.y = element_text(size=33),
                                   axis.text.y = element_text(size=30)),
                           inf_students_7_plot + 
                             scale_y_continuous(limits=c(0,y_max)) + 
                             theme(axis.text.x = element_blank(), 
                                   axis.ticks.x = element_blank(),
                                   axis.title.y = element_text(size=33),
                                   axis.text.y = element_text(size=30)), 
                           absent_students_7_plot + 
                             theme(axis.text.x = element_blank(), 
                                   axis.ticks.x = element_blank(),
                                   axis.title.y = element_text(size=33),
                                   axis.text.y = element_text(size=30)),
                           inf_students_8_plot + 
                             scale_y_continuous(limits=c(0,y_max)) + 
                             theme(axis.title.y = element_text(size=33),
                                   axis.text.y = element_text(size=30)), 
                           absent_students_8_plot + 
                             theme(axis.title.y = element_text(size=33),
                                   axis.text.y = element_text(size=30)),
                           nrow=3, ncol=2, labels= "AUTO", label_size = 30, align="v")
combined_plot <- cowplot::plot_grid(plot, legend$legend, nrow=2, rel_heights = c(1, 0.1))

ggsave(combined_plot, file=paste0(figurePath, "Figure4.pdf"), 
       width=34, height=30)




# ---------------------------------------------------------------------------- #
# Figure 5
# ---------------------------------------------------------------------------- #
# Cost benefit
# ---------------------------------------------------------------------------- #
# Figure 5A: A health burden: symptomatic student days
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

# Remove first part of the year without when booster campaign hasn't started yet
remove_days <- 1:242 # Number of days to be removed
n_per_day_list_2 <- lapply(n_per_day_list, function(x) x[-c(1:242),])

symp_student_days_list_2 <- lapply(n_per_day_list_2, function(x) lapply(x[-1], function(y) sum(y)))
symp_student_days_2 <- lapply(symp_student_days_list_2, unlist)

# Bootstrapping
ind <- 1:14
effect <- vector(mode="list", length=length(ind))
for(i in 1:(length(ind))){
  effect[[i]] <- rep(NA, 100)
  for(k in 1:10000){
    baseline <- sample(symp_student_days_2[[1]], 1)
    intervention <- sample(symp_student_days_2[[ind[i]]], 1)
    effect[[i]][k] <- 100*(baseline-intervention)/baseline
  }
}

# Plot "intervention effect" on health burden
ind_interv <- 10:14
(eff_plot <- plot.outbreak.size(effect[ind_interv], 
                                  scenarios=scenarios[ind_interv], 
                                  permutation = seq(1:length(scenarios)),
                                  iter=10000,
                                  figuresPath=outputPath, 
                                  suffix="red_total_symp_students",
                                  occup_suffix="",
                                  title="",
                                  title_y="Reduction in number of symptomatic student days after 1st September 2022\n(%, compared to baseline scenario)",
                                  colors=scenario_colors[ind_interv]))

# ---------------------------------------------------------------------------- #
# Figure 5B: Cost-benefit
cost_benefit <- cost.benefit.plot(n_students_list, 
                                  type_1 = 2:4, 
                                  type_2 = 6:7, 
                                  ind=c(10:14), scenario_colors = scenario_colors)
ggsave(cost_benefit$plot, file=paste0(figurePath, "figure_cost_benefit_interventions.pdf"),
       width=14, height=14)
write.csv(cost_benefit$summary[order(cost_benefit$summary$time),], file=paste0(outputPath, "summary_cost_benefit.csv"), 
          row.names = F)

# ---------------------------------------------------------------------------- #
# Figure 5: Combined plot
figure_5_plot <- cowplot::plot_grid(eff_plot$plot + theme(legend.position = "none"), 
                                    cost_benefit$plot, 
                                    ncol=2, align="h", rel_widths = c(1.07, 1),
                                    labels="AUTO", label_size=30)
ggsave(figure_5_plot, file= paste0(figurePath, "Figure5.pdf"), 
       width=32, height=14)