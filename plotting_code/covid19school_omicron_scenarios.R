# ============================================================================ #
# School transmission long term simulation
# ============================================================================ #
rm(list=ls())
source("plotting_code/covid19school_plotting_functions.R")

# Load all scenarios
fileName <- create.data.for.plotting(file_path = "/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/results/omicron_sim/",
                                     file_suffix="omicron_all_", 
                                     scenarios=c("omicron_Baseline_townsend_redSusc_R2R1-5",
                                                 # Higher reproduction number
                                                 "omicron_townsend_redSusc_R1R0-75",
                                                 "omicron_baseTownsend_redSusc_R4R3",
                                                 # Susceptibility after reinfection
                                                 "omicron_townsend_redSusc50_R2R1-5",
                                                 "omicron_townsend_fullSusc_R2R1-5",
                                                 # Increased virulence
                                                 "omicron_townsend_incrVir20_R2R1-5",
                                                 # Waning
                                                 "omicron_waning18m_redSusc_R2R1-5", # slow
                                                 "omicron_waning3m_redSusc_R2R1-5", # fast
                                                 # Immune escape for vaccinated
                                                 "omicron_highVacc_townsend_redSusc_R2R1-5", # high vaccination efficacy
                                                 "omicron_lowVacc_townsend_redSusc_R2R1-5", #low vaccination efficacy
                                                 # Shorter isolation period and low adherence
                                                 "omicron_noiso_townsend_redSusc_R2R1-5",
                                                 # Class quarantine 
                                                 "omicron_quaranClass50_townsend_redSusc_R2R1-5",
                                                 "omicron_quaranClass75_townsend_redSusc_R2R1-5",
                                                 # Screening
                                                 # Imperfect sensitivity
                                                 "omicron_scr50NoCTwice_iso33_100_townsend_redSusc_R2R1-5", 
                                                 "omicron_scr75NoCTwice_iso33_100_townsend_redSusc_R2R1-5",
                                                 # Annual booster
                                                 "omicron_annualBoosterIncr20_townsend_redSusc_R2R1-5"
                                     ), 
                                     names_scenarios=c("Baseline",
                                                       ### Reproduction number
                                                       "Lower R (50% decrease)",
                                                       "Higher R (100% increase)",
                                                       ### Susceptibility
                                                       "Reduced susceptibility (50%) to reinfection",
                                                       "Full susceptibility to reinfection",
                                                       #### Virulence
                                                       "Increased virulence (20% more symptoms)",
                                                       #### Waning
                                                       "Slow waning (avg duration: 18 months)",
                                                       "Fast waning (avg duration: 3 months)",
                                                       #### Higher immune escape
                                                       "Lower immune escape in vaccinated",
                                                       "Higher immune escape in vaccinated",
                                                       #### Shorter isolation
                                                       "Shorter isolation (10% adherence)",
                                                       #### Quarantine
                                                       "Class quarantine (50% case isolation)",
                                                       "Class quarantine (75% case isolation)",
                                                       ### Screening
                                                       "Screening 2x weekly (50% adherence)", 
                                                       "Screening 2x weekly (75% adherence)",
                                                       ### Booster campaign
                                                       "Annual booster campaign"
                                                       
                                     ),
                                     suffix_scenarios=c(rep("", 17)), 
                                     suffix = "_vacc60_30m",
                                     scenario_colors = c("#0095d0", # blue
                                                         
                                                         "#f774ad", # light pink
                                                         "#be3679", # dark pink
                                                         
                                                         "#3aab00", # light green
                                                         "#38761d", # dark green
                                                         
                                                         "#d66363", # light red
                                                         
                                                         "#f1c232", # yellow
                                                         "#ff8000", # orange
                                                         
                                                         "#d66363", # light red
                                                         "#980000", # dark red
                                                         
                                                         "#274e13", # dark green
                                                         
                                                         "#9900ff", # light purple
                                                         "#351c75", # dark purple
                                                         
                                                         "#0000ff", # dark blue
                                                         "#00ffff", # light blue
                                                         
                                                         "#0e215b" # very dark blue
                                     )
)
load(fileName)

### Print number of iterations
for(i in 1:length(n_students_list)){
  number_iterations <- ncol(n_students_list[[i]][[1]][[1]])
  print(paste0(names_scenarios[i], ", Number of iterations: ", number_iterations))
}


# ============================================================================ #
# Plotting
# ============================================================================ #
subsets <- list(c(1,2,3), c(1,4,5), c(1,9,10), c(1,7,8), 
                c(1,6),     # Increased virulence
                c(1,11),    # Shorter isolation
                c(1,12,13), # Class quarantine
                c(1),       # Only baseline
                c(1,14,15), # Screening 2x weekly (50%,75%), imperfect sensitivity
                c(1,16),     # Baseline and annual booster
                c(1,17),     # Baseline and annual booster with increased VE
                c(1,16,17),
                c(1,13,15,17)
) 
quaran_ind <- c(1,12,13)
facet_titles <- c("Reproduction number", "Susceptibility to reinfection", "Vaccine immune escape", "Waning of immunity",
                  rep("", length(subsets)-4))
# facet_titles <- c(rep("", length(subsets)))
start_date = "03/01/2022"
for(s in 1:length(subsets)){
  # -------------------------------------------------------------------------- #
  # Prevalence (percentage of infections)
  # -------------------------------------------------------------------------- #
  var_name <- paste0("inf_students_", s)
  assign(var_name, n.per.unit(n_students_list[unlist(subsets[[s]])],
                              time_unit = 7, time_unit_name = "week",
                              type = 2:4, 
                              scenarios = scenarios[unlist(subsets[[s]])], 
                              figuresPath=outputPath, 
                              suffix="n_inf_students", occup_suffix=paste0("vacc",vacc_cov, "_", s),
                              title="", title_y="Infected students per week (%)", 
                              start_date = start_date, date_format = "%d/%m/%Y",
                              colors=scenario_colors[unlist(subsets[[s]])],
                              legend_position = "none", legend_title=NULL,
                              facet_title = facet_titles[s],
                              percentage = T, 
                              n_total = 944,
                              vacc_eff = F, 
                              lower_ci = 0.025, upper_ci=0.975, 
                              width=26, height=10))
  date_format = "%d/%m/%Y"
  assign(paste0(var_name, "_plot"), 
         eval(parse(text=paste0(var_name, "$plot"))) -
           annotate("rect", xmin=min((inf_students_1$data)$time),xmax=as.Date("30/03/2022", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray77", color="black", linetype=29, alpha=0.3) -
           annotate("rect",xmin=as.Date("01/04/2022", format=date_format),xmax=as.Date("30/09/2022", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray88", color="NA", linetype=29, alpha=0.3) -
           annotate("rect", xmin=as.Date("01/10/2022", format=date_format),xmax=as.Date("30/03/2023", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray77", color="black", linetype=29, alpha=0.3) -
           annotate("rect", xmin=as.Date("01/04/2023", format=date_format),xmax=as.Date("30/09/2023", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray88", color="NA", linetype=29, alpha=0.3) -
           annotate("rect", xmin=as.Date("01/10/2023", format=date_format),xmax=as.Date("30/03/2024", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray77", color="black", linetype=29, alpha=0.3))
  
  # -------------------------------------------------------------------------- #
  # Total number of symptomatic infections
  # -------------------------------------------------------------------------- #
  var_name <- paste0("symp_inf_students_", s)
  assign(var_name, n.per.unit(n_students_list[unlist(subsets[[s]])],
                              time_unit = 7, time_unit_name = "week",
                              type = 3, 
                              scenarios = scenarios[unlist(subsets[[s]])], 
                              figuresPath=outputPath, 
                              suffix="n_inf_students", occup_suffix=paste0("vacc",vacc_cov, "_", s),
                              title="", title_y="Sympt. infected students per week (%)", 
                              start_date =  start_date, date_format = "%d/%m/%Y",
                              colors=scenario_colors[unlist(subsets[[s]])],
                              legend_position = "none", legend_title=NULL,
                              facet_title = facet_titles[s],
                              percentage = T, 
                              n_total = 944,
                              vacc_eff = F, 
                              lower_ci = 0.025, upper_ci=0.975, 
                              width=26, height=10))
  date_format = "%d/%m/%Y"
  assign(paste0(var_name, "_plot"), 
         eval(parse(text=paste0(var_name, "$plot"))) -
           annotate("rect", xmin=min((symp_inf_students_1$data)$time),xmax=as.Date("30/03/2022", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray77", color="black", linetype=29, alpha=0.3) -
           annotate("rect",xmin=as.Date("01/04/2022", format=date_format),xmax=as.Date("30/09/2022", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray88", color="NA", linetype=29, alpha=0.3) -
           annotate("rect", xmin=as.Date("01/10/2022", format=date_format),xmax=as.Date("30/03/2023", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray77", color="black", linetype=29, alpha=0.3) -
           annotate("rect", xmin=as.Date("01/04/2023", format=date_format),xmax=as.Date("30/09/2023", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray88", color="NA", linetype=29, alpha=0.3) -
           annotate("rect", xmin=as.Date("01/10/2023", format=date_format),xmax=as.Date("30/03/2024", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray77", color="black", linetype=29, alpha=0.3))
  
  # -------------------------------------------------------------------------- #
  # Percentage of symptomatic infections
  # -------------------------------------------------------------------------- #
  var_name <- paste0("symp_inf_total_students_", s)
  assign(var_name, n.per.unit(n_students_list[unlist(subsets[[s]])],
                              time_unit = 7, time_unit_name = "week",
                              type = 3, 
                              scenarios = scenarios[unlist(subsets[[s]])], 
                              figuresPath=outputPath, 
                              suffix="n_inf_students", occup_suffix=paste0("vacc",vacc_cov, "_", s),
                              title="", title_y="Sympt. infected students per week (%)", 
                              start_date =  start_date, date_format = "%d/%m/%Y",
                              colors=scenario_colors[unlist(subsets[[s]])],
                              legend_position = "none", legend_title=NULL,
                              facet_title = facet_titles[s],
                              percentage = F, 
                              n_total = 944,
                              vacc_eff = F, 
                              lower_ci = 0.025, upper_ci=0.975, 
                              width=26, height=10))
  date_format = "%d/%m/%Y"
  assign(paste0(var_name, "_plot"), 
         eval(parse(text=paste0(var_name, "$plot"))) -
           annotate("rect", xmin=min((symp_inf_students_1$data)$time),xmax=as.Date("30/03/2022", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray77", color="black", linetype=29, alpha=0.3) -
           annotate("rect",xmin=as.Date("01/04/2022", format=date_format),xmax=as.Date("30/09/2022", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray88", color="NA", linetype=29, alpha=0.3) -
           annotate("rect", xmin=as.Date("01/10/2022", format=date_format),xmax=as.Date("30/03/2023", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray77", color="black", linetype=29, alpha=0.3) -
           annotate("rect", xmin=as.Date("01/04/2023", format=date_format),xmax=as.Date("30/09/2023", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray88", color="NA", linetype=29, alpha=0.3) -
           annotate("rect", xmin=as.Date("01/10/2023", format=date_format),xmax=as.Date("30/03/2024", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray77", color="black", linetype=29, alpha=0.3))
  
  # -------------------------------------------------------------------------- #
  # Weighted number of students at risk for infection
  # -------------------------------------------------------------------------- #
  var_name <- paste0("weighted_risk_inf_students_", s)
  assign(var_name, n.per.unit(n_students_list[unlist(subsets[[s]])],
                              time_unit = 7, time_unit_name = "week",
                              type = 24, 
                              scenarios = scenarios[unlist(subsets[[s]])], 
                              figuresPath=outputPath, 
                              suffix="n_inf_students", occup_suffix=paste0("vacc",vacc_cov, "_", s),
                              title="", title_y="Students at risk for infection\nper week (%, weighted)", 
                              start_date =  start_date, date_format = "%d/%m/%Y",
                              colors=scenario_colors[unlist(subsets[[s]])],
                              legend_position = "none", legend_title=NULL,
                              facet_title = facet_titles[s],
                              percentage = T, 
                              n_total = 944,
                              vacc_eff = F, 
                              lower_ci = 0.025, upper_ci=0.975, 
                              width=26, height=10))
  date_format = "%d/%m/%Y"
  assign(paste0(var_name, "_plot"), 
         eval(parse(text=paste0(var_name, "$plot"))) -
           annotate("rect", xmin=min((weighted_risk_inf_students_1$data)$time),xmax=as.Date("30/03/2022", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray77", color="black", linetype=29, alpha=0.3) -
           annotate("rect",xmin=as.Date("01/04/2022", format=date_format),xmax=as.Date("30/09/2022", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray88", color="NA", linetype=29, alpha=0.3) -
           annotate("rect", xmin=as.Date("01/10/2022", format=date_format),xmax=as.Date("30/03/2023", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray77", color="black", linetype=29, alpha=0.3) -
           annotate("rect", xmin=as.Date("01/04/2023", format=date_format),xmax=as.Date("30/09/2023", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray88", color="NA", linetype=29, alpha=0.3) -
           annotate("rect", xmin=as.Date("01/10/2023", format=date_format),xmax=as.Date("30/03/2024", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray77", color="black", linetype=29, alpha=0.3))
  
  # -------------------------------------------------------------------------- #
  # Absent students
  # -------------------------------------------------------------------------- #
  var_name <- paste0("absent_students_", s)
  assign(var_name, n.per.unit(n_students_list[unlist(subsets[[s]])],
                              time_unit = 7, time_unit_name = "week",
                              type = 6:7, 
                              scenarios = scenarios[unlist(subsets[[s]])], 
                              figuresPath=outputPath, 
                              suffix="n_inf_students", occup_suffix=paste0("vacc",vacc_cov, "_", s),
                              title="", title_y="Absent students per week\n(%, isolated or quarantined)", 
                              start_date =  start_date, date_format = "%d/%m/%Y",
                              colors=scenario_colors[unlist(subsets[[s]])],
                              legend_position = "none", legend_title=NULL,
                              facet_title = facet_titles[s],
                              percentage = T, 
                              n_total = 944,
                              vacc_eff = F, 
                              lower_ci = 0.025, upper_ci=0.975, 
                              width=26, height=10))
  date_format = "%d/%m/%Y"
  assign(paste0(var_name, "_plot"), 
         eval(parse(text=paste0(var_name, "$plot"))) -
           annotate("rect", xmin=min((absent_students_1$data)$time),xmax=as.Date("30/03/2022", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray77", color="black", linetype=29, alpha=0.3) -
           annotate("rect",xmin=as.Date("01/04/2022", format=date_format),xmax=as.Date("30/09/2022", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray88", color="NA", linetype=29, alpha=0.3) -
           annotate("rect", xmin=as.Date("01/10/2022", format=date_format),xmax=as.Date("30/03/2023", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray77", color="black", linetype=29, alpha=0.3) -
           annotate("rect", xmin=as.Date("01/04/2023", format=date_format),xmax=as.Date("30/09/2023", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray88", color="NA", linetype=29, alpha=0.3) -
           annotate("rect", xmin=as.Date("01/10/2023", format=date_format),xmax=as.Date("30/03/2024", format=date_format),ymin=-Inf,ymax=Inf,
                    fill="gray77", color="black", linetype=29, alpha=0.3))
}
