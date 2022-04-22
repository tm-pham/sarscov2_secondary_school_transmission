# ============================================================================ #
# Cost benefit of quarantine and screening
# Plots for Figure 6
# ============================================================================ #
rm(list=ls())
path <- "/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/"
setwd(paste0(path, "code/"))
source(paste0(path, "code/plotting_code/covid19school_plotting_functions.R"))
source(paste0(path, "code/plotting_code/covid19school_omicron_scenarios.R"))
library(zoo)
outputPath <- "/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/figures/"


# ---------------------------------------------------------------------------- #
# NEW
# ---------------------------------------------------------------------------- #
# Cost benefit
# ---------------------------------------------------------------------------- #
cost_benefit <- cost.benefit.plot(n_students_list, 
                                  type_1 = 2:4, 
                                  type_2 = 6:7, 
                                  ind=c(12:15, 17), scenario_colors = scenario_colors)

ggsave(cost_benefit$plot, file=paste0(outputPath, "figure_cost_benefit_interventions.pdf"),
       width=14, height=14)


write.csv(cost_benefit$summary[order(cost_benefit$summary$time),], file=paste0(outputPath, "summary_cost_benefit.csv"), 
          row.names = F)

# ---------------------------------------------------------------------------- #
# OLD
# ---------------------------------------------------------------------------- #
# Number of prevented infections per quarantined student
# ---------------------------------------------------------------------------- #
df_inf_students <- inf_students_8$data

df_inf_students <- df_inf_students %>%
  group_by(time) %>% 
  dplyr::mutate(id=cur_group_id())

df_inf_students <- df_inf_students %>% 
  arrange(id, time) %>%
  group_by(id) %>%
  dplyr::mutate(prev_inf = mean[1L]-mean)

df_inf_students

df_absent_students <- absent_students_8$data
df_absent_students <- df_absent_students %>%
  group_by(time) %>% 
  dplyr::mutate(id=cur_group_id())
df_absent_students <- df_absent_students %>% 
  arrange(id, time) %>%
  group_by(id) %>%
  dplyr::mutate(add_absent = mean-mean[1L])

df_inf_absent <- as.data.frame(cbind(df_inf_students[,c("time", "scenario", "mean", "prev_inf")], 
                                     df_absent_students[, c("mean","add_absent")]))
colnames(df_inf_absent) <- c("time", "scenario", "n_inf", "prev_inf", "n_absent", "add_absent")
df_inf_absent <- as_tibble(df_inf_absent) %>% group_by(time) %>% dplyr::mutate(res = prev_inf/n_absent)
df_inf_absent

# Calculate rolling average (over 4 weeks)
df_inf_absent <- df_inf_absent  %>% 
  dplyr::group_by(scenario) %>% 
  dplyr::arrange(time) %>% 
  dplyr::mutate(res_mean4 = zoo::rollmean(res, k=4, fill=NA))

# ---------------------------------------------------------------------------- #
# Number of prevented infected students per screened student
# ---------------------------------------------------------------------------- #
df_inf_scr <- inf_students_10$data
df_inf_scr <- df_inf_scr %>%
  group_by(time) %>% 
  dplyr::mutate(id=cur_group_id())
df_inf_scr <- df_inf_scr%>% 
  arrange(id, time) %>%
  group_by(id) %>%
  dplyr::mutate(prev_inf = mean[1L]-mean)
df_absent_scr <- absent_students_10$data
df_absent_scr <- df_absent_scr %>%
  group_by(time) %>% 
  dplyr::mutate(id=cur_group_id())
df_absent_scr <- df_absent_scr%>% 
  arrange(id, time) %>%
  group_by(id) %>%
  dplyr::mutate(add_absent = mean[1L]-mean)
df_inf_absent_scr <- as.data.frame(cbind(df_inf_scr[,c("time", "scenario", "mean", "prev_inf")], 
                                         df_absent_scr[, c("mean","add_absent")]))
colnames(df_inf_absent_scr) <- c("time", "scenario", "n_inf", "prev_inf", "n_absent", "add_absent")
df_inf_absent_scr <- as_tibble(df_inf_absent_scr) %>% group_by(time) %>% dplyr::mutate(res = prev_inf/n_absent)
df_inf_absent_scr %>% filter(scenario!="Baseline")

# Calcualte rolling average (over 4 weeks)
df_inf_absent_scr <- df_inf_absent_scr  %>% 
  dplyr::group_by(scenario) %>% 
  dplyr::arrange(time) %>% 
  dplyr::mutate(res_mean4 = zoo::rollmean(res, k=4, fill=NA))


df <- rbind(df_inf_absent %>% filter(scenario!="Baseline"), df_inf_absent_scr %>% filter(scenario!="Baseline"))
df <- df %>% arrange(time)

# ---------------------------------------------------------------------------- #
# Cost-benefit plot
# ---------------------------------------------------------------------------- #
cost_benefit_plot <- ggplot(df, aes(x=time, y=res, color=scenario)) +
  geom_point(size=2.5) + 
  geom_line(size=1.5) + 
  labs(y="Number of prevented infected students\nper absent student") + 
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  scale_colour_manual(values=scenario_colors[c(12,13,14,15)]) + 
  theme_publication() + 
  theme(legend.title = element_blank(),
        legend.justification = c("right", "top"),
        legend.position = c(0.95, 0.95),
        legend.box.background = element_rect(colour = "black", size=1.2), 
        legend.text = element_text(size=40),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=40),
        axis.text.y = element_text(size=36),
        axis.text.x = element_text(size=28, angle=45, hjust=0.9)) - 
  annotate("rect", xmin=min((inf_students_1$data)$time),xmax=as.Date("30/03/2022", format=date_format),ymin=-Inf,ymax=Inf,
           fill="gray77", color="black", linetype=29, alpha=0.3) -
  annotate("rect",xmin=as.Date("01/04/2022", format=date_format),xmax=as.Date("30/09/2022", format=date_format),ymin=-Inf,ymax=Inf,
           fill="gray88", color="NA", linetype=29, alpha=0.3) -
  annotate("rect", xmin=as.Date("01/10/2022", format=date_format),xmax=as.Date("30/03/2023", format=date_format),ymin=-Inf,ymax=Inf,
           fill="gray77", color="black", linetype=29, alpha=0.3) -
  annotate("rect", xmin=as.Date("01/04/2023", format=date_format),xmax=as.Date("30/09/2023", format=date_format),ymin=-Inf,ymax=Inf,
           fill="gray88", color="NA", linetype=29, alpha=0.3) -
  annotate("rect", xmin=as.Date("01/10/2023", format=date_format),xmax=as.Date("30/03/2024", format=date_format),ymin=-Inf,ymax=Inf,
           fill="gray77", color="black", linetype=29, alpha=0.3)
cost_benefit_plot 
ggsave(cost_benefit_plot, file=paste0(outputPath, "cost_benefit_quarantine_screening.pdf"),
       width=32, height=14)

max(df[df$scenario%in%c("Class quarantine (50% case isolation)", "Class quarantine (75% case isolation)"), "res"])


cowplot::plot_grid(inf_students_8_plot + theme(axis.text.x = element_blank(), axis.ticks = element_blank()), 
                   inf_students_10_plot + theme(axis.text.x = element_blank(), axis.ticks = element_blank()), 
                   cost_benefit_plot + theme(legend.position = "none"), 
                   nrow=3, align="v")

# ---------------------------------------------------------------------------- #
# Cost-benefit plot for rolling average
# ---------------------------------------------------------------------------- #
cost_benefit_plot <- ggplot(df, aes(x=time, y=res_mean4, color=scenario)) +
  geom_point(size=2.5) + 
  geom_line(size=1.5) + 
  labs(y="Number of prevented infected students\nper absent student") + 
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  scale_colour_manual(values=scenario_colors[c(12,13,14,15)]) + 
  theme_publication() + 
  theme(legend.title = element_blank(),
        legend.justification = c("right", "top"),
        legend.position = c(0.95, 0.95),
        legend.box.background = element_rect(colour = "black", size=1.2), 
        legend.text = element_text(size=40),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=40),
        axis.text.y = element_text(size=36),
        axis.text.x = element_text(size=28, angle=45, hjust=0.9)) - 
  annotate("rect", xmin=min((inf_students_1$data)$time),xmax=as.Date("30/03/2022", format=date_format),ymin=-Inf,ymax=Inf,
           fill="gray77", color="black", linetype=29, alpha=0.3) -
  annotate("rect",xmin=as.Date("01/04/2022", format=date_format),xmax=as.Date("30/09/2022", format=date_format),ymin=-Inf,ymax=Inf,
           fill="gray88", color="NA", linetype=29, alpha=0.3) -
  annotate("rect", xmin=as.Date("01/10/2022", format=date_format),xmax=as.Date("30/03/2023", format=date_format),ymin=-Inf,ymax=Inf,
           fill="gray77", color="black", linetype=29, alpha=0.3) -
  annotate("rect", xmin=as.Date("01/04/2023", format=date_format),xmax=as.Date("30/09/2023", format=date_format),ymin=-Inf,ymax=Inf,
           fill="gray88", color="NA", linetype=29, alpha=0.3) -
  annotate("rect", xmin=as.Date("01/10/2023", format=date_format),xmax=as.Date("30/03/2024", format=date_format),ymin=-Inf,ymax=Inf,
           fill="gray77", color="black", linetype=29, alpha=0.3)
cost_benefit_plot 
ggsave(cost_benefit_plot, file=paste0(outputPath, "cost_benefit_quarantine_screening_rollmean4.pdf"),
       width=32, height=14)

# Maximum prevention 
(max_quaran <- max(df[df$scenario%in%c("Class quarantine (50% case isolation)", 
                        "Class quarantine (75% case isolation)"), "res_mean4"], na.rm=T))
(max_scr <- max(df[df$scenario%in%c("Screening 2x weekly (50% adherence)", 
                        "Screening 2x weekly (75% adherence)"), "res_mean4"], na.rm=T))
# Quarantine
ind_max_qu <- which(unlist(df[df$scenario%in%c("Class quarantine (50% case isolation)", 
                                                "Class quarantine (75% case isolation)"), "res_mean4"]==max_quaran))
df_qu <- df[df$scenario%in%c("Class quarantine (50% case isolation)", 
                             "Class quarantine (75% case isolation)"), ]
df_qu[ind_max_qu, ]

# Screening
ind_max_scr <- which(unlist(df[df$scenario%in%c("Screening 2x weekly (50% adherence)", 
                                "Screening 2x weekly (75% adherence)"), "res_mean4"]==max_scr))
df_scr <- df[df$scenario%in%c("Screening 2x weekly (50% adherence)", 
                              "Screening 2x weekly (75% adherence)"), ]
df_scr[ind_max_scr, ]


# Prevented infections
(max_prevInf_scr <- max(df[df$scenario%in%c("Screening 2x weekly (50% adherence)", 
                                    "Screening 2x weekly (75% adherence) "), "prev_inf"], na.rm=T))
ind_max_prevInf_scr <- which(unlist(df[df$scenario%in%c("Screening 2x weekly (50% adherence)", 
                                                "Screening 2x weekly (75% adherence) "), "prev_inf"]==max_prevInf_scr))
df_scr[ind_max_prevInf_scr, ]

# Added absenteism
(add_absent <- max(df[df$scenario%in%c("Class quarantine (50% case isolation)", 
                                      "Class quarantine (75% case isolation)"), "add_absent"], na.rm=T))
ind_max_abs_qu <- which(unlist(df[df$scenario%in%c("Class quarantine (50% case isolation)", 
                                               "Class quarantine (75% case isolation)"), "res_mean4"]==add_absent))
df_qu <- df[df$scenario%in%c("Class quarantine (50% case isolation)", 
                             "Class quarantine (75% case isolation)"), ]
df_qu[ind_max_qu, ]
