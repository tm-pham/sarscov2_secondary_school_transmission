# =========================================================================== #
# Fixed variables 
# =========================================================================== #
seed=12345

# =========================================================================== #
# Time
# =========================================================================== #
unit=24
steps = 8/unit; time_period =30*4*7; time_steps = seq(1, time_period, by = steps)
group_per_step = rep(c(rep(1,(1/steps)*(24/unit)),rep(2,(1/steps)*(24/unit))), time_period/2)[1:length(time_steps)] # Assign groups to each time point, assuming 2 groups
day_names = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
# Dates
start_date = '03/01/2022'
date_format = '%d/%m/%Y'
date_seq = seq(as.Date(start_date, format=date_format), by="day", length.out=time_period)
date_steps = sort(rep(date_seq, 3))
date_steps = date_steps[1:length(time_steps)]

# Holidays
christmas1 = sort(rep(seq(from=as.Date("22/12/2022", format=date_format), to= as.Date("03/01/2023", format=date_format), by = "day"), 3))
christmas2 = sort(rep(seq(from=as.Date("22/12/2023", format=date_format), to= as.Date("03/01/2024", format=date_format), by = "day"), 3))
# christmas3 = sort(rep(seq(from=as.Date("22/12/2024", format=date_format), to= as.Date("03/01/2025", format=date_format), by = "day"), 3))
summer_holidays1 = sort(rep(seq(from=as.Date("15/07/2022", format=date_format), to= as.Date("01/09/2022", format=date_format), by = "day"), 3))
summer_holidays2 = sort(rep(seq(from=as.Date("15/07/2023", format=date_format), to= as.Date("01/09/2023", format=date_format), by = "day"), 3))
summer_holidays3 = sort(rep(seq(from=as.Date("15/07/2024", format=date_format), to= as.Date("01/09/2024", format=date_format), by = "day"), 3))
vacation_dates = c(summer_holidays1, christmas1, summer_holidays2, christmas2, summer_holidays3)
summer_vacation_dates = c(summer_holidays1, summer_holidays2, summer_holidays3)
vacation = time_steps[which(date_steps%in%vacation_dates)]
summer_vacation_1 = time_steps[which(date_steps%in%summer_holidays1)]
summer_vacation_2 = time_steps[which(date_steps%in%summer_holidays2)]
summer_vacation_3 = time_steps[which(date_steps%in%summer_holidays3)]
summer_vacation_1 = summer_vacation_1[summer_vacation_1==as.integer(summer_vacation_1)]
summer_vacation_2 = summer_vacation_2[summer_vacation_2==as.integer(summer_vacation_2)]
summer_vacation_3 = summer_vacation_3[summer_vacation_3==as.integer(summer_vacation_3)]

end_summer_holidays = c(as.Date("01/09/2022", format=date_format), as.Date("01/09/2023", format=date_format), as.Date("01/09/2024", format=date_format))
booster_time = time_steps[which(date_steps%in%end_summer_holidays)]
booster_time = booster_time[booster_time==as.integer(booster_time)]
# =========================================================================== #
# Disease characteristics
# =========================================================================== #
### Reproduction number (stepwise)
R0_winter = 2.0
R0_summer = 1.5
winter1 = sort(rep(seq(from=as.Date(start_date, format=date_format), to= as.Date("31/03/2022", format=date_format), by = "day"), 3))
summer1 = sort(rep(seq(from=as.Date("01/04/2022", format=date_format), to= as.Date("30/09/2022", format=date_format), by = "day"), 3))
winter2 = sort(rep(seq(from=as.Date("01/10/2022", format=date_format), to= as.Date("31/03/2023", format=date_format), by = "day"), 3))
summer2 = sort(rep(seq(from=as.Date("01/04/2023", format=date_format), to= as.Date("30/09/2023", format=date_format), by = "day"), 3))
winter3 = sort(rep(seq(from=as.Date("01/10/2023", format=date_format), to= as.Date("31/03/2024", format=date_format), by = "day"), 3))
summer3 = sort(rep(seq(from=as.Date("01/04/2024", format=date_format), to= as.Date("30/09/2024", format=date_format), by = "day"), 3))

R0_vec = c(rep(R0_winter, length(winter1)),
           rep(R0_summer, length(summer1)),
           rep(R0_winter, length(winter2)),
           rep(R0_summer, length(summer2)),
           rep(R0_winter, length(winter3)),
           rep(R0_summer, length(summer3)))
R0_vec = R0_vec[1:length(time_steps)]

### Reduction in vaccine efficacy due to new variant
red_vacc_exp = 85/52 # Keeling at al (2021): Short-term projections
red_vacc_exp_teacher = 88/60 # Table 1 in Keeling et al (2021): Short-term projections


### Susceptibility and infectivitiy
rel_susc_waning_vec = rep(0.75, length(scenarios))

rel_inf = 1.0

infec_scale = 1.0 
rec_time = c(14, 10) # Recovery period
iso_time = 7 # Isolation period (period that individuals have to isolate at home)
fp_iso_time = 1 # False-positives need to wait approx. 24 hours for PCR result
quaran_time = 10

# Scaling factor for symptomatic proportion
scaling_prop_symp = 0.8
scaling_constant_flag = F

# scaling <- 1/avg_cont
scaling = 1.0

# Values for Infectiousness profile for time since infection
time_vec_hour = seq(0, 28 , by=1/24)
gen_time = dgamma(time_vec_hour, shape=0.6639232, scale=3.313636) # omicron
infectivity = gen_time*R0*scaling
# # Function for Infectiousness profile
# infectivity_fun <- smooth.spline(seq(1, length(infectivity)), infectivity, spar=0.2)


# =========================================================================== #
# Testing characteristics
# =========================================================================== #
spec = 1.0 # Specifcity, for now: 100% 

### PCR Test sensitivity for time since infection
# test_sens = 0.9*c(0, 0.333333333, 0.454545455, 0.875, 
#                   0.96, 0.96, 0.92, 0.76, 0.64, 0.56, 0.25, 0.181818182, 0.045454545, 0)
# test_sens_fun = smooth.spline(c(0, seq(0.2883333,length(test_sens)-1,by=1)), test_sens, spar=0.3)

test_sens = 0.9*c(0, 0.454545455, 0.875, 
                  0.96, 0.96, 0.92, 0.76, 0.64, 0.56, 0.25, 0.181818182, 0.045454545, 0)
test_sens_fun = smooth.spline(c(0, seq(0.2883333,length(test_sens)-1,by=1)), test_sens, spar=0.3)
# test_sens_fun = smooth.spline(seq(0,length(test_sens)-1,by=1), test_sens, spar=0.3)
# pcr_test_sens = c(0, 0.666666667, 0.727272727, 1, 1, 1, 1, 1, 0.96, 0.88, 
#                   0.791666667, 0.909090909, 0.727272727, 0.9, 0.8, 0.526315789, 
#                   0.384615385, 0)
# pcr_test_sens_fun = smooth.spline(c(0, seq(0.2883333,length(pcr_test_sens)-1,by=1)), pcr_test_sens, spar=0.3)
pcr_test_sens = c(0, 0.727272727, 1, 1, 1, 1, 1, 0.96, 0.88, 
                  0.791666667, 0.909090909, 0.727272727, 0.9, 0.8, 0.526315789, 
                  0.384615385, 0)
pcr_test_sens_fun = smooth.spline(c(0, seq(0.2883333,length(pcr_test_sens)-1,by=1)), pcr_test_sens, spar=0.3)

### Data frame with infectiousness and test sensitivity for every hour
df_inf_sens = as.data.frame(cbind(time=time_vec_hour, 
                                  infectiousness=infectivity, 
                                  sensitivity=predict(test_sens_fun, time_vec_hour)$y, 
                                  pcr_sens=predict(pcr_test_sens_fun, time_vec_hour)$y))
df_inf_sens[,-1] <- apply(df_inf_sens[,-1], 2, function(x) ifelse(x<0, 0, x)) # Force negative values to be zero
df_inf_sens[,-1] <- apply(df_inf_sens[,-1], 2, function(x) ifelse(x>1, 1, x))

# =========================================================================== #
# General school characteristics
# =========================================================================== #
class_size = c(23, 29, 23, 29, 30, 24) # Number of student in each class
n_grades = 6                           # Number of grades
n_class = c(7,6,8,7,5,3)               # Number of classes per grade
grade_size = n_class*class_size        # Total number of students per grade
n_students = sum(grade_size)           # Total number of students 

### Contact network
# Option for contacts of students to be sampled daily (not used)
sample_contacts_daily = F

### Proportion of effective contacts of teachers
# 0.18 for half occupancy, 0.13 for full occupancy
# sample_prop = c(0.13, 0.18, 0.13, 0.18, 0.13, 0.13)
sample_prop = rep(0.13, 5)

### Contact matrix for students (from pilot study)
### MEAN values
n_cont_same_class = c(6,5,6,8,8,11)
n_cont_vec = c(3, 0, 0, 0, 0, 0,
               2, 5, 0, 0, 0, 0,
               1, 1, 5, 0, 0, 0,
               1, 1, 2, 7, 1, 1,
               0, 0, 0, 1, 7, 1,
               0, 0, 0, 1, 2, 7)
n_cont = matrix(n_cont_vec, nrow=6, byrow = T)
n_cont_close = 1 # Number of close contacts in class (students are sitting in pairs) 
n_cont_grade = diag(n_cont) # Number of contacts outside class but in school (students of the same grade)
n_cont_out_school = 2 # Number of contacts with students of the school but outside of school
n_cont_other = 3 # Number of contacts with other individuals not from school
n_random = 0
n_subjects = 5 # Number of subjects per grade

n_quaran = 2 # Number of students that need to be quarantined (from pilot study)
n_cont_teachers = 6
n_teacher_cont_other = 4

# =========================================================================== #
# Parameters (to be changed)
# =========================================================================== #


### Numerical tolerance
tol = 1e-5

### No interventions at all?
no_int_vec = rep(F, length(scenarios))

### Scaling factor for transmission probability for teacher-student contact
cont_susc_scale = c(0.5, 0.85)

### Adherence to screening
screening_adherence = 0
risk_testing_adherence = 0

### Compliance to isolation and quarantine
compliance_iso = 0.33 # Proportion of individuals that adhere to isolation
compliance_quaran = 0.87 # Proportion of individuals that adhere to quarantine
compliance_iso_scr = 0.9

### Types of individuals
types = c("S", "PS", "IS", "IA", "R", "IH", "Q", "V")

### Screening days
testing_mat = matrix(c(0,0,0,0,0,0,0), ncol=7, byrow=T)
colnames(testing_mat) <- day_names
if(nrow(testing_mat)!=length(scenarios)) print("Matrix for testing matrix does not have the right dimensions.")

### Occupancy
occup_suffix = rep("full_occup", length(scenarios))
occup = rep(1, length(scenarios)) # Occupancy level, options: 0.5, 1

### Flags for screening, risk-based testing and vaccination
scr_flags = rep(F, length(scenarios)); risk_flags = rep(F, length(scenarios))
vacc_flags = rep(T, length(scenarios))

### Aerosol transmission flags
aerosol_probs = rep(0.1, length(scenarios))
aerosol_susc_scale = c(1.0, 1.0)

### Proportion of vaccinated students and teachers
prop_vaccinated = c(0.60, 0.8)
prop_vaccinated_1 = prop_vaccinated[1]
prop_vaccinated_2 = prop_vaccinated[1]
vacc_eff_1 = c(0.57, 0.70)/red_vacc_exp
vacc_eff_2 = c(0.88, 0.92)/red_vacc_exp
vacc_eff_teacher_1 = c(0.57, 0.70)/red_vacc_exp_teacher
vacc_eff_teacher_2 = c(0.59, 0.866)/red_vacc_exp_teacher
# vacc_eff_1 = c(0.2, 0.59)
# vacc_eff_2 = c(0.78, 0.9)
vacc_rate_weekly_1 = 0
vacc_rate_weekly_2 = 0
vacc_weekly_flags <- rep(0, length(scr_flags))
t_2nd_dose = 14 # in days
vacc_eff_lag = c(0,0) # Lag already included in df_prob_vaccination
scaling_factor = 0.8 # Scaling proportion of asymptomatics
vacc_rate_2 = 0

### Correlation between vaccination and screening adherence
vc_corr = rep(-1, length(scr_flags))

### Fixed introductions from community
student_incidence <- 944*1.6*279/100000; teacher_incidence <- 72*1.3*243/100000
# (intro_fix_per_week <- c(944*1.6*279, 72*1.3*243)/100000)
# intro_fix_per_week <- c(944*1.6*161, 72*1.3*115)/100000
# intro_fix_per_week <- c(944*1.6*90, 72*1.3*60)/100000
intro_fix_per_week <- NULL

### External incidence rate for importations
# external_prob = 1.5*c(0.0007965315, 0.0007965315)
(external_prob = c(student_incidence/(mean(vacc_eff_2)*0.64*prop_vaccinated[1]*944*7), teacher_incidence/(mean(vacc_eff_teacher_2)*prop_vaccinated[2]*72*7)))
# external_prob = c(0.0005218215,0.0005218215)
# external_prob = c(0.00007340511,0.00007340511)

### Waning
lag_reinfection_vec = rep(0, length(scenarios))

# ============================================================================ #
# Absent days and absent individuals
abs_days_students = rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
absences_students = rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
abs_days_teachers = rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
absences_teachers = rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))

# Peak number of infected individuals
peak_students = rep(list(data.frame(matrix(rep(NA, 4*iter), nrow=iter, ncol=4, byrow=T))), length(scenarios))
peak_teachers = rep(list(data.frame(matrix(rep(NA, 4*iter), nrow=iter, ncol=4, byrow=T))), length(scenarios))

# Infector
infector_students = infector_teachers = vector(mode="list", length=length(scenarios))

# Contacts between students and teachers
contact_student_teachers = vector(mode="list", length=length(scenarios))

# Outbreak size
outbreak_data_students = outbreak_data_teachers = vector(mode="list", length=length(scenarios))
outbreak_size_students = outbreak_size_teachers = rep(list(rep(0,iter)), length(scenarios))

# Outbreak size per "location"
outbreak_per_loc_st = rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))
outbreak_per_loc_teach = rep(list(data.frame(matrix(rep(NA, 3*iter), nrow=iter, ncol=3, byrow=T))), length(scenarios))

# Symptomatic cases
symptomatic_students = rep(list(rep(0,iter)), length(scenarios))
symptomatic_teachers = rep(list(rep(0,iter)), length(scenarios))

# Secondary cases
sec_cases_students = sec_cases_teachers = vector(mode="list", length=length(scenarios))
sec_symp_students = sec_symp_teachers = vector(mode="list", length=length(scenarios))

# Detected students and teachers by risk-based testing
det_risk_students = det_risk_teachers = vector(mode="list", length=length(scenarios))

# Screening
screening_stats = rep(list(data.frame(matrix(rep(NA, 2*iter), nrow=iter, ncol=2, byrow=T))), length(scenarios))
det_screening = rep(list(data.frame(matrix(rep(NA, 2*iter), nrow=iter, ncol=2, byrow=T))), length(scenarios))

# Quarantined and isolated students
IA_quaran_students = IS_quaran_students = vector(mode="list", length=length(scenarios))
IA_iso_teachers = IA_iso_students = vector(mode="list", length=length(scenarios))

# Number over time
ns_list = vector(mode="list", length=length(scenarios))
nt_list = vector(mode="list", length=length(scenarios))
epidemic_list = vector(mode="list", length=length(scenarios))

# Vaccination coverage
vacc_coverage1 = vector(mode="list", length(scenarios))
vacc_coverage2 = vector(mode="list", length(scenarios))
vacc_coverage2_t = vector(mode="list", length(scenarios))

# Proportion immune
prop_immune_students = vector(mode="list", length(scenarios))
prop_immune_teachers = vector(mode="list", length(scenarios))


### Infections within the same class
n_no_index_students_list <- n_no_index_teachers_list <- vector(mode="list", length=length(scenarios))
n_same_class_students_list <- n_same_class_teachers_list <- vector(mode="list", length=length(scenarios))

# Number of infected weekly
os_student_weekly = os_teacher_weekly = os_weekly = vector(mode="list", length=length(scenarios))
intro_student_weekly = intro_teacher_weekly = intro_weekly = vector(mode="list", length=length(scenarios))
symp_student_weekly = symp_teacher_weekly = symp_weekly = vector(mode="list", length=length(scenarios))

# Reinfections
reinfected_students_list = vector(mode="list", length=length(scenarios))
reinfected_students_rec_list = vector(mode="list", length=length(scenarios))
reinfected_students_vacc_list = vector(mode="list", length=length(scenarios))
df_reinfected_students_list  = vector(mode="list", length=length(scenarios))

reinfected_teachers_list = vector(mode="list", length=length(scenarios))
reinfected_teachers_rec_list = vector(mode="list", length=length(scenarios))
reinfected_teachers_vacc_list = vector(mode="list", length=length(scenarios))
df_reinfected_teachers_list  = vector(mode="list", length=length(scenarios))

# Set colnames
for(x in 1:length(scenarios)){
  colnames(screening_stats[[x]]) <- c("students","teachers")
  colnames(det_screening[[x]]) <- c("pos_students","pos_teachers")
  
  colnames(outbreak_per_loc_st[[x]]) <- c("Outside school-related", "Within school", "External")
  colnames(outbreak_per_loc_teach[[x]]) <- c("Outside school-related", "Within school", "External")
  
  colnames(peak_students[[x]]) <- c("Asymptomatic", "Pre-symptomatic", "Symptomatic", "Infected")
  colnames(peak_teachers[[x]]) <- c("Asymptomatic", "Pre-symptomatic", "Symptomatic", "Infected")
  
  colnames(abs_days_students[[x]]) <- c("Isolated", "Quarantined", "Total absent")
  colnames(absences_students[[x]]) <- c("Isolated", "Quarantined", "Total absent")
  
  colnames(abs_days_teachers[[x]]) <- c("Isolated", "Quarantined", "Total absent")
  colnames(absences_teachers[[x]]) <- c("Isolated", "Quarantined", "Total absent")
}

# ============================================================================ #
# Data frame with infection and vaccination times
# ============================================================================ #
# Created with covid19school_incidence_prob_infection_time.R
df_time_inf_students = read.csv(file="../../data/prob_infection_time_students_NL.csv")
df_time_inf_teachers = read.csv(file="../../data/prob_infection_time_teachers_NL.csv")

### Data frame with daily probability of reinfection
use_reinf_function = T
use_townsend = T
slower_waning_after_recovery = F
# Reinfection function according to Townsend et al (2021)
sarscov2baseline = 0.1301183
lambda_townsend = 0.003746311184177791
reinfection.function <- function(t, baseline=0.1301183, lambda=0.003746311184177791){
  return(1- (baseline + (1-baseline)*exp(-lambda*t)))
}

if(use_reinf_function){
  time_vec <- seq(0, 5000, by=1/3)
  if(!use_townsend){
    # lambda_students = 0.000268938
    # lambda_teachers = 0.002541675 
    lambda_students = lambda_townsend
    lambda_teachers = lambda_townsend
    df_prob_reinfection_students = as.data.frame(cbind(time=time_vec, prob=reinfection.function(t=1, lambda=lambda_students)))
    df_prob_reinfection_teachers = as.data.frame(cbind(time=time_vec, prob=reinfection.function(t=1, lambda=lambda_teachers)))
    df_prob_reinfection_students_2 = df_prob_reinfection_students
    df_prob_reinfection_teachers_2 = df_prob_reinfection_teachers
    if(slower_waning_after_recovery){
      df_prob_reinfection_students_2 = as.data.frame(cbind(time=time_vec, prob=reinfection.function(t=1, lambda=lambda_townsend)))
      df_prob_reinfection_teachers_2 = as.data.frame(cbind(time=time_vec, prob=reinfection.function(t=1, lambda=lambda_townsend)))
    }
  }else{
    df_prob_reinfection_students = as.data.frame(cbind(time=time_vec, prob=reinfection.function(t=1, lambda=lambda_townsend)))
    df_prob_reinfection_teachers = df_prob_reinfection_students
    df_prob_reinfection_students_2 = df_prob_reinfection_students
    df_prob_reinfection_teachers_2 = df_prob_reinfection_teachers
  }
}else{
  df_prob_reinfection_students = read.csv(file="../../data/daily_prob_reinfection_students.csv")
  df_prob_reinfection_teachers = read.csv(file="../../data/daily_prob_reinfection_teachers.csv")
  df_prob_reinfection_students_2 = read.csv(file="../../data/daily_prob_reinfection_students_2.csv")
  df_prob_reinfection_teachers_2 = read.csv(file="../../data/daily_prob_reinfection_teachers_2.csv")
}


### Data frame with probability of vaccination before start of simulation (given being vaccinated)
df_prob_vacc_students = read.csv(file="../../data/prob_vacc_students_over_time.csv")
df_prob_vacc_teachers = read.csv(file="../../data/prob_vacc_teachers_over_time.csv")
