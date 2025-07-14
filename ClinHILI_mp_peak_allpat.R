library(tidyverse)
library(table1)
library(tableone)
library(ggplot2)
library(parameters)
library(chest)
library(clusterSEs)
library(gtsummary)
library(mgcv)
library(plotly)
library(tibble)
library(flextable)
library(patchwork)
library(officer)
library(splines)
library(corrplot)
library(ggpubr)
library(cobalt)

setwd("C:/Users/P088722/OneDrive - Amsterdam UMC/ClinHILI/data")

# read in data
data <- read.csv("repeat_preoperative.csv")

# exclude cases < 2 hours
#data <- data[data$duration_of_surgery > 120, ]

# exclude cases without mp data
data <- data[!is.na(data$twa_mp_peak), ]

# exclude cases without mp or cp data
data <- data[!is.na(data$twa_cp), ]

# exclude cases with mp > 30 J/min
#data <- data[data$twa_mp_peak < 30 & data$twa_mp_peak > 0, ]

# exclude cases without ppc outcome data
data <- data[!is.na(data$ppc), ]

# define quartiles of chemical power
cp_quartile_25  <- quantile(data$twa_cp, probs = 0.25, na.rm = TRUE)
cp_quartile_50  <- quantile(data$twa_cp, probs = 0.50, na.rm = TRUE)
cp_quartile_75  <- quantile(data$twa_cp, probs = 0.75, na.rm = TRUE)
cp_quartile_100 <- quantile(data$twa_cp, probs = 1.00, na.rm = TRUE)

data$quartile_cp[data$twa_cp >  0              & data$twa_cp <= cp_quartile_25]  <- 1
data$quartile_cp[data$twa_cp >  cp_quartile_25 & data$twa_cp <= cp_quartile_50]  <- 2
data$quartile_cp[data$twa_cp >  cp_quartile_50 & data$twa_cp <= cp_quartile_75]  <- 3
data$quartile_cp[data$twa_cp >  cp_quartile_75 & data$twa_cp <= cp_quartile_100] <- 4

median_cp <- median(data$twa_cp)
data$median_cp[data$twa_cp <= median_cp] <- 1
data$median_cp[data$twa_cp >  median_cp] <- 2

# mutate variables
data <- data %>%
  mutate(
    # Calculate and add BMI
    bmi = weight / ((height / 100) ^ 2),
    # Extract year of surgery
    year_surgery = as.factor(substring(date_randomization, 1, 4)),
    # Define factor variables
    gender = as.factor(gender),
    asa_score = as.factor(asa_score),
    trial = as.factor(trial),
    ariscat_respiratory_inf = as.factor(ariscat_respiratory_inf),
    anemia_preop = as.factor(anemia_preop),
    heart_failure = as.factor(heart_failure),
    copd = as.factor(copd),
    active_cancer = as.factor(active_cancer),
    type_surgery = as.factor(type_surgery),
    specific_procedure = as.factor(specific_procedure),
    surgical_approach = as.factor(surgical_approach),
    ariscat_emergency_proc = as.factor(ariscat_emergency_proc),
    ppc = as.factor(ppc),
    severe_ppc = as.factor(severe_ppc),
    quartile_cp = as.factor(quartile_cp),
    median_cp = as.factor(median_cp),
    twa_vt = as.numeric(twa_vt),
    twa_vtpbw = as.numeric(twa_vtpbw),
    twa_rr = as.numeric(twa_rr),
    twa_pplat = as.numeric(twa_pplat),
    twa_driving_plat = as.numeric(twa_driving_plat),
    twa_ppeak = as.numeric(twa_ppeak),
    twa_driving_peak = as.numeric(twa_driving_peak),
    twa_peep = as.numeric(twa_peep),
    twa_fio2 = as.numeric(twa_fio2),
    twa_cp = as.numeric(twa_cp),
    twa_mp_peak = as.numeric(twa_mp_peak),
    twa_mp_plat = as.numeric(twa_mp_plat),
    twa_mp_plat_peak = as.numeric(twa_mp_plat_peak)
  )

# Label the variables
label(data$age) <- "Age (y)"
label(data$gender) <- "Gender"
label(data$height) <- "Height (cm)"
label(data$weight) <- "Weight (kg)"
label(data$bmi) <- "Body Mass Index (BMI)"
label(data$asa_score) <- "ASA Score"
label(data$trial) <- "Trial"
label(data$ariscat_score) <- "ARISCAT score"
label(data$spo2) <- "Preoperative SPO2"
label(data$ariscat_respiratory_inf) <- "Respiratory Infection"
label(data$anemia_preop) <- "Preoperative Anemia"
label(data$heart_failure) <- "Heart Failure"
label(data$copd) <- "COPD"
label(data$active_cancer) <- "Active Cancer"
label(data$type_surgery) <- "Type of Surgery"
label(data$specific_procedure) <- "Specific Procedure"
label(data$surgical_approach) <- "Surgical Approach"
label(data$year_surgery) <- "Year of Surgery"
label(data$ariscat_emergency_proc) <- "Emergency Procedure"
label(data$duration_of_surgery) <- "Duration of Surgery"
label(data$ppc) <- "PPC"
label(data$severe_ppc) <- "Severe PPC"
label(data$quartile_cp) <- "Quartiles"
label(data$twa_vt) <- "Tidal volume (mL)"
label(data$twa_vtpbw) <- "Tidal volume (mL/kg PBW)"
label(data$twa_rr) <- "Respiratory rate (breaths/min)"
label(data$twa_pplat) <- "Plateau pressure (cmH20)"
label(data$twa_driving_plat) <- "Static driving pressure (cmH2O)"
label(data$twa_ppeak) <- "Peak pressure (cmH20)"
label(data$twa_driving_peak) <- "Dynamic driving pressure (cmH2O)"
label(data$twa_peep) <- "PEEP (cmH2O)"
label(data$twa_fio2) <- "FiO2 (%)"
label(data$twa_cp) <- "Chemical Power [J.min-1]"
label(data$twa_mp_peak) <- "Dynamic Mechanical Power [J.min-1]"
label(data$twa_mp_plat) <- "Static Mechanical Power [J.min-1]"
label(data$twa_mp_plat_peak) <- "Mixed Mechanical Power [J.min-1]"

# Define list of model variables
var.list <- c("twa_mp_peak", "twa_cp", "age", "gender", "bmi", "asa_score", 
              "spo2", "ariscat_respiratory_inf", "anemia_preop", 
              "heart_failure", "copd", "active_cancer",
              "surgical_approach", "ariscat_emergency_proc", 
              "twa_peep", "duration_of_surgery")

# count missingness in model variables
table1( ~ age + 
          gender + 
          bmi + 
          asa_score + 
          spo2 +
          ariscat_respiratory_inf +
          anemia_preop +
          heart_failure +
          copd +
          active_cancer + 
          surgical_approach +
          ariscat_emergency_proc +
          twa_peep +
          duration_of_surgery, 
          data = data,
          caption = "Model variables with missingness")

# Exclude cases with missing data for model variables
data <- data %>% 
  filter(across(all_of(var.list), ~ !is.na(.)))

# Define custom render functions
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("Mean (SD)"=sprintf("%s (&plusmn;%s)", MEAN, SD)))
}

my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f%%)", FREQ, PCT))))
}

# table 0: median cp and mp for inclusion in body text
tbl0 <- table1( ~ twa_cp + twa_mp_peak, 
                data = data,
                caption = "Intraoperative chemical and mechanical power")

# save table 0
t1flex(tbl0) %>% 
  save_as_docx(path="mp_peak_revised_allpat/tables/table0.docx")


# boxplots chemical and mechanical power
plot1 <- ggplot(data, aes(x = ppc, y = twa_mp_peak)) + 
  geom_boxplot(aes(group = ppc)) +
  scale_x_discrete(labels = c("0" = "no", "1" = "yes")) +
  labs(x = "PPC", y = expression("Mechanical Power [J." * min^{-1} * "]"), title = "") + 
  theme_minimal()

plot2 <- ggplot(data, aes(x = ppc, y = twa_cp)) + 
  geom_boxplot(aes(group = ppc)) + 
  scale_x_discrete(labels = c("0" = "no", "1" = "yes")) +
  labs(x = "PPC", y = expression("Chemical Power [J." * min^{-1} * "]"), title = "") + 
  theme_minimal()

# Combine the plots side by side
combined_plot <- plot1 + plot2

# save the combined plot
ggsave("mp_peak_revised_allpat/figures/figure_power_ppc.jpg", units="cm", width=15, height=10, dpi=300)


# Reshape the data to long format
data_long <- data %>%
  pivot_longer(cols = c(twa_cp, twa_mp_peak), 
               names_to = "variable", 
               values_to = "power")

# Create the combined boxplot
combined_plot <- ggplot(data_long, aes(x = variable, y = power)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c("twa_cp" = "Chemical Power", "twa_mp_peak" = "Mechanical Power")) +
  labs(x = "", y = expression("Power [J." * min^{-1} * "]"), title = "") + 
  theme_minimal()

# save the combined plot
ggsave("mp_peak_revised_allpat/figures/figure_power.jpg", units="cm", width=15, height=10, dpi=300)


# table 1: patient characteristics stratified by median cp
(tbl1 <- table1( ~ age + 
          gender + 
          height + 
          weight + 
          bmi + 
          asa_score + 
          trial + 
          ariscat_score + 
          spo2 + 
          ariscat_respiratory_inf + 
          anemia_preop +
          heart_failure +
          copd + 
          active_cancer +
          hb_preop +
          type_surgery +
          specific_procedure +
          surgical_approach +
          ariscat_emergency_proc + 
          duration_of_surgery +
          ppc +
          severe_ppc | median_cp, 
        data = data,
        caption = "Patient characteristics", 
        render.continuous=my.render.cont, 
        render.categorical=my.render.cat))

# save table 1
t1flex(tbl1) %>% 
  save_as_docx(path="mp_peak_revised_allpat/tables/table1.docx")

# create table 1 with ASD
vars <- c(
  "age", 
  "gender", 
  "height", 
  "weight", 
  "bmi", 
  "asa_score", 
  "trial", 
  "ariscat_score", 
  "spo2", 
  "ariscat_respiratory_inf", 
  "anemia_preop",
  "heart_failure",
  "copd", 
  "active_cancer",
  "hb_preop",
  "specific_procedure",
  "surgical_approach",
  "ariscat_emergency_proc", 
  "duration_of_surgery",
  "ppc",
  "severe_ppc"
)

tbl1_asd <- CreateTableOne(vars = vars, strata = "median_cp", data = data, test = FALSE)
print(tbl1_asd, smd = TRUE)

# table 2: Ventilation parameters stratified by median cp
tbl2 <- table1( ~ twa_vt +
          twa_vtpbw +
          twa_rr + 
          twa_pplat +
          twa_driving_plat +
          twa_ppeak +
          twa_driving_peak +
          twa_peep +
          twa_fio2 +
          twa_cp + 
          twa_mp_peak +
          twa_mp_plat +
          twa_mp_plat_peak
          |median_cp, 
          data = data,
          caption = "Ventilation parameters", 
          render.continuous=my.render.cont, 
          render.categorical=my.render.cat)

# save table 2
t1flex(tbl2) %>% 
  save_as_docx(path="mp_peak_revised_allpat/tables/table2.docx")

# create table 2 with SMD
vars <- c(
  "twa_vt",
  "twa_vtpbw",
  "twa_rr", 
  "twa_pplat",
  "twa_driving_plat",
  "twa_ppeak",
  "twa_driving_peak",
  "twa_peep",
  "twa_fio2",
  "twa_cp", 
  "twa_mp_peak",
  "twa_mp_plat",
  "twa_mp_plat_peak"
)

tbl2_asd <- CreateTableOne(vars = vars, strata = "median_cp", data = data, test = FALSE)
print(tbl2_asd, smd = TRUE)


# Inferential statistics

# Primary and secondary aims

# Check for collinearity among model variables
# Convert to numeric

# Define function: Convert character variables to factors, then to numeric
convert_to_numeric <- function(x) {
  if (is.character(x)) {
    return(as.numeric(factor(x)))
  } else if (is.factor(x)) {
    return(as.numeric(x))
  } else {
    return(x)  # Already numeric
  }
}

var.data <- data[,var.list]

var.data <- var.data %>% 
  mutate(across(everything(), convert_to_numeric))

# 1. Correlation Matrix
cor_matrix <- cor(var.data, use = "complete.obs")
jpeg(filename = "mp_peak_revised_allpat/figures/correlation_plot.jpg",  width = 1000, height = 800, quality = 95)
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.8)
dev.off()

# adjusted model
prim.model.adj.int <- glm(ppc ~ twa_mp_peak * twa_cp + 
                        age + 
                        gender + 
                        bmi +
                        asa_score +
                        spo2 +
                        ariscat_respiratory_inf +
                        anemia_preop +
                        heart_failure +
                        copd +
                        active_cancer +
                        surgical_approach +
                        ariscat_emergency_proc +
                        twa_peep +
                        duration_of_surgery,
                      family=binomial, 
                      data=data)

(regression_table <- prim.model.adj.int %>%
    tbl_regression(exponentiate = TRUE))

# save regression table
regression_flextable <- as_flex_table(regression_table)
save_as_docx(regression_flextable, path = "mp_peak_revised_allpat/tables/regression_table_linear_model_interaction.docx")

# adjusted model
prim.model.adj <- glm(ppc ~ twa_mp_peak + twa_cp + 
                        age + 
                        gender + 
                        bmi +
                        asa_score +
                        spo2 +
                        ariscat_respiratory_inf +
                        anemia_preop +
                        heart_failure +
                        copd +
                        active_cancer +
                        surgical_approach +
                        ariscat_emergency_proc +
                        twa_peep +
                        duration_of_surgery,
                      family=binomial, 
                      data=data)

(regression_table <- prim.model.adj %>%
    tbl_regression(exponentiate = TRUE))

# save regression table
regression_flextable <- as_flex_table(regression_table)
save_as_docx(regression_flextable, path = "mp_peak_revised_allpat/tables/regression_table_linear_model.docx")

# predict using the model
data$predictions <- predict(prim.model.adj, newdata = data, type = "response")

# plot predictions over mp
(mp_ppc <- ggplot(data, aes(twa_mp_peak, predictions)) +
  geom_point(alpha=0.5, color="darkblue") + 
  geom_smooth(color = "darkred") +
  labs(x = expression("Mechanical Power [J." * min^{-1} * "]"),
       y = "") +
  theme_minimal())

ggsave("mp_peak_revised_allpat/figures/figure_mp_ppc.jpg", units="cm", width=15, height=10, dpi=300)

# plot predictions over cp
(cp_ppc <- ggplot(data, aes(twa_cp, predictions)) +
  geom_point(alpha=0.5, color="darkblue") + 
  geom_smooth(color = "darkred") +
  labs(x = expression("Chemical Power [J." * min^{-1} * "]"),
       y = "Probability of PPCs") +
  theme_minimal())

ggsave("mp_peak_revised_allpat/figures/figure_cp_ppc.jpg", units="cm", width=15, height=10, dpi=300)

# combined plot predictions over cp and mp
ggarrange(cp_ppc, mp_ppc,
          ncol = 2, nrow = 1,
          labels = c("A", "B"),    # Label the plots as A and B
          font.label = list(size = 14, face = "bold"))             # Align plots verticallyggarrange()

ggsave("mp_peak_revised_allpat/figures/figure_cp_mp_ppc.jpg", units="cm", width=30, height=10, dpi=300)


# FIXED INTERVAL BINNING and HEATMAP

# Define bin edges
mp_bin <- c(min(data$twa_mp_peak), 10, 19, max(data$twa_mp_peak))
cp_bin <- c(min(data$twa_cp), 10, 15, max(data$twa_cp))

# Bin the data
data.binned <- data %>%
  mutate(mp_bin = cut(twa_mp_peak, breaks = mp_bin, include.lowest = TRUE),
         cp_bin = cut(twa_cp, breaks = cp_bin, include.lowest = TRUE))

# Calculate proportions
data.binned$ppc <- as.numeric(data.binned$ppc)
data.binned$ppc[data.binned$ppc == 1] <- 0
data.binned$ppc[data.binned$ppc == 2] <- 1
bin_summary <- data.binned %>%
  group_by(mp_bin, cp_bin) %>%
  summarise(
    total = n(),
    response_sum = sum(ppc, na.rm = TRUE),
    proportion = response_sum / total,
    probabilities_sum = sum(predictions, na.rm = TRUE),
    probabilities = probabilities_sum / total
  ) %>%
  ungroup()

# Pivot data for heatmap
plot_data <- bin_summary %>%
  pivot_longer(cols = probabilities, names_to = "metric", values_to = "value")

# define bin labels
mp_bin_labels <- c("<10", "10-19", ">19")
cp_bin_labels <- c("<10", "10-15", ">15")

# Plot heatmap
ggplot(plot_data, aes(x = mp_bin, y = cp_bin, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = c("lightyellow", "darkred"),
    values = scales::rescale(c(0, 0.28, 0.5)),
    guide = "colorbar"
  ) +  
  geom_text(aes(label = total), color = "black", size = 5) + # Add counts as text labels
  labs(x = expression("Mechanical Power [J." * min^{-1} * "]"),
       y = expression("Chemical Power [J." * min^{-1} * "]"),
       fill = "Probability of PPCs") +
  scale_x_discrete(labels = mp_bin_labels) +  # Auto labels from bin levels
  scale_y_discrete(labels = cp_bin_labels) +  # Auto labels from bin levels
  theme_minimal()

ggsave("mp_peak_revised_allpat/figures/figure_interaction_fixed_heatmap.jpg", units="cm", width=15, height=10, dpi=300)
ggsave("mp_peak_revised_allpat/figures/figure_interaction_quantile_heatmap_4x4.jpg", units="cm", width=15, height=10, dpi=300)