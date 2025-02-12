# load libraries
library(lme4)
library(mgcv)
library(tidyverse)
library(ggplot2)
library(performance)
library(sjPlot)
library(car)
library(emmeans)
library(ggpubr)
library(patchwork)
library(readxl)
library(dplyr)

# load the datasets
natives_data <- read_excel("natives_V2.xlsx")
stimuli_data <- read_excel("Stimuli.xlsx")

# add stimuli information to natives_data
merged_data <- natives_data %>%
  left_join(stimuli_data, by = "ID")

# tidy the dataset
tidy_data <- merged_data %>%
  select(RECORDING_SESSION_LABEL, 
         ID, 
         TRIAL_LABEL, 
         TRIAL_INDEX, 
         BIN_INDEX, 
         `AVERAGE_IA_1_SAMPLE_COUNT_%`, 
         `AVERAGE_IA_2_SAMPLE_COUNT_%`, 
         EXP, 
         cond)

# rename columns
tidy_data <- tidy_data %>%
  rename(
    participant = RECORDING_SESSION_LABEL,
    stimulus_id = ID,
    trial_label = TRIAL_LABEL,
    trial_index = TRIAL_INDEX,
    bin_index = BIN_INDEX,
    target_fixation = `AVERAGE_IA_1_SAMPLE_COUNT_%`,
    distractor_fixation = `AVERAGE_IA_2_SAMPLE_COUNT_%`,
    experiment = EXP,
    condition = cond
  )

# remove practice data
tidy_data <- tidy_data %>%
  filter(experiment != "PS")

# make sure categorical predictors are factors
tidy_data <- tidy_data %>%
  mutate(
    condition = as.factor(condition),  # Convert condition to factor
    participant = as.factor(participant),  # Ensure participant is a factor
    stimulus_id = as.factor(stimulus_id)   # Ensure stimulus_id is a factor
  )

# Check structure again
str(tidy_data)

# transform data
# convert fixation proportions to counts
tidy_data <- tidy_data %>%
  mutate(
    target_count = round(target_fixation * 100),
    distractor_count = round(distractor_fixation * 100),
    total_fixations = target_count + distractor_count
  )


str(tidy_data)

colSums(is.na(tidy_data))

# Fit the GAMM model using binomial regression
gamm_model <- bam(cbind(target_count, total_fixations - target_count) ~ 
                    s(bin_index, k=10) +                   # smooth term for time
                    condition +                            # lexical stress (paroxytone vs. oxytone)
                    s(bin_index, by=condition, k=10) +    # interaction: stress modulates time-course
                    s(participant, bs="fs") +             # by-participant random smooth
                    s(stimulus_id, bs="re"),              # by-item random intercept
                  family=binomial,                        # logistic regression for fixation proportion
                  data=tidy_data,
                  method="fREML")

# View model summary
summary(gamm_model)

# Compute mean target fixation per bin & condition
summary_data <- tidy_data %>%
  group_by(bin_index, condition) %>%
  summarise(
    mean_fixation = mean(target_fixation, na.rm = TRUE),
    se = sd(target_fixation, na.rm = TRUE) / sqrt(n()),  # Standard Error
    .groups = 'drop'
  )

# Adjust bin_index to reflect the actual time bins (from -500 ms to 750 ms)
summary_data <- summary_data %>%
  mutate(bin_index = bin_index * 50 - 500)  # Convert bin index to actual time in ms

# Define the plot
ggplot(summary_data, aes(x = bin_index, y = mean_fixation, color = factor(condition), fill = factor(condition))) +
  geom_line(size = 1.2) +                            # Mean fixation line
  geom_point(size = 2) +                             # Points for each bin
  geom_ribbon(aes(ymin = mean_fixation - se, ymax = mean_fixation + se), alpha = 0.2) +  # Confidence intervals
  geom_vline(xintercept = 200, linetype = "dashed", color = "black", size = 1) +  # Critical 200ms line
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "black", size = 1) +  # Chance level at 50%
  scale_x_continuous(breaks = seq(-500, 700, 50)) +  # Tick marks every 50 ms
  scale_y_continuous(limits = c(0, 1)) +  # Ensure y-axis stays between 0-1
  scale_color_manual(name = "Lexical Stress", values = c("1" = "blue", "2" = "red"), labels = c("Paroxytone", "Oxytone")) +
  scale_fill_manual(name = "Lexical Stress", values = c("1" = "blue", "2" = "red"), labels = c("Paroxytone", "Oxytone")) +
  labs(
    x = "Time Relative to First Syllable Offset (ms)",
    y = "Proportion of Target Fixations",
    color = "Lexical Stress",
    fill = "Lexical Stress"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )





