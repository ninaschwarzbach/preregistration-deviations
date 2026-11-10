
# =============================================================================
# Deviating from Preregistration: Does It Hurt Researchers' Credibility?
# Analysis script
#
# Reproduces the descriptive results, tables and figures reported in the
# manuscript and the supplementary materials.
#
# Input:  data/Data_PreregDevation_OSF.txt   (also available on OSF)
# Output: results are printed to the console; figures are saved in figures/
#
# Note: open-text responses are not shared to protect participants' privacy.
# Code that needs these responses is commented out. The text data are
# available upon request.
# =============================================================================


# 0. Setup --------------------------------------------------------------------

rm(list = ls())

# Set the working directory to the folder that contains this script.
# (Not needed if you open the project via preregistration-deviations.Rproj.)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getActiveDocumentContext()$path
  if (nzchar(script_path)) setwd(dirname(script_path))
}

library(tidyverse)  # dplyr, tidyr, ggplot2, forcats, ...
library(ggstats)    # position_likert(), label_percent_abs() for Figure 1

# Folder for the figures
dir.create("figures", showWarnings = FALSE)

# Load data
dat <- read.table("data/Data_PreregDevation_OSF.txt", header = TRUE)


# Helper functions ------------------------------------------------------------

# Print a header in the console, so the output is easy to find
section <- function(title) cat("\n\n===== ", title, " =====\n\n", sep = "")

# Multiple-choice questions are stored as one string per participant, with the
# selected options separated by commas. This function turns them into a data
# frame with one column per option (the option if selected, NA otherwise).
split_multiselect <- function(strings, first_option, n_columns) {
  split_strings <- strsplit(strings, ",")
  all_options   <- unique(unlist(split_strings))
  all_options   <- c(first_option, setdiff(all_options, first_option))
  extra_columns <- if (length(all_options) < n_columns) {
    paste0("Extra", seq_len(n_columns - length(all_options)))
  } else {
    character(0)
  }
  columns <- c(all_options, extra_columns)[1:n_columns]

  result <- data.frame(matrix(NA, nrow = length(strings), ncol = n_columns))
  colnames(result) <- columns
  for (i in seq_along(split_strings)) {
    for (option in split_strings[[i]]) {
      if (option %in% colnames(result)) result[i, option] <- option
    }
  }
  result
}

# Count how often each option was selected, per experience group
count_by_group <- function(result, group) {
  counts <- matrix(NA, ncol = 4, nrow = ncol(result))
  colnames(counts) <- c("Variable", "No experience", "Preregistration",
                        "Preregistration and Deviation")
  for (k in seq_len(ncol(result))) {
    counts[k, 1]   <- colnames(result)[k]
    counts[k, 2:4] <- table(group, result[[k]])
  }
  counts
}


# 1. Sample description -------------------------------------------------------

section("Survey duration (minutes)")
dat$duration_min <- as.numeric(dat$Duration..in.seconds.) / 60
print(c(mean   = mean(dat$duration_min),
        sd     = sd(dat$duration_min),
        median = median(dat$duration_min),
        min    = min(dat$duration_min),
        max    = max(dat$duration_min)))

section("Experience with preregistration, deviations and reporting")
print(table(dat$prereg_experience, useNA = "ifany"))
print(table(dat$dev_experience))     # only asked if experience with preregistration
print(table(dat$report_experience))  # only asked if experience with deviations

section("Training in preregistration")
print(table(dat$training, useNA = "ifany"))

# Experience groups
# 0 = no experience with preregistration
# 1 = experience with preregistration, not with deviations
# 2 = experience with preregistration and deviations
dat$group_member <- ifelse(dat$prereg_experience == "No", 0,
                           ifelse(dat$prereg_experience == "Yes" & dat$dev_experience == "Yes", 2, 1))
dat$group <- ifelse(dat$group_member == 0, "No experience",
                    ifelse(dat$group_member == 1, "Preregistration",
                           "Preregistration and Deviation"))


# 2. Expected consequences of reporting deviations (Table 1) ------------------

# Answers coded as -1 (less), 0 (the same), 1 (more).
# Note: this treats the three-point scale as interval-scaled.
dat$report_consequences_1_num <- ifelse(dat$report_consequences_1 == "less", -1,
                                        ifelse(dat$report_consequences_1 == "more", 1, 0)) # publishing chances
dat$report_consequences_2_num <- ifelse(dat$report_consequences_2 == "less", -1,
                                        ifelse(dat$report_consequences_2 == "more", 1, 0)) # credibility
dat$report_consequences_3_num <- ifelse(dat$report_consequences_3 == "less", -1,
                                        ifelse(dat$report_consequences_3 == "more", 1, 0)) # trustworthiness

describe_by_group <- function(formula) {
  out <- as.data.frame(as.matrix(aggregate(formula, data = dat,
                                           function(x) c(n = NROW(x),
                                                         mean = round(mean(x), 2),
                                                         sd = round(sd(x), 2)))))
  colnames(out) <- c("group", "n", "mean", "sd")
  out$mean  <- as.numeric(out$mean)
  out$sd    <- as.numeric(out$sd)
  out$group <- c("No exp", "Pre", "P+D")
  out
}

desc_pub1   <- describe_by_group(report_consequences_1_num ~ group)
desc_cred2  <- describe_by_group(report_consequences_2_num ~ group)
desc_trust3 <- describe_by_group(report_consequences_3_num ~ group)

section("Table 1: Perceived impact of reporting deviations, M (SD)")
cat("Publishing chance\n");   print(desc_pub1)
cat("\nCredibility\n");       print(desc_cred2)
cat("\nTrustworthiness\n");   print(desc_trust3)

section("Expected effect on publishing chances (% of all participants)")
print(round(table(dat$report_consequences_1) / nrow(dat) * 100, 1))


# 3. Concerns about professional reputation (Figure 1) ------------------------

# Q6: concerned about reputation for deviating (experience with deviations)
# Q9.3: would be concerned about reputation if deviating (no experience)
# Q8: concerned about reputation for NOT reporting deviations
section("Reputation items by group (raw counts)")
print(table(dat$group, dat$dev_reputation_1))
print(table(dat$group, dat$prereg_barriers_2))

likert_to_num <- function(x) {
  ifelse(x == "I agree", 4,
         ifelse(x == "I rather agree", 3,
                ifelse(x == "I rather don't agree", 2,
                       ifelse(x == "I don't agree", 1, NA))))  # "I don't know" -> NA
}
dat$dev_reputation_1_num    <- likert_to_num(dat$dev_reputation_1)     # Q6
dat$prereg_barriers_2_num   <- likert_to_num(dat$prereg_barriers_2)    # Q9.3
dat$report_reputation_1_num <- likert_to_num(dat$report_reputation_1)  # Q8.1
dat$report_reputation_2_num <- likert_to_num(dat$report_reputation_2)  # Q8.2

describe_num <- function(x) {
  c(n = sum(!is.na(x)), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
}
section("Concern about reputation for deviating")
cat("Experience with preregistration and deviations:\n"); print(describe_num(dat$dev_reputation_1_num))
cat("No experience with preregistration:\n");             print(describe_num(dat$prereg_barriers_2_num))

section("Concern about reputation for not reporting deviations (Q8)")
cat("Reported deviations (Q8.1):\n");         print(describe_num(dat$report_reputation_1_num))
cat("Did not report deviations (Q8.2):\n");   print(describe_num(dat$report_reputation_2_num))

# Data for Figure 1
data_repplot <- rbind(
  data.frame(Response = dat$dev_reputation_1,
             Experience = "Experience with preregistration and deviation"),
  data.frame(Response = dat$prereg_barriers_2,
             Experience = "No experience with preregistration and deviation")
)
data_repplot <- data_repplot[complete.cases(data_repplot), ]

response_levels <- c("I don't agree", "I rather don't agree", "I rather agree", "I agree")
data_repplot$Response <- factor(data_repplot$Response, levels = response_levels)  # "I don't know" -> NA
data_repplot$Experience <- factor(
  data_repplot$Experience,
  levels = c("No experience with preregistration and deviation",
             "Experience with preregistration and deviation"),
  labels = c("No experience with\npreregistration and deviation",
             "Experience with\npreregistration and deviation")
)

data_repplot_prop <- data_repplot %>%
  filter(!is.na(Response)) %>%
  count(Experience, Response) %>%
  complete(Experience, Response, fill = list(n = 0)) %>%
  group_by(Experience) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

likert_colors <- c(
  "I don't agree"        = "#CD7F32",
  "I rather don't agree" = "#DDC3A9",
  "I rather agree"       = "#99D0CD",
  "I agree"              = "#03A89E"
)

# Show percentages only for segments of at least 7.5%
custom_label <- function(x) {
  p <- scales::percent(x, accuracy = 1)
  p[x < .075] <- ""
  p
}

figure1 <- ggplot(data_repplot_prop, aes(y = Experience, x = prop, fill = Response)) +
  geom_col(position = position_likert(cutoff = 2)) +
  geom_text(aes(label = custom_label(prop)),
            position = position_likert(vjust = 0.5), size = 4) +
  scale_x_continuous(labels = label_percent_abs()) +
  scale_fill_manual(values = likert_colors, na.translate = FALSE) +
  labs(x = "Proportion within group", y = NULL,
       fill = "Concerns about reputation",
       title = "Perceived Influence of Deviation on Professional Reputation by Experience") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y  = element_text(size = 14),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title   = element_text(size = 16, hjust = 0.5))

ggsave("figures/Figure1_reputation_concerns.png", figure1,
       width = 4000, height = 1500, units = "px", dpi = 300, bg = "white")


# 4. Perceived credibility and trustworthiness (Figure 2) ---------------------

# One credibility and one trustworthiness rating per participant
# (each participant saw one of the four scenarios)
dat$crediblestudy <- as.numeric(ifelse(dat$scenario_version == "A", dat$assessed_cr_tr_a_1,
                                ifelse(dat$scenario_version == "B", dat$assessed_cr_tr_b_1,
                                ifelse(dat$scenario_version == "C", dat$assessed_cr_tr_c_1,
                                                                    dat$assessed_cr_tr_d_1))))
dat$trustworthyauthor <- as.numeric(ifelse(dat$scenario_version == "A", dat$assessed_cr_tr_a_2,
                                    ifelse(dat$scenario_version == "B", dat$assessed_cr_tr_b_2,
                                    ifelse(dat$scenario_version == "C", dat$assessed_cr_tr_c_2,
                                                                        dat$assessed_cr_tr_d_2))))

section("Credibility and trustworthiness per scenario, n / M / SD")
desc.cred  <- aggregate(crediblestudy ~ scenario_version, data = dat,
                        function(x) c(n = NROW(x), mean = round(mean(x), 1), sd = round(sd(x), 1)))
desc.trust <- aggregate(trustworthyauthor ~ scenario_version, data = dat,
                        function(x) c(n = NROW(x), mean = round(mean(x), 1), sd = round(sd(x), 1)))
cat("Credibility\n");        print(desc.cred)
cat("\nTrustworthiness\n");  print(desc.trust)

# Long format: one row per participant and rating type
dat_long <- dat %>%
  select(group, scenario_version, crediblestudy, trustworthyauthor) %>%
  pivot_longer(cols = c(crediblestudy, trustworthyauthor),
               names_to = "RatingType", values_to = "Rating") %>%
  filter(!is.na(Rating)) %>%
  mutate(
    group      = factor(group, levels = c("No experience", "Preregistration",
                                          "Preregistration and Deviation")),
    Scenario   = factor(scenario_version, levels = c("A", "B", "C", "D")),
    RatingType = ifelse(RatingType == "crediblestudy", "Credibility", "Trustworthiness"),
    # Factors of the 2x2 design
    Preregistration = ifelse(Scenario %in% c("B", "C"), "Yes", "No"),
    Deviation       = ifelse(Scenario %in% c("C", "D"), "Yes", "No")
  )

section("Supplementary Table 1: Ratings by scenario and experience group")
summary_table_scenarios <- dat_long %>%
  group_by(RatingType, Scenario, group) %>%
  summarise(N = n(),
            Mean = round(mean(Rating), 1),
            SD = round(sd(Rating), 1),
            .groups = "drop")
print(summary_table_scenarios, n = Inf)

section("Ratings by factor of the 2x2 design (deviation reported / preregistered)")
print(dat_long %>%
        group_by(RatingType, Deviation) %>%
        summarise(N = n(), Mean = round(mean(Rating), 1), SD = round(sd(Rating), 1),
                  .groups = "drop"))
print(dat_long %>%
        group_by(RatingType, Preregistration) %>%
        summarise(N = n(), Mean = round(mean(Rating), 1), SD = round(sd(Rating), 1),
                  .groups = "drop"))

section("Differences between scenarios (raw and standardized with pooled SD)")
cohens_d <- function(x, y) {
  pooled_sd <- sqrt(((length(x) - 1) * var(x) + (length(y) - 1) * var(y)) /
                      (length(x) + length(y) - 2))
  (mean(x) - mean(y)) / pooled_sd
}
scenario_pairs <- combn(c("A", "B", "C", "D"), 2)
for (rating in c("Credibility", "Trustworthiness")) {
  cat(rating, "\n")
  for (k in seq_len(ncol(scenario_pairs))) {
    s1 <- scenario_pairs[1, k]
    s2 <- scenario_pairs[2, k]
    x  <- dat_long$Rating[dat_long$RatingType == rating & dat_long$Scenario == s1]
    y  <- dat_long$Rating[dat_long$RatingType == rating & dat_long$Scenario == s2]
    cat(sprintf("  %s vs %s: difference = %5.1f, d = %5.2f\n",
                s1, s2, mean(x) - mean(y), cohens_d(x, y)))
  }
}

section("Correlation between study credibility and author trustworthiness")
print(cor(dat$trustworthyauthor, dat$crediblestudy))
correlations <- dat %>%
  group_by(group) %>%
  summarise(correlation = cor(trustworthyauthor, crediblestudy))
print(correlations)

# Figure 2: boxplots per scenario and experience group; diamonds show the means
figure2 <- ggplot(dat_long, aes(x = Scenario, y = Rating, fill = group)) +
  geom_boxplot(position = position_dodge(width = 0.7), width = 0.6,
               outlier.shape = NA, color = "black") +
  stat_summary(aes(group = group), fun = mean, geom = "point",
               shape = 23, size = 2.5, fill = "white", color = "black",
               position = position_dodge(width = 0.7)) +
  facet_wrap(~ RatingType, ncol = 1) +
  scale_fill_manual(values = c("No experience"                 = "#006D67",
                               "Preregistration"               = "#DDC3A9",
                               "Preregistration and Deviation" = "#CD7F32"),
                    name = "Experience") +
  labs(title = "Perceived Credibility and Trustworthiness by Scenario",
       x = "Scenario", y = "Rating") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(hjust = 1),
        strip.text  = element_text(size = 13),
        plot.title  = element_text(size = 16, hjust = 0.5))

ggsave("figures/Figure2_credibility_trustworthiness.png", figure2,
       width = 3000, height = 2000, units = "px", dpi = 300, bg = "white")


# 5. Evaluation of the deviation (Table 2, Supplementary Table 3) -------------

section("Table 2: Evaluation of the deviation (Scenarios C and D)")
print(table(dat$fishy_c)); print(sum(!is.na(dat$fishy_c)))
print(table(dat$fishy_d)); print(sum(!is.na(dat$fishy_d)))

section("Table 2 split by experience group")
print(table(dat$fishy_c, dat$group))
print(table(dat$fishy_d, dat$group))

section("Supplementary Table 3: Credibility by evaluation of the deviation")
cred_c <- aggregate(crediblestudy ~ fishy_c, data = dat,
                    FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                        sd   = sd(x, na.rm = TRUE),
                                        n    = sum(!is.na(x))))
cred_d <- aggregate(crediblestudy ~ fishy_d, data = dat,
                    FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                        sd   = sd(x, na.rm = TRUE),
                                        n    = sum(!is.na(x))))
cat("Scenario C\n"); print(cred_c)
cat("\nScenario D\n"); print(cred_d)


# 6. Factors considered when assessing credibility and trustworthiness --------
#    (Figure 3, Supplementary Figure 1)

# Select the answer for the scenario each participant saw
dat$cred_factors <- ifelse(dat$scenario_version == "A", dat$cr_factors_a,
                    ifelse(dat$scenario_version == "B", dat$cr_factors_b,
                    ifelse(dat$scenario_version == "C", dat$cr_factors_c,
                                                        dat$cr_factors_d)))
dat$trust_factors <- ifelse(dat$scenario_version == "A", dat$tr_factors_a,
                     ifelse(dat$scenario_version == "B", dat$tr_factors_b,
                     ifelse(dat$scenario_version == "C", dat$tr_factors_c,
                                                         dat$tr_factors_d)))
# Open-text answers to the "other" option (not shared):
# dat$cred_factors_text <- ifelse(dat$scenario_version == "A", dat$cr_factors_a_9_TEXT,
#                          ifelse(dat$scenario_version == "B", dat$cr_factors_b_9_TEXT,
#                          ifelse(dat$scenario_version == "C", dat$cr_factors_c_9_TEXT,
#                                                              dat$cr_factors_d_9_TEXT)))
# dat$trust_factors_text <- ifelse(dat$scenario_version == "A", dat$tr_factors_a_9_TEXT,
#                           ifelse(dat$scenario_version == "B", dat$tr_factors_b_9_TEXT,
#                           ifelse(dat$scenario_version == "C", dat$tr_factors_c_9_TEXT,
#                                                               dat$tr_factors_d_9_TEXT)))

result.cred  <- split_multiselect(dat$cred_factors,  "sample size", 9)
result.trust <- split_multiselect(dat$trust_factors, "sample size", 9)

section("Factors considered: credibility (count and % of participants)")
counts_cred  <- colSums(!is.na(result.cred))
factors.cred <- cbind(counts_cred, perc_cred = round(counts_cred / nrow(result.cred) * 100, 1))
print(factors.cred)

section("Factors considered: trustworthiness (count and % of participants)")
count_trust   <- colSums(!is.na(result.trust))
factors.trust <- cbind(count_trust, perc_trust = round(count_trust / nrow(result.trust) * 100, 1))
print(factors.trust)

# Counts per experience group
counts_cred_group  <- as.data.frame(sapply(split(result.cred, dat$group),
                                           function(x) colSums(!is.na(x))))
counts_trust_group <- as.data.frame(sapply(split(result.trust, dat$group),
                                           function(x) colSums(!is.na(x))))

cred_long <- counts_cred_group %>%
  rownames_to_column("Item") %>%
  pivot_longer(-Item, names_to = "Experience", values_to = "Count") %>%
  mutate(Dimension = "Credibility")
trust_long <- counts_trust_group %>%
  rownames_to_column("Item") %>%
  pivot_longer(-Item, names_to = "Experience", values_to = "Count") %>%
  mutate(Dimension = "Trustworthiness")
combined_long <- bind_rows(cred_long, trust_long)
combined_long$Item <- factor(combined_long$Item)

# One color per answer option (in the alphabetical order of the options)
item_colors <- setNames(
  c("#006D67", "#039B95", "#03A89E", "#6FC5C3", "#BFDCDC",
    "#DDC3A9", "#C8997B", "#C8995A", "#CD7F32")[seq_along(levels(combined_long$Item))],
  levels(combined_long$Item)
)

# Figure 3: all participants
plot_data <- combined_long %>%
  group_by(Dimension, Item) %>%
  summarise(Count = sum(Count), .groups = "drop")

figure3 <- ggplot(plot_data, aes(x = Count, y = Dimension, fill = Item)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5),
            size = 4, color = "black") +
  scale_fill_manual(values = item_colors) +
  labs(title = "Factors Contributing to Perceived Credibility and Trustworthiness",
       x = "Number of responses", y = NULL, fill = "Item") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y     = element_text(size = 14),
        legend.position = "right",
        plot.title      = element_text(size = 16, hjust = 0.5))

ggsave("figures/Figure3_factors_credibility_trustworthiness.png", figure3,
       width = 4000, height = 1500, units = "px", dpi = 300, bg = "white")

# Supplementary Figure 1: split by experience group
plot_data_split <- combined_long %>%
  group_by(Dimension, Item, Experience) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  mutate(Experience = factor(Experience, levels = c("No experience", "Preregistration",
                                                    "Preregistration and Deviation")))

figureS1 <- ggplot(plot_data_split, aes(x = Count, y = Dimension, fill = Item)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5),
            size = 3.8, color = "black") +
  scale_fill_manual(values = item_colors) +
  facet_wrap(~ Experience, ncol = 1) +
  labs(title = "Factors Contributing to Perceived Credibility and Trustworthiness by Experience",
       x = "Number of responses", y = NULL, fill = "Item") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y     = element_text(size = 14),
        legend.position = "right",
        plot.title      = element_text(size = 16, hjust = 0.5),
        strip.text      = element_text(size = 13))

ggsave("figures/FigureS1_factors_by_experience.png", figureS1,
       width = 4000, height = 1800, units = "px", dpi = 300, bg = "white")


# 7. Perceived prevalence of deviations (Table 3) -----------------------------

dat$practices_perception_1 <- as.numeric(dat$practices_perception_1)
dat$practices_perception_2 <- as.numeric(dat$practices_perception_2)
dat$practices_perception_3 <- as.numeric(dat$practices_perception_3)

section("Table 3: Perceived prevalence, full sample (mean, SD, number missing)")
for (v in c("practices_perception_1", "practices_perception_2", "practices_perception_3")) {
  cat(v, "\n")
  print(c(mean = mean(dat[[v]], na.rm = TRUE), sd = sd(dat[[v]], na.rm = TRUE),
          missing = sum(is.na(dat[[v]]))))
}

section("Table 3: Perceived prevalence by group")
desc_perception1 <- describe_by_group(practices_perception_1 ~ group)
desc_perception2 <- describe_by_group(practices_perception_2 ~ group)
desc_perception3 <- describe_by_group(practices_perception_3 ~ group)
print(desc_perception1); print(desc_perception2); print(desc_perception3)


# 8. Cultural norms (Figure 4, Supplementary Table 2) -------------------------

section("Institutional norms should motivate reporting deviations (full sample)")
print(table(dat$norms))
print(round(table(dat$norms) / nrow(dat) * 100, 1))
print(sum(is.na(dat$norms)))  # number of missing responses

section("Supplementary Table 2: Institutional norms by group (counts and row %)")
n.groups <- table(dat$group)
groupcounts_norms <- as.data.frame(table(dat$group, dat$norms))
groupcounts_norms$percentage <- round(
  ifelse(groupcounts_norms$Var1 == "No experience",
         groupcounts_norms$Freq / n.groups[[1]] * 100,
         ifelse(groupcounts_norms$Var1 == "Preregistration",
                groupcounts_norms$Freq / n.groups[[2]] * 100,
                groupcounts_norms$Freq / n.groups[[3]] * 100)), 1)
print(groupcounts_norms)

# Who requires / gives guidance on reporting deviations (multiple choice)
result.requirement <- split_multiselect(dat$requirement, "my PI or collaborators.", 5)
result.guidance    <- split_multiselect(dat$guidance,    "my PI or collaborators.", 5)

counts.requirement <- count_by_group(result.requirement, dat$group)
counts.guidance    <- count_by_group(result.guidance,    dat$group)

# Short labels for the five answer options, matched by the answer text
# (the order of the options differs between the two questions)
option_labels <- c(
  "my PI or collaborators."                                                      = "My PI or collaborators.",
  "the journal where I would like to submit my paper/I have submitted my paper." = "Journal",
  "the funder of my research project."                                           = "Funder",
  "else:"                                                                        = "Else",
  "none"                                                                         = "None"
)

section("Check: answer options and their labels in Figure 4")
print(data.frame(requirement_option = counts.requirement[, 1],
                 requirement_label  = unname(option_labels[counts.requirement[, 1]]),
                 guidance_option    = counts.guidance[, 1],
                 guidance_label     = unname(option_labels[counts.guidance[, 1]])))

# Open-text answers to the "else" option (not shared):
# requirement_text <- ifelse(dat$requirement_5_TEXT == "-99", NA, dat$requirement_5_TEXT)
# guidance_text    <- ifelse(dat$guidance_5_TEXT == "-99", NA, dat$guidance_5_TEXT)

to_long <- function(counts, source) {
  labels <- unname(option_labels[counts[, "Variable"]])
  if (anyNA(labels)) stop("Unknown answer option: ", counts[is.na(labels), "Variable"])
  data.frame(Factor     = rep(labels, times = 3),
             Experience = rep(colnames(counts)[2:4], each = nrow(counts)),
             Count      = as.numeric(counts[, 2:4]),
             Source     = source)
}
dat_require_melt  <- to_long(counts.requirement, "Requires Reporting Deviations")
dat_guidance_melt <- to_long(counts.guidance,    "Guidance with Reporting Deviations")

section("Figure 4: counts")
combined_faceted_data <- bind_rows(dat_guidance_melt, dat_require_melt)
combined_faceted_data$Factor <- factor(
  combined_faceted_data$Factor,
  levels = c("Funder", "Journal", "My PI or collaborators.", "Else", "None")
)
print(combined_faceted_data)

figure4 <- ggplot(combined_faceted_data, aes(y = Experience, x = Count, fill = Factor)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5),
            size = 5, color = "black") +
  scale_fill_manual(values = c("#CD7F32", "#DDC3A9", "#99D0CD", "#03A89E", "#006D67")) +
  facet_wrap(~ Source, ncol = 1) +
  labs(title = "Factors Involved in Reporting Deviations",
       x = "Number of Responses", y = NULL, fill = "Factor") +
  theme_minimal(base_size = 14) +
  theme(strip.text      = element_text(size = 14),
        axis.text.y     = element_text(size = 12),
        legend.position = "right",
        legend.text     = element_text(size = 12),
        plot.title      = element_text(size = 16, hjust = 0.5))

ggsave("figures/Figure4_norms_reporting_deviations.png", figure4,
       width = 4000, height = 1500, units = "px", dpi = 300, bg = "white")

section("Done")
cat("Figures saved in:", normalizePath("figures"), "\n")

