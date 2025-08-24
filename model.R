# ==== Setup ----
library(readr)
library(dplyr)
library(purrr)
library(broom)
library(lubridate)
library(marginaleffects)  # for avg_predictions

# Paths
input_csv  <- "/Users/toyhtoza/Desktop/CFPB/adi_combined.csv"
output_dir <- "/Users/toyhtoza/Desktop/CFPB"

# ==== Load & clean ----
df <- read_csv(input_csv, show_col_types = FALSE)

# Harmonize product categories (matches manuscript buckets)
df <- df %>%
  mutate(
    Product = recode(
      Product,
      "Prepaid card" = "Credit card or prepaid card",
      "Credit card" = "Credit card or prepaid card",
      "Vehicle loan or lease" = "Payday loan, title loan, personal loan, or advance loan",
      "Consumer Loan" = "Payday loan, title loan, personal loan, or advance loan",
      "Payday loan" = "Payday loan, title loan, personal loan, or advance loan",
      "Payday loan, title loan, or personal loan" = "Payday loan, title loan, personal loan, or advance loan",
      "Bank account or service" = "Checking or savings account",
      "Other financial service" = "Money transfer, virtual currency, or money service",
      "Money transfers" = "Money transfer, virtual currency, or money service",
      "Virtual currency" = "Money transfer, virtual currency, or money service",
      "Credit reporting" = "Credit reporting, credit repair services, or other personal consumer reports",
      "Credit reporting or other personal consumer reports" = "Credit reporting, credit repair services, or other personal consumer reports",
      "Debt or credit management" = "Credit reporting, credit repair services, or other personal consumer reports"
    )
  )

# Outcome
df <- df %>%
  mutate(resolved_timely = ifelse(`Timely response?` == "Yes", 1L, 0L))

# Tags -> groups (allow “Both”)
df <- df %>%
  mutate(
    Tags = ifelse(is.na(Tags), "None", Tags),
    Group_raw = case_when(
      Tags == "Older American, Servicemember" ~ "Both",
      Tags == "Older American"                ~ "Older American",
      Tags == "Servicemember"                 ~ "Servicemember",
      TRUE                                    ~ "Neither"
    ),
    Older_American = as.integer(Group_raw %in% c("Older American", "Both")),
    Servicemember  = as.integer(Group_raw %in% c("Servicemember", "Both"))
  )

# Factors
df <- df %>%
  mutate(
    State = factor(State),
    # robust date parse (mdy handles 2 or 4-digit years)
    `Date received` = suppressWarnings(mdy(`Date received`)),
    Year  = factor(year(`Date received`), levels = 2014:2022),
    Product = factor(Product)
  )

# Standardize ADI within the full dataset
df <- df %>%
  mutate(
    ADI_STATERNK_std = as.numeric(scale(ADI_STATERNK))
  )

# Analysis sample
df_clean <- df %>%
  filter(
    !is.na(resolved_timely),
    !is.na(ADI_STATERNK),
    !is.na(State),
    !is.na(Year),
    !is.na(Product)
  )

# Set reference levels (match manuscript: CA, checking/savings)
if ("CA" %in% levels(df_clean$State)) df_clean$State <- relevel(df_clean$State, ref = "CA")
if ("Checking or savings account" %in% levels(df_clean$Product)) {
  df_clean$Product <- relevel(df_clean$Product, ref = "Checking or savings account")
}

# ==== MODEL 4: Time interactions (used in the paper) ----
model4_time_interactions <- glm(
  resolved_timely ~
    Older_American + Servicemember +
    Year + ADI_STATERNK_std +
    Older_American:Year + Servicemember:Year +
    ADI_STATERNK_std:Older_American + ADI_STATERNK_std:Servicemember +
    State + Product,
  data = df_clean,
  family = binomial()
)

saveRDS(model4_time_interactions, file.path(output_dir, "model4_time_interactions.rds"))
print("=== MODEL 4 SUMMARY ===")
print(summary(model4_time_interactions))

# ==== Derived labels for outputs ----
# For outputs, map to manuscript labels (General population / Service-member / Older adult)
df_clean <- df_clean %>%
  mutate(
    Older_American = as.integer(grepl("Older American", Tags)),
    Servicemember  = as.integer(grepl("Servicemember",  Tags)),
    Group = case_when(
      Older_American == 1 & Servicemember == 0 ~ "Older adult",
      Servicemember  == 1 & Older_American == 0 ~ "Service-member",
      Older_American == 0 & Servicemember == 0 ~ "General population",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Group))

# ==== Output A: Group × Year predicted probabilities ----
# Average predicted probabilities by Group & Year using model 4
gy_levels <- df_clean %>% distinct(Group, Year)
group_year_preds <- map_dfr(seq_len(nrow(gy_levels)), function(i) {
  g <- gy_levels$Group[i]
  y <- gy_levels$Year[i]
  sub <- df_clean %>% filter(Group == g, Year == y)
  if (nrow(sub) < 10) return(NULL)
  ap <- avg_predictions(model4_time_interactions, newdata = sub, type = "response")
  tibble(
    group = g,
    year  = as.integer(as.character(y)),
    estimate  = ap$estimate,
    conf.low  = ap$conf.low,
    conf.high = ap$conf.high
  )
})

group_year_path <- file.path(output_dir, "predicted_probabilities_by_group_year.csv")
write.csv(group_year_preds, group_year_path, row.names = FALSE)
message("Wrote: ", group_year_path)

# ==== Output B: State-level changes (Early 2014–2017 vs Modern 2018–2022) ----
early_years  <- as.character(2014:2017)
modern_years <- as.character(2018:2022)

main_states <- c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA',
                 'HI','ID','IL','IN','IA','KS','KY','LA','ME','MD',
                 'MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ',
                 'NM','NY','NC','ND','OH','OK','OR','PA','RI','SC',
                 'SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')

predict_period <- function(model, state, group_label, years_vec, product_ref, adi_z = 0) {
  # set group flags
  older <- as.integer(group_label == "Older adult")
  sm    <- as.integer(group_label == "Service-member")
  # build grid across chosen years
  nd <- expand.grid(
    State = state,
    Year = factor(years_vec, levels = levels(df_clean$Year)),
    Product = product_ref,
    ADI_STATERNK_std = adi_z
  )
  nd$Older_American <- older
  nd$Servicemember  <- sm
  mean(predict(model, newdata = nd, type = "response"))
}

ref_product <- levels(df_clean$Product)[1]

state_changes <- purrr::map_dfr(main_states, function(st) {
  purrr::map_dfr(c("General population","Service-member","Older adult"), function(grp) {
    early  <- predict_period(model4_time_interactions, st, grp, early_years,  ref_product, adi_z = 0)
    modern <- predict_period(model4_time_interactions, st, grp, modern_years, ref_product, adi_z = 0)
    tibble(
      state = st, group = grp,
      early_period = early,
      modern_period = modern,
      change = modern - early,
      pct_change = (modern - early) / early * 100
    )
  })
})

state_changes_path <- file.path(output_dir, "state_period_changes_by_group.csv")
write.csv(state_changes, state_changes_path, row.names = FALSE)
message("Wrote: ", state_changes_path)

# ==== Output C: Within-state ADI disparities (low vs high ADI quintiles) ----
calculate_state_adi_disparities <- function(model, data) {
  states <- intersect(main_states, unique(as.character(data$State)))
  results <- list()

  for (st in states) {
    sd <- data %>% filter(State == st)
    if (nrow(sd) < 100) next

    # quintiles on raw state rank; then use the z-scored means in prediction
    sd <- sd %>% mutate(adi_quintile = ntile(ADI_STATERNK, 5))
    hi  <- sd %>% filter(adi_quintile == 5)
    lo  <- sd %>% filter(adi_quintile == 1)
    if (nrow(hi) < 20 || nrow(lo) < 20) next

    hi_z <- mean(hi$ADI_STATERNK_std, na.rm = TRUE)
    lo_z <- mean(lo$ADI_STATERNK_std, na.rm = TRUE)

    for (grp in c("General population","Service-member","Older adult")) {
      older <- as.integer(grp == "Older adult")
      sm    <- as.integer(grp == "Service-member")

      # Fix Year and Product for comparability (use a mid-period year)
      nd_hi <- nd_lo <- data.frame(
        State = st,
        Year  = factor("2020", levels = levels(df_clean$Year)),
        Product = ref_product,
        Older_American = older,
        Servicemember  = sm,
        ADI_STATERNK_std = NA_real_
      )
      nd_hi$ADI_STATERNK_std <- hi_z
      nd_lo$ADI_STATERNK_std <- lo_z

      p_hi <- predict(model, newdata = nd_hi, type = "response")
      p_lo <- predict(model, newdata = nd_lo, type = "response")

      # observed rates (for reference)
      if (older == 1) {
        obs_hi <- mean(hi$resolved_timely[hi$Older_American == 1], na.rm = TRUE)
        obs_lo <- mean(lo$resolved_timely[lo$Older_American == 1], na.rm = TRUE)
      } else if (sm == 1) {
        obs_hi <- mean(hi$resolved_timely[hi$Servicemember == 1],  na.rm = TRUE)
        obs_lo <- mean(lo$resolved_timely[lo$Servicemember == 1],   na.rm = TRUE)
      } else {
        obs_hi <- mean(hi$resolved_timely[hi$Older_American == 0 & hi$Servicemember == 0], na.rm = TRUE)
        obs_lo <- mean(lo$resolved_timely[lo$Older_American == 0 & lo$Servicemember == 0], na.rm = TRUE)
      }

      results[[length(results)+1]] <- tibble(
        state = st,
        group = grp,
        high_adi_predicted = p_hi,
        low_adi_predicted  = p_lo,
        absolute_disparity_predicted = p_lo - p_hi,    # low - high (positive: advantaged faster)
        relative_disparity_predicted = (p_lo / p_hi) - 1,
        high_adi_observed = obs_hi,
        low_adi_observed  = obs_lo,
        absolute_disparity_observed = obs_lo - obs_hi
      )
    }
  }
  bind_rows(results)
}

state_adi_disparities <- calculate_state_adi_disparities(model4_time_interactions, df_clean)
adi_path <- file.path(output_dir, "state_adi_disparities.csv")
write.csv(state_adi_disparities, adi_path, row.names = FALSE)
message("Wrote: ", adi_path)

# ==== Quick summaries (optional) ----
message("\nSummary by group (early→modern change):")
print(state_changes %>% group_by(group) %>%
        summarise(mean_change = mean(change, na.rm = TRUE),
                  min_change  = min(change, na.rm = TRUE),
                  max_change  = max(change, na.rm = TRUE)))

message("\nADI disparities (predicted) by group:")
print(state_adi_disparities %>% group_by(group) %>%
        summarise(mean_abs_disp = mean(absolute_disparity_predicted, na.rm = TRUE),
                  min_abs_disp  = min(absolute_disparity_predicted, na.rm = TRUE),
                  max_abs_disp  = max(absolute_disparity_predicted, na.rm = TRUE)))
