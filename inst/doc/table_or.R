## -----------------------------------------------------------------------------
#| label: setup
library(plotor)
set.seed(123) # reproducibility


## -----------------------------------------------------------------------------
#| label: model
# create a small example dataset
rows <- 400
df <- data.frame(
  outcome = rbinom(n = rows, size = 1, prob = 0.25) |> 
    factor(labels = c("Healthy", "Disease")),
  age = rnorm(n = rows, mean = 50, sd = 12),
  sex = sample(x = 0:1, size = rows, replace = TRUE) |> 
    factor(labels = c("Female", "Male")),
  smoke = sample(x = 0:2, size = rows, replace = TRUE) |> 
    factor(labels = c("Never", "Former", "Current"))
)

# fit a logistic regression model
m <- glm(
  formula = outcome ~ age + sex + smoke,
  family = "binomial",
  data = df
)


## -----------------------------------------------------------------------------
#| label: tibble

table_or(m, output = "tibble")


## -----------------------------------------------------------------------------
#| label: gt

table_or(m, output = "gt")


## -----------------------------------------------------------------------------
#| label: customise - confidence level

table_or(m, output = "tibble", conf_level = 0.90)


## -----------------------------------------------------------------------------
#| label: customise - anonymise counts

table_or(m, output = "tibble", anonymise_counts = TRUE)


## -----------------------------------------------------------------------------
#| label: customise - combine

# fit a second model
m2 <- glm(
  formula = outcome ~ age + sex,
  family = "binomial",
  data = df
)

# combine results
combined_results <-
  dplyr::bind_rows(
    dplyr::bind_cols(model = "Model 1", table_or(m)),
    dplyr::bind_cols(model = "Model 2", table_or(m2))
  )

head(combined_results)


## -----------------------------------------------------------------------------
#| label: export - html
#| eval: false

# gt_table <- table_or(m, output = "gt")
# gt::gtsave(data = gt_table, filename = "odds_ratios.html")


## -----------------------------------------------------------------------------
#| label: export - word
#| eval: false

# gt_table <- table_or(m, output = "gt")
# gt::gtsave(data = gt_table, filename = "odds_ratios.docx")


## -----------------------------------------------------------------------------
#| label: export - csv
#| eval: false

# readr::write_csv(x = table_or(m), file = "odds_ratios.csv")

