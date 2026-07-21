## -----------------------------------------------------------------------------
#| label: setup
library(plotor)
set.seed(123) # reproducibility


## -----------------------------------------------------------------------------
#| label: data
rows <- 400
df <- data.frame(
  # the first factor level is the reference, 
  # results in odds of 'Disease' vs 'Healthy'
  outcome = rbinom(n = rows, size = 1, prob = 0.25) |> 
    factor(labels = c("Healthy", "Disease")),
  age = rnorm(n = rows, mean = 50, sd = 12),
  sex = sample(x = 0:1, size = rows, replace = TRUE) |> 
    factor(labels = c("Female", "Male")),
  smoke = sample(x = 0:2, size = rows, replace = TRUE) |> 
    factor(labels = c("Never", "Former", "Current"))
)


## -----------------------------------------------------------------------------
#| label: model
m <- glm(
  formula = outcome ~ age + sex + smoke,
  data = df,
  family = "binomial"
)


## -----------------------------------------------------------------------------
#| label: diagnostics
check_or(m)


## -----------------------------------------------------------------------------
#| label: table

# two output formats shown: gt (rendered) and tibble (for programmatic use)
table_or(m, output = "gt") # formatted HTML table
table_or(m, output = "tibble") # programmatic output


## -----------------------------------------------------------------------------
#| label: plot
#| fig-cap: Forest plot shows point estimates and 95% confidence intervals; the vertical reference line at OR = 1 indicates no association
#| fig-alt: Forest plot of adjusted odds ratios with horizontal error bars showing 95% confidence intervals for predictors (age, sex and smoke). A vertical reference line at OR = 1 marks no association; points to the right.
#| fig-dpi: 300
#| fig-width: 6
#| fig-height: 3.5
#| fig-unit: cm
plot_or(m)

