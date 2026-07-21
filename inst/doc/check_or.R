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

# prints messages to console
check_or(m)


## -----------------------------------------------------------------------------
#| label: example - separation

# create data with separation
rows <- 100
df_sep <- data.frame(
  outcome = c(rep(0, 50), rep(1, 50)) |> factor(labels = c("No", "Yes")),
  predictor1 = c(rep(0, 50), rep(1, 50)), # perfect separator
  predictor2 = rpois(n = rows, lambda = 5)
)

# fit model
m_sep <- glm(
  formula = outcome ~ predictor1 + predictor2,
  family = "binomial",
  data = df_sep
)

# run diagnostics
check_or(m_sep)

