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

# create the forest plot
plot_or(m)


## -----------------------------------------------------------------------------
#| label: interpreting confidence intervals

# ensure reproducibility
set.seed(123)

# create a small example dataset
rows <- 400
df <- data.frame(
  narrow_interval = rnorm(n = rows, sd = 10),
  wide_interval = rnorm(n = rows, sd = 1),
  interval_crosses_1 = rnorm(n = rows, sd = 3),
  interval_doesnt_cross_1 = rnorm(n = rows, sd = 3)
)

# create outcome based on predictors to achieve desired associations:
# narrow_interval: strong association (narrow CI)
# wide_interval: weak association (wide CI)
# interval_crosses_1: very weak association (CI crosses 1)
# interval_doesnt_cross_1: moderate association (CI doesn't cross 1)
df$outcome <- 
  rbinom(
    n = rows,
    size = 1,
    prob = plogis(
      0 + # intercept
      2 * scale(df$narrow_interval)[, 1] + 
      0.1 * scale(df$wide_interval)[, 1] + 
      0.01 * scale(df$interval_crosses_1)[, 1] + 
      -0.3 * scale(df$interval_doesnt_cross_1)[, 1]
    )
  ) |> factor(labels = c("Healthy", "Disease"))

# fit a logistic regression model
m <- glm(
  formula = outcome ~ narrow_interval + wide_interval + 
    interval_crosses_1 + interval_doesnt_cross_1,
  family = "binomial",
  data = df
)

# create the forest plot (skip checks on logistic regression assumptions)
plotor::plot_or(m, assumption_checks = FALSE)


## -----------------------------------------------------------------------------
#| label: export - png
#| eval: false

# p <- plot_or(m)
# ggplot2::ggsave(
#   filename = "forest_plot.png",
#   plot = p,
#   width = 10,
#   height = 6,
#   dpi = 300
# )


## -----------------------------------------------------------------------------
#| label: export - pdf
#| eval: false

# p <- plot_or(m)
# ggplot2::ggsave(
#   filename = "forest_plot.pdf",
#   plot = p,
#   width = 10,
#   height = 6
# )

