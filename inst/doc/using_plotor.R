## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(plotor)
library(dplyr)
library(datasets)
library(tidyr)
library(stats)
library(broom)
library(forcats)
library(ggplot2)

## -----------------------------------------------------------------------------
df <- datasets::Titanic |> 
  as_tibble() |> 
  # convert counts to observations
  filter(n > 0) |>
  uncount(weights = n) |>
  # convert categorical variables to factors.
  # we specify an order for levels in Class and Survival, otherwise ordering
  # in descending order of frequency
  mutate(
    Class = Class |>
      fct(levels = c('1st', '2nd', '3rd', 'Crew')),
    Sex = Sex |>
      fct_infreq(),
    Age = Age |>
      fct_infreq(),
    Survived = Survived |>
      fct(levels = c('No', 'Yes'))
  )

# preview the data
df |> glimpse()

## -----------------------------------------------------------------------------
# conduct a logistic regression of survival against the other variables
lr <- glm(
  data = df,
  family = 'binomial',
  formula = Survived ~ Class + Sex + Age
)

# preview the model as a tidy table
lr |> 
  tidy() |> 
  glimpse()

## ----warning=FALSE, fig.width=9, fig.height=4, fig.format='png', fig.retina=TRUE----
# using plot_or
plot_or(glm_model_results = lr)

## ----warning=FALSE, fig.width=9, fig.height=6, fig.format='png', fig.retina=TRUE----
plot_or(glm_model_results = lr) + 
  theme(text = element_text(size = 16))

## ----warning=FALSE, message=FALSE, fig.width=9, fig.height=4, fig.format='png', fig.retina=TRUE----
plot_or(glm_model_results = lr) + 
  scale_x_log10(breaks = c(0.1, 0.5, 1, 5, 10))

## ----warning=FALSE, message=FALSE, fig.width=9, fig.height=4, fig.format='png', fig.retina=TRUE----
plot_or(glm_model_results = lr) +
  scale_colour_manual(values = c(
    'Significant' = '#44bd32',
    'Comparator' = '#8c7ae6',
    'Not significant' = '#e84118')
  )

## ----warning=FALSE, message=FALSE, fig.width=9, fig.height=4, fig.format='png', fig.retina=TRUE----
plot_or(glm_model_results = lr) +
  labs(
    title = 'Passenger survival from the Titanic disaster',
    subtitle = 'Odds Ratio of survival by Class, Age and Gender',
    caption = 'Data source: Dawson, Robert J. MacG. (1995), The ‘Unusual Episode’ Data Revisited. Journal of Statistics Education, 3. doi:10.1080/10691898.1995.11910499'
  )

## -----------------------------------------------------------------------------
df <- datasets::esoph |> 
  # convert aggregated data to tidy observational data
  tidyr::pivot_longer(
    cols = c(ncases, ncontrols),
    names_to = 'Group',
    values_to = 'people'
  ) |> 
  uncount(weights = people) |> 
  # prepare the variables
  mutate(
    # convert the intervention group to a factor
    Group = Group |> 
      case_match('ncases' ~ 'Case', 'ncontrols' ~ 'Control') |> 
      fct(levels = c('Control', 'Case')),
    # remove the ordering from these factors so the glm model doesn't treat
    # them as numeric
    agegp = agegp |> factor(ordered = F),
    alcgp = alcgp |> factor(ordered = F),
    tobgp = tobgp |> factor(ordered = F)
  )

# preview the data
df |> glimpse()

## ----warning=FALSE, message=FALSE, fig.width=9, fig.height=5, fig.format='png', fig.retina=TRUE----
# conduct the logistic regression
lr <- glm(
  data = df,
  family = 'binomial',
  formula = Group ~ agegp + alcgp + tobgp
)

# plot the odds ratio plot with customised title
plot_or(lr) +
  labs(title = 'Likelihood of developing oesophageal cancer')

## -----------------------------------------------------------------------------
# library to apply labels to data
library(labelled)

# create a list of variable = labels
var_labels <- list(
  agegp = 'Age group',
  alcgp = 'Alcohol consumption',
  tobgp = 'Tobacco consumption',
  Group = 'Developing oesophageal cancer'
)

# label the variables in our data
labelled::var_label(df) <- var_labels

# preview the data with labels appplied 
labelled::look_for(df)

## ----warning=FALSE, message=FALSE, fig.width=9, fig.height=5, fig.format='png', fig.retina=TRUE----
# conduct the logistic regression
lr <- glm(
  data = df,
  family = 'binomial',
  formula = Group ~ agegp + alcgp + tobgp
)

# plot the odds ratio plot using variable labels
plot_or(lr)

