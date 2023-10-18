## ----setup, include = FALSE---------------------------------------------------
library("knitr")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
data.table::setDTthreads(1)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("socialmixr")

## ----eval=FALSE---------------------------------------------------------------
#  library("socialmixr")

## ----echo=FALSE---------------------------------------------------------------
suppressWarnings(library("socialmixr"))

## ----eval=FALSE---------------------------------------------------------------
#  ?contact_matrix

## -----------------------------------------------------------------------------
data(polymod)

## -----------------------------------------------------------------------------
contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))

## ----eval=FALSE---------------------------------------------------------------
#  list_surveys()

## ----eval=FALSE---------------------------------------------------------------
#  peru_survey <- get_survey("https://doi.org/10.5281/zenodo.1095664")
#  saveRDS(peru_survey, "peru.rds")

## ----eval=FALSE---------------------------------------------------------------
#  peru_survey <- readRDS("peru.rds")

## -----------------------------------------------------------------------------
survey_countries(polymod)

## -----------------------------------------------------------------------------
cite(polymod)

## -----------------------------------------------------------------------------
m <- replicate(
  n = 5,
  contact_matrix(
    polymod,
    countries = "United Kingdom", age.limits = c(0, 1, 5, 15),
    sample.participants = TRUE
  )
)
mr <- Reduce("+", lapply(m["matrix", ], function(x) x / ncol(m)))
mr

## ----warning=FALSE, message=FALSE---------------------------------------------
contact_matrix(polymod,
  countries = "United Kingdom", age.limits = c(0, 20),
  return.demography = TRUE
)$demography

## -----------------------------------------------------------------------------
contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15), symmetric = TRUE)

## ----message=FALSE, warning=FALSE---------------------------------------------
contact_matrix(survey = polymod, countries = "Germany", age.limits = c(0, 60), symmetric = TRUE, per.capita = TRUE)

## -----------------------------------------------------------------------------
contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15), split = TRUE)

## ----warning=FALSE, message=FALSE---------------------------------------------
# contact matrix for school-related contacts
contact_matrix(polymod, age.limits = c(0, 20, 60), filter = list(cnt_school = 1))$matrix

# contact matrix for work-related contacts involving physical contact
contact_matrix(polymod, age.limits = c(0, 20, 60), filter = list(cnt_work = 1, phys_contact = 1))$matrix

# contact matrix for daily contacts at home with males
contact_matrix(polymod, age.limits = c(0, 20, 60), filter = list(cnt_home = 1, cnt_gender = "M", duration_multi = 5))$matrix

## ----message=FALSE, warning=FALSE---------------------------------------------
contact_matrix(
  survey = polymod, age.limits = c(0, 18, 60), weigh.dayofweek = TRUE,
  weigh.age = TRUE, return.part.weights = TRUE
)

## ----message=FALSE, warning=FALSE---------------------------------------------
# e.g. use household size as (dummy) weight to provide more importance to participant data from large households
contact_matrix(survey = polymod, age.limits = c(0, 18, 60), weights = "hh_size")

## ----message=FALSE, warning=FALSE---------------------------------------------
contact_matrix(
  survey = polymod, age.limits = c(0, 18, 60), weigh.dayofweek = TRUE,
  weigh.age = TRUE, return.part.weights = TRUE, weight.threshold = 3
)

## ----echo=FALSE---------------------------------------------------------------
survey_data <- data.frame(
  age = c(1, 1, 2, 2, 2, 3),
  day.of.week = as.factor(c("weekend", "weekend", "weekend", "week", "week", "week")),
  age.group = NA,
  m_i = c(3, 2, 9, 10, 8, 15)
)

# age groups 1-2 and 3
survey_data$age.group <- 1 - (survey_data$age < 3) + 1
survey_data$age.group <- as.factor(c("A", "B"))[survey_data$age.group]

kable(survey_data)

## -----------------------------------------------------------------------------
N <- 6
N_age <- c(2, 3, 1)
N_age.group <- c(5, 1)
N_day.of.week <- c(3, 3)

P <- 3000
P_age <- c(1000, 1000, 1000)
P_age.group <- c(2000, 1000)

P_day.of.week <- c(5 / 7, 2 / 7) * 3000

## ----echo=FALSE---------------------------------------------------------------
print(paste("unweighted average number of contacts:", round(mean(survey_data$m_i), digits = 2)))

## ----echo=FALSE---------------------------------------------------------------
kable(aggregate(m_i ~ age + age.group, data = survey_data, mean))

## ----echo=FALSE---------------------------------------------------------------
# including population constants
survey_data$w <- NA
for (i in seq_len(nrow(survey_data))) {
  day_i <- survey_data$day.of.week[i]
  survey_data$w[i] <- (P_day.of.week[day_i] / P) / (N_day.of.week[day_i] / N)
}
survey_data$w_tilde <- survey_data$w / sum(survey_data$w) * N

# without population constants
survey_data$w_dot <- NA
for (i in seq_len(nrow(survey_data))) {
  day_i <- survey_data$day.of.week[i]
  survey_data$w_dot[i] <- (P_day.of.week[day_i]) / (N_day.of.week[day_i])
}
survey_data$w_dot_tilde <- survey_data$w_dot / sum(survey_data$w_dot) * N

# round
survey_data[, -(1:4)] <- round(survey_data[, -(1:4)], digits = 2)

# print
kable(survey_data)

# remove the 'dot' weights
survey_data$w_dot <- NULL
survey_data$w_dot_tilde <- NULL

## ----echo=FALSE---------------------------------------------------------------
# add weighted number of contacts
survey_data["m_i * w_tilde"] <- survey_data$m_i * survey_data$w_tilde

# show table
kable(survey_data)

# remove the weighted number of contacts
survey_data$"m_i * w_tilde" <- NULL

print(paste("weighted average number of contacts:", round(mean(survey_data$m_i * survey_data$w_tilde), digits = 2)))

## ----echo=FALSE---------------------------------------------------------------
kable(list(
  aggregate(m_i * w_tilde ~ age, data = survey_data, mean),
  aggregate(m_i * w_tilde ~ age.group, data = survey_data, mean)
))

## ----echo=FALSE---------------------------------------------------------------
survey_data$w_PS <- NA
for (i in seq_len(nrow(survey_data))) {
  k_i <- survey_data$age.group[i]
  flag_k <- survey_data$age.group == k_i
  survey_data$w_PS[i] <- survey_data$w[i] / sum(survey_data$w[flag_k]) * N_age.group[k_i]
}

# round
survey_data[, -(1:4)] <- round(survey_data[, -(1:4)], digits = 2)

kable(survey_data)

## ----echo=FALSE---------------------------------------------------------------
kable(aggregate(m_i * w_PS ~ age.group, data = survey_data, mean))

## ----echo=FALSE---------------------------------------------------------------
survey_data$w <- NA
survey_data$w_tilde <- NA
survey_data$w_PS <- NULL
for (i in seq_len(nrow(survey_data))) {
  age_i <- survey_data$age[i]
  survey_data$w[i] <- (P_age[age_i] / P) / (N_age[age_i] / N)
}
survey_data$w_tilde <- survey_data$w / sum(survey_data$w) * N

survey_data$w_PS <- NA
for (i in seq_len(nrow(survey_data))) {
  k_i <- survey_data$age.group[i]
  flag_k <- survey_data$age.group == k_i
  survey_data$w_PS[i] <- survey_data$w[i] / sum(survey_data$w[flag_k]) * N_age.group[k_i]
}

# round
survey_data[, -(1:4)] <- round(survey_data[, -(1:4)], digits = 2)

# print
kable(survey_data)

print(paste("weighted average number of contacts:", round(mean(survey_data$m_i * survey_data$w_tilde), digits = 2)))

## ----echo=FALSE---------------------------------------------------------------
kable(list(
  aggregate(m_i * w_tilde ~ age, data = survey_data, mean),
  aggregate(m_i * w_tilde ~ age.group, data = survey_data, mean)
))

## ----echo=FALSE---------------------------------------------------------------
kable(aggregate(m_i * w_PS ~ age.group, data = survey_data, mean))

## ----echo=FALSE---------------------------------------------------------------
survey_data <- survey_data[c(rep(1:2, 5), 3:6), ]
survey_data$m_i[survey_data$age == 3] <- 30
rownames(survey_data) <- NULL

survey_data <- survey_data[order(survey_data$age), ]
survey_data$w <- NULL
survey_data$w_tilde <- NULL
survey_data$w_PS <- NULL

## ----echo=FALSE---------------------------------------------------------------
N <- nrow(survey_data)
N_age <- table(survey_data$age)
N_age.group <- table(survey_data$age.group)
N_day.of.week <- table(survey_data$day.of.week)

## ----echo=FALSE---------------------------------------------------------------
survey_data$w <- NA
survey_data$w_tilde <- NA
survey_data$w_PS <- NULL
for (i in seq_len(nrow(survey_data))) {
  age_i <- survey_data$age[i]
  survey_data$w[i] <- (P_age[age_i] / P) / (N_age[age_i] / N)
}
survey_data$w_tilde <- survey_data$w / sum(survey_data$w) * N

# round
survey_data[, -(1:4)] <- round(survey_data[, -(1:4)], digits = 2)
kable(survey_data)

print(paste("unweighted average number of contacts:", round(mean(survey_data$m_i), digits = 2)))
print(paste("weighted average number of contacts:", round(mean(survey_data$m_i * survey_data$w_tilde), digits = 2)))

## ----echo=FALSE---------------------------------------------------------------
survey_data$w_tilde[survey_data$w_tilde > 3] <- 3

## ----echo=FALSE---------------------------------------------------------------
survey_data$w_tilde[survey_data$w_tilde > 3] <- 3

print(paste("weighted average number of contacts after truncation:", round(mean(survey_data$m_i * survey_data$w_tilde), digits = 2)))

## ----fig.width=4, fig.height=4------------------------------------------------
library("reshape2")
library("ggplot2")
df <- melt(mr, varnames = c("age.group", "age.group.contact"), value.name = "contacts")
ggplot(df, aes(x = age.group, y = age.group.contact, fill = contacts)) +
  theme(legend.position = "bottom") +
  geom_tile()

## ----fig.width=4, fig.height=4------------------------------------------------
matrix_plot(mr)
matrix_plot(mr, color.palette = gray.colors)

