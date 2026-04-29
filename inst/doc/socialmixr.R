## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
data.table::setDTthreads(1)

## -----------------------------------------------------------------------------
library(socialmixr)
library(ggplot2)
data(polymod)

## -----------------------------------------------------------------------------
polymod[country == "United Kingdom"] |>
  assign_age_groups(age_limits = c(0, 1, 5, 15)) |>
  compute_matrix()

## -----------------------------------------------------------------------------
uk_grouped <- polymod[country == "United Kingdom"] |>
  assign_age_groups(age_limits = c(0, 1, 5, 15))

head(uk_grouped$participants[, c("part_id", "part_age", "age.group")])
head(uk_grouped$contacts[, c("part_id", "cnt_age", "contact.age.group")])

## -----------------------------------------------------------------------------
unique(polymod$participants$country)

## -----------------------------------------------------------------------------
polymod[country %in% c("United Kingdom", "Germany")]

## -----------------------------------------------------------------------------
bootstrap <- function(survey) {
  sampled_ids <- sample(
    unique(survey$participants$part_id),
    replace = TRUE
  )
  survey$participants <- survey$participants[
    list(sampled_ids), on = "part_id"
  ]
  survey$contacts <- survey$contacts[
    list(sampled_ids),
    on = "part_id",
    nomatch = NULL,
    allow.cartesian = TRUE
  ]
  survey
}

uk <- polymod[country == "United Kingdom"] |>
  assign_age_groups(age_limits = c(0, 1, 5, 15))

m <- suppressWarnings(
  replicate(n = 5, uk |> bootstrap() |> compute_matrix())
)
mr <- Reduce("+", lapply(m["matrix", ], function(x) x / ncol(m)))
mr

## ----eval=requireNamespace("wpp2024", quietly = TRUE), message=FALSE, warning=FALSE----
# data("popAge1dt", package = "wpp2024")
# uk_pop <- popAge1dt[name == "United Kingdom" & year == 2020,
#   .(lower.age.limit = age, population = pop * 1000)
# ]
# head(uk_pop)

## -----------------------------------------------------------------------------
custom_pop <- data.frame(
  lower.age.limit = c(0, 18, 60),
  population = c(12000000, 35000000, 20000000)
)

## -----------------------------------------------------------------------------
survey_country_population(polymod, countries = "United Kingdom")

## ----message=FALSE, warning=FALSE---------------------------------------------
uk_pop <- survey_country_population(polymod, countries = "United Kingdom")

polymod[country == "United Kingdom"] |>
  assign_age_groups(age_limits = c(0, 1, 5, 15)) |>
  compute_matrix() |>
  symmetrise(survey_pop = uk_pop)

## ----message=FALSE, warning=FALSE---------------------------------------------
de_pop <- survey_country_population(polymod, countries = "Germany")

polymod[country == "Germany"] |>
  assign_age_groups(age_limits = c(0, 60)) |>
  compute_matrix() |>
  symmetrise(survey_pop = de_pop) |>
  per_capita(survey_pop = de_pop)

## -----------------------------------------------------------------------------
polymod[country == "United Kingdom"] |>
  assign_age_groups(age_limits = c(0, 1, 5, 15)) |>
  compute_matrix() |>
  split_matrix(survey_pop = uk_pop)

## ----warning=FALSE, message=FALSE---------------------------------------------
# contact matrix for school-related contacts
polymod[cnt_school == 1] |>
  assign_age_groups(age_limits = c(0, 20, 60)) |>
  compute_matrix()

# contact matrix for work-related contacts involving physical contact
polymod[cnt_work == 1][phys_contact == 1] |>
  assign_age_groups(age_limits = c(0, 20, 60)) |>
  compute_matrix()

# contact matrix for daily contacts at home with males
polymod[cnt_home == 1][cnt_gender == "M"][duration_multi == 5] |>
  assign_age_groups(age_limits = c(0, 20, 60)) |>
  compute_matrix()

## ----message=FALSE, warning=FALSE---------------------------------------------
polymod[country == "United Kingdom"] |>
  assign_age_groups(age_limits = c(0, 18, 60)) |>
  weigh("dayofweek", target = c(5, 2), groups = list(1:5, c(0, 6))) |>
  weigh("part_age", target = uk_pop) |>
  compute_matrix()

## ----message=FALSE, warning=FALSE---------------------------------------------
polymod |>
  assign_age_groups(age_limits = c(0, 18, 60)) |>
  weigh("hh_size") |>
  compute_matrix()

## ----message=FALSE, warning=FALSE---------------------------------------------
polymod[country == "United Kingdom"] |>
  assign_age_groups(age_limits = c(0, 18, 60)) |>
  weigh("dayofweek", target = c(5, 2), groups = list(1:5, c(0, 6))) |>
  weigh("part_age", target = uk_pop) |>
  compute_matrix(weight_threshold = 3)

## ----echo=FALSE---------------------------------------------------------------
survey_data <- data.frame(
  age = c(1, 1, 2, 2, 2, 3),
  day.of.week = as.factor(c(
    "weekend", "weekend", "weekend",
    "week", "week", "week"
  )),
  age.group = NA,
  m_i = c(3, 2, 9, 10, 8, 15)
)

# age groups 1-2 and 3
survey_data$age.group <- 1 - (survey_data$age < 3) + 1
survey_data$age.group <- as.factor(c("A", "B"))[survey_data$age.group]

knitr::kable(survey_data)

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
cat(paste(
  "unweighted average number of contacts:",
  round(mean(survey_data$m_i), digits = 2)
))

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(aggregate(
  m_i ~ age + age.group, data = survey_data, mean
))

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
knitr::kable(survey_data)

# remove the 'dot' weights
survey_data$w_dot <- NULL
survey_data$w_dot_tilde <- NULL

## ----echo=FALSE---------------------------------------------------------------
# add weighted number of contacts
survey_data["m_i * w_tilde"] <- survey_data$m_i * survey_data$w_tilde

# show table
knitr::kable(survey_data)

# remove the weighted number of contacts
survey_data$`m_i * w_tilde` <- NULL

cat(paste(
  "weighted average number of contacts:",
  round(
    mean(survey_data$m_i * survey_data$w_tilde),
    digits = 2
  )
))

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(list(
  aggregate(m_i * w_tilde ~ age, data = survey_data, mean),
  aggregate(m_i * w_tilde ~ age.group, data = survey_data, mean)
))

## ----echo=FALSE---------------------------------------------------------------
survey_data$w_PS <- NA
for (i in seq_len(nrow(survey_data))) {
  k_i <- survey_data$age.group[i]
  flag_k <- survey_data$age.group == k_i
  survey_data$w_PS[i] <- survey_data$w[i] /
    sum(survey_data$w[flag_k]) * N_age.group[k_i]
}

# round
survey_data[, -(1:4)] <- round(
  survey_data[, -(1:4)], digits = 2
)

knitr::kable(survey_data)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(aggregate(
  m_i * w_PS ~ age.group,
  data = survey_data, mean
))

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
  survey_data$w_PS[i] <- survey_data$w[i] /
    sum(survey_data$w[flag_k]) * N_age.group[k_i]
}

# round
survey_data[, -(1:4)] <- round(
  survey_data[, -(1:4)], digits = 2
)

# print
knitr::kable(survey_data)

cat(paste(
  "weighted average number of contacts:",
  round(
    mean(survey_data$m_i * survey_data$w_tilde),
    digits = 2
  )
))

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(list(
  aggregate(m_i * w_tilde ~ age, data = survey_data, mean),
  aggregate(m_i * w_tilde ~ age.group, data = survey_data, mean)
))

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(aggregate(m_i * w_PS ~ age.group, data = survey_data, mean))

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
survey_data[, -(1:4)] <- round(
  survey_data[, -(1:4)], digits = 2
)
knitr::kable(survey_data)

cat(paste(
  "unweighted average number of contacts:",
  round(mean(survey_data$m_i), digits = 2)
))
cat(paste(
  "weighted average number of contacts:",
  round(
    mean(survey_data$m_i * survey_data$w_tilde),
    digits = 2
  )
))

## ----echo=FALSE---------------------------------------------------------------
survey_data$w_tilde[survey_data$w_tilde > 3] <- 3

## ----echo=FALSE---------------------------------------------------------------
survey_data$w_tilde[survey_data$w_tilde > 3] <- 3

cat(paste(
  "weighted average number of contacts after truncation:",
  round(
    mean(survey_data$m_i * survey_data$w_tilde),
    digits = 2
  )
))

## ----fig.width=4, fig.height=4------------------------------------------------
df <- reshape2::melt(
  mr,
  varnames = c("age.group", "age.group.contact"),
  value.name = "contacts"
)
ggplot(df, aes(x = age.group, y = age.group.contact, fill = contacts)) +
  theme(legend.position = "bottom") +
  geom_tile()

## ----fig.width=4, fig.height=4------------------------------------------------
matrix_plot(mr)
matrix_plot(mr, color.palette = gray.colors)

