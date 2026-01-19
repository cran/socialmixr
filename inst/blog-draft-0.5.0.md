---
title: "socialmixr 0.5.0: Modular Workflows for Contact Matrices"
author: "Sebastian Funk"
date: "2026-01-XX"
categories: [R, epidemiology, contact matrices]
---

We're excited to announce the release of socialmixr 0.5.0! This release focuses
on improved modularity and flexibility for contact matrix workflows, making it
easier to customise how you process survey data before generating contact
matrices.

You can install the latest version from CRAN:

```r
install.packages("socialmixr")
```

## New modular workflow

The headline feature of this release is the ability to pre-process survey data
before passing it to `contact_matrix()`. Two new exported functions make this
possible:

### `assign_age_groups()`

This function handles all age-related processing: imputing ages from ranges,
handling missing values, and assigning age groups. You can now inspect and
modify the processed data before generating a contact matrix:

```r
library(socialmixr)
data(polymod)

# Process ages separately
processed <- polymod |>
  assign_age_groups(age_limits = c(0, 5, 18, 65))

# Inspect the result
head(processed$participants[, .(part_id, part_age, age.group)])

# Then generate the matrix
contact_matrix(processed, age_limits = c(0, 5, 18, 65))
```

### `survey_country_population()`

This function retrieves population data for a survey, which you can inspect or
modify before use:

```r
# Get population data for UK
pop_data <- polymod |>
  survey_country_population(countries = "United Kingdom")

# Use custom population in contact_matrix
contact_matrix(
  polymod,
  countries = "United Kingdom",
  survey_pop = pop_data
)
```

## Transitioning to contactsurveys

We've begun transitioning survey download functionality to a new dedicated
package: [contactsurveys](https://github.com/epiforecasts/contactsurveys).

Functions like `get_survey()`, `download_survey()`, and `list_surveys()` now
show deprecation warnings pointing to contactsurveys. The new package offers
improved caching and will be the home for survey download features going
forward.

**For now**, these functions continue to work in socialmixr. We recommend
transitioning when convenient:

```r
# Old way (still works, but deprecated)
survey <- get_survey("10.5281/zenodo.1095664")

# New way
# install.packages("contactsurveys")
library(contactsurveys)
survey <- download_survey("10.5281/zenodo.1095664") |>
  socialmixr::load_survey()
```

## Breaking changes

This release includes some breaking changes that may affect existing code:

### Age limits now preserved

`contact_matrix()` now preserves all user-specified `age_limits`, even when no
participants exist in some age groups. Previously, empty age groups were
silently dropped. This may change matrix dimensions:

```r
# If no participants aged 80+, this now returns a 4x4 matrix (not 3x3)
contact_matrix(polymod, age_limits = c(0, 18, 65, 80))
```

### Age limits inferred from contacts too

When `age_limits` is not specified, it's now inferred from both participant
*and* contact ages. This may result in more age groups if contacts include ages
beyond the participant range.

## Acknowledgements

This release was made possible by contributions from Nicholas Tierney
(@njtierney), Lander Willem (@lwillem), Hugo Gruson (@Bisaloo), Lloyd Chapman
(@LloydChapman), James Azam (@jamesmbaazam), and Abdoelnaser Degoot
(@Degoot-AM). Thank you!

For a full list of changes, see the
[NEWS file](https://epiforecasts.io/socialmixr/news/index.html).
