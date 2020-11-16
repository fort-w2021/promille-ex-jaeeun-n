# function: calculate the blood alcohol concentration using
#   the formula of Widmark and Whatson
tell_me_how_drunk <- function(age,
                              sex = c("male", "female"),
                              height,
                              weight,
                              drinking_time,
                              drinks) {
  # input checking ############
  checkmate::assert_number(age, lower = 1, upper = 120)
  sex <- tolower(sex)
  sex <- match.arg(sex)
  checkmate::assert_number(height, lower = 1, upper = 300)
  checkmate::assert_number(weight, lower = 1, upper = 650)
  checkmate::assert_atomic_vector(drinking_time, any.missing = FALSE, len = 2)
  checkmate::assert_posixct(drinking_time)
  if (drinking_time[[1]] >= drinking_time[[2]]) {
    stop("start of drinking time must be before end of drinking time")
  }
  checkmate::assert(check_atomic_vector(drinks, all.missing = FALSE),
    check_list(drinks, all.missing = FALSE),
    combine = "or"
  )
  drinks <- unlist(drinks)
  if (is.null(names(drinks)) |
    !all(names(drinks) %in% c("massn", "hoibe", "wein", "schnaps"))) {
    stop("can't identify your drinks")
  }
  checkmate::assert_numeric(drinks,
                            lower = 0.0000001,
                            finite = TRUE,
                            any.missing = FALSE)
  if (age < 16) {
    warning("illegal")
  }
  if (age > 16 & age < 18 & any("schnaps" %in% names(drinks))) {
    warning("illegal")
  }
  ##############################

  total_body_water <- calculate_total_body_water(
    age = age,
    sex = sex,
    height = height,
    weight = weight
  )
  blood_alcohol <- calculate_blood_alcohol(
    drinks = drinks,
    total_body_water = total_body_water
  )
  blood_alcohol <- include_drinking_time(
    blood_alcohol = blood_alcohol,
    drinking_time = drinking_time
  )

  return(blood_alcohol)
}

# function: calculate the total body water for male using Whatson's formula
# and Eickner's modification for female
calculate_total_body_water <- function(age,
                                       sex = c("male", "female"),
                                       height,
                                       weight) {
  if (sex == "male") {
    total_body_water <- 2.447 -
      0.09516 * age +
      0.1074 * height +
      0.3362 * weight
    return(total_body_water)
  } else {
    total_body_water <- 0.203 -
      0.07 * age +
      0.1069 * height +
      0.2466 * weight
    return(total_body_water)
  }
}

# function: calculate blood alcohol
# based on amount&type of drinks and total body water
calculate_blood_alcohol <- function(drinks, total_body_water) {
  characteristics_drinks <- data.frame(
    drink = c("massn", "hoibe", "wein", "schnaps"),
    alcohol_percentage = c(0.06, 0.06, 0.11, 0.4),
    ml_per_portion = c(1, 0.5, 0.2, 0.04) * 1000
  )

  what_i_drank <- dplyr::tibble(
    drink = names(drinks),
    portions = drinks
  )
  what_i_drank <- dplyr::left_join(what_i_drank,
    characteristics_drinks,
    by = "drink"
  )
  what_i_drank <- dplyr::mutate(what_i_drank,
    portions_in_ml = portions * ml_per_portion,
    alcohol_volume_in_ml = alcohol_percentage * portions_in_ml
  )

  alcohol_density <- 0.8
  alcohol_mass <- sum(what_i_drank[, "alcohol_volume_in_ml"]) * alcohol_density

  blood_density <- 1.055
  blood_alcohol <- (0.8 * alcohol_mass) / (blood_density * total_body_water)
  blood_alcohol
}

# function: correct the blood alcohol for the reduction over time
include_drinking_time <- function(blood_alcohol, drinking_time) {
  drinking_duration <- as.numeric(
    difftime(drinking_time[[2]], drinking_time[[1]], units = "hours")
  )
  drinking_duration <- max(drinking_duration, 1)

  new_blood_alcohol <- max(blood_alcohol - ((drinking_duration - 1) * 0.15), 0)
  new_blood_alcohol
}
