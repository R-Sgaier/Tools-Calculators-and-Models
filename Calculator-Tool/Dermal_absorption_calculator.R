

# Function to calculate direct variables

#' Title var_calculator
#'
#' @param data
#'
#' @return data frame with 4 additional variables calculated: T 0.5 absorption, Absorbed dose,
#' Non absorbed dose, and Total recovery
#' @export
#'
#' @examples
var_calculator <- function(data) {
  data$T05_absorption <- (data$Receptor_fluid_T05 / data$Receptor_fluid_T1) * 100
  data$Absorbed_dose <- data$Receptor_fluid + data$Receptor_chamber_wash
  data$Non_absorbed_dose <- data$Skin_wash + data$Tape_strips_1_2 + data$Donor_compartment_wash
  data$Total_recovery <- data$Absorbed_dose + data$Non_absorbed_dose + data$Tape_strips_3_plus +
    data$Stripped_skin
  return(data)
}


# Function to calculate absorption Sum
# If > 75% of total absorption occurs within half of the study duration, tape-stripped material can be excluded
#' Title calculate_sum_Abs
#'
#' @param data
#'
#' @return data frame with the variable "absorption sum" added
#' @export
#'
#' @examples
calculate_sum_Abs <- function(data) {
  for (i in 1:nrow(data)) {
    if (data$T05_absorption[i] <= 75) {
      data$D_Abs[i] <- (data$Absorbed_dose[i] + data$Tape_strips_3_plus[i] + data$Stripped_skin[i])
    } else {
      data$D_Abs[i] <- (data$Absorbed_dose[i] + data$Stripped_skin[i])
    }
  }
  return(data)
}


# Function to calculate the dermal absorption value
# If mean total recovery is < 95% and replicate total recovery is < 95%: the dermal absorption value is normalized
# If the absorption sum is < 5% and total recovery is < 95%, the missing recovery percentage is added to the dermal absorption value
#' Title calculate_derm_Abs
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
calculate_derm_Abs <- function(data) {
  Total_recovery_mean <- mean(data$Total_recovery)
  for (i in 1:nrow(data)) {
    if (Total_recovery_mean < 95 && (data$Total_recovery[i]) < 95) {
      if (data$D_Abs < 5) {
        data$derm_Abs[i] <- data$D_Abs[i] + (100 - data$Total_recovery[i])
      } else {
        data$derm_Abs[i] <- (data$D_Abs[i] / data$Total_recovery[i] ) * 100
      }
    } else {
      data$derm_Abs[i] <- data$D_Abs[i]
    }
  }
  return(data)
}


