#' Goes through the whole cleaning process for time as well as energy
#'
#' @param time_path Path of raw time measurements
#' @param energy_path Path of raw energy measurements
#'
#' @export
clean_measurement <- function(time_path, energy_path) {
  time_raw <- readr::read_csv(time_path,
                              show_col_types = FALSE)
  energy_raw <- readr::read_csv(energy_path,
                                show_col_types = FALSE)

  time_df <- clean_time_measurement(time_raw)

  if ("graphic" %in% colnames(energy_raw)) {
    energy_df <- clean_energy_measurement_v2(energy_raw, time_df)
  } else {
    energy_df <- clean_energy_measurement(energy_raw, time_df)
  }

  list("time" = time_df,
       "energy" = energy_df,
       "raw_time" = time_raw,
       "raw_energy" = energy_raw)
}
