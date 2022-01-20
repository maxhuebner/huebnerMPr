
#' Transform raw energy measurements into a tidy tibble for further analysis
#'
#' @param df Data frame containing raw energy data from measurement script
#' @param df_time Data frame with processed time data for energy calculation
#'
#' @export
clean_energy_measurement <- function(df, df_time) {
  df %>%
    dplyr::group_by(problem_size, rep_id) %>%
    dplyr::summarise(power_avg = mean(power),
              util_min = min(utilization),
              util_avg = mean(utilization),
              time = max(time)) %>%
    dplyr::summarise(dplyr::across(power_avg:util_avg, mean)) %>%
    dplyr::left_join(df_time, by = "problem_size") %>%
    dplyr::select(-(exec_time_avg:cpu_init_avg)) %>%
    dplyr::mutate(energy_avg = power_avg * time_avg / 1e3)
}

#' Updated Version of energy measurement cleaning with more captured variables
#' Accounts for measurements on different clock speeds.
#'
#' @param df Data frame containing raw energy data from measurement script
#' @param df_time Data frame with processed time data for energy calculation
#'
#' @export
clean_energy_measurement_v2 <- function(df, df_time) {
  df %>%
    dplyr::group_by(problem_size, memory, graphic, rep_id) %>%
    dplyr::summarise(power_avg = mean(power),
                     temp_max = max(temp),
                     temp_avg = mean(temp),
                     g_clock_avg = mean(graphic_clock),
                     m_clock_avg = mean(memory_clock),
                     util_min = min(utilization),
                     util_avg = mean(utilization)
                     ) %>%
    dplyr::group_by(problem_size, memory, graphic) %>%
    dplyr::summarise(dplyr::across(power_avg:util_avg, mean)) %>%
    dplyr::left_join(df_time, by = "problem_size") %>%
    dplyr::select(-(exec_time_avg:cpu_init_avg)) %>%
    dplyr::mutate(energy_avg = power_avg * time_avg / 1e3)
}


#' Transform raw energy measurements into a tidy tibble for further analysis
#'
#' Depreciated function that is only used to transform older raw data. The
#' measurement script was changed relatively early. However, some measurements
#' may still depend on this function.
#'
#' @param df Data frame containing raw energy data from measurement script
#' @param df_time Data frame with processed time data for energy calculation
#' @param idle_draw Idle power draw of the gpu
#'
#' @export
clean_energy_mm_old <- function(df, time_df, idle_draw) {
  df %>%
    dplyr::mutate(power_without_idle = power - idle_draw * 1e3) %>%
    dplyr::group_by(problem_size, rep_id) %>%
    dplyr::summarize(power = mean(power),
                     power_without_idle = mean(power_without_idle)) %>%
    dplyr::summarise(power_avg = mean(power),
                     power_avg_idle = mean(power_without_idle)) %>%
    dplyr::left_join(time_df, by = "problem_size") %>%
    dplyr::mutate(
      energy_avg = power_avg * time_avg / 1e3,
      energy_mean_idle = power_avg_idle * time_avg / 1e3)
}
