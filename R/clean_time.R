
#' Transform raw time measurements into a tidy tibble for further analysis
#'
#' @param df Data frame containing raw time data from measurement script
#'
#' @export
clean_time_measurement <- function(df) {
  df %>%
    dplyr::group_by(problem_size) %>%
    dplyr::summarise(dplyr::across(
      dplyr::contains('time') |
        dplyr::contains("gpu_") |
        dplyr::contains("cpu"), list(avg = mean))) %>%
    dplyr::rename(exec_time_avg = time_avg, time_avg = time_cuda_avg) %>%
    dplyr::relocate(time_avg, .after = problem_size) %>%
    dplyr::mutate(time_avg = time_avg / 1e3) %>%
    dplyr::ungroup()
}


#' Transform raw time measurements into a tidy tibble for further analysis
#'
#' Depreciated function that is only used to transform older raw data. The
#' measurement script was changed relatively early. However, some measurements
#' may still depend on this function.
#'
#' @param df Data frame containing raw time data from measurement script
#'
#' @export
clean_time_m_old <- function(df) {
  df %>%
    dplyr::group_by(problem_size) %>%
    dplyr::summarise(time_avg = mean(time),
              time_median = median(time)) %>%
    dplyr::ungroup()
}
