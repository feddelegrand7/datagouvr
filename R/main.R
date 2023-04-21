
`%>%` <- magrittr::`%>%`

datagouvr_search <- function(search_query, page = 1, page_size = 20, verbose = FALSE, ...) {

  base_url <- .get_base_url()

  response <- httr2::request(base_url) %>%
    httr2::req_url_path_append("datasets") %>%
    httr2::req_url_query(
      `q` = search_query,
      `page` = page,
      `page_size` = page_size,
      ...
    ) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  if (is.null(response$data)) {
    .get_empty_data_frame()
  }

  DT <- .convert_list_to_dataframe(response$data)

  cols_to_unchop <- .get_cols_to_unchop()
  cols_to_unnest <- .get_cols_to_unnest()

  final_dt <- DT %>%
    tidyr::unchop(cols = base::eval(cols_to_unchop)) %>%
    tidyr::unnest_wider(
      col =  base::eval(cols_to_unnest),
      names_sep = "_"
    )

  final_dt <- final_dt[, c(
    cols_to_unchop,
    grep("^metric", names(final_dt), value = TRUE),
    grep("^owner", names(final_dt), value = TRUE),
    grep("^quality", names(final_dt), value = TRUE),
    grep("^spatial", names(final_dt), value = TRUE),
    grep("^organization", names(final_dt), value = TRUE)
  )]

  if (!verbose) {
    final_dt <- final_dt[, c("id", "title", "last_update")]
  }

  tibble::as_tibble(final_dt)
}

datagouvr_explain <- function(id) {

  response <- .get_response_per_id(trimws(id))

  response$description

}

datagouvr_list_resources <- function(id) {

  response <- .get_response_per_id(trimws(id))

  DT <- .convert_list_to_dataframe(response$resources)

  cols_to_unchop <- .get_cols_to_unchop_for_list_resources()

  final_data <- DT %>%
    tidyr::unchop(
      cols = base::eval(cols_to_unchop)
    )

  tibble::as_tibble(final_data[, cols_to_unchop])
}
