
.get_base_url <- function() {
  "https://www.data.gouv.fr/api/1/"
}

.get_response_per_id <- function(id) {

  base_url <- .get_base_url()

  response <- httr2::request(base_url) %>%
    httr2::req_url_path_append("datasets") %>%
    httr2::req_url_path_append(id) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
}

.convert_list_to_dataframe <- function(list_input) {

  tmp <- list()

  for (i in seq_along(list_input)) {
    tmp[[i]] <- tibble::enframe(list_input[[i]])
  }

  tmp_pivoted <- lapply(tmp, tidyr::pivot_wider)

  DT <- do.call(rbind, tmp_pivoted)

  DT <- as.data.frame(apply(DT, 2, as.character))

  DT
}

.get_empty_data_frame <- function() {

  tibble::tibble(
    "id" = character(),
    "title" = character(),
    "created_at" = character(),
    "last_modified" = character(),
    "last_update" = character(),
    "frequency" = character(),
    "frequency_date" = character(),
    "license" = character(),
    "page" = character(),
    "private" = character(),
    "slug" = character(),
    "uri" = character(),
    "deleted" = character(),
    "archived" = character(),
    "metrics" = character(),
    "owner" = character(),
    "quality" = character(),
    "spatial" = character(),
    "organization" = character()
  )

}

.get_cols_to_unchop <- function() {

  c(
    "id",
    "title",
    "created_at",
    "last_modified",
    "last_update",
    "frequency",
    "frequency_date",
    "license",
    "page",
    "private",
    "slug",
    "uri",
    "deleted",
    "archived"
  )

}

.get_cols_to_unnest <- function() {
  c(
    "metrics",
    "owner",
    "quality",
    "spatial",
    "organization"
  )
}


.get_cols_to_unchop_for_list_resources <- function() {
  c(
    "url",
    "format",
    "created_at",
    "filesize",
    "filetype"
  )
}



