# given a table name, return a function that takes a dataset reference and
# returns data from the given table in that dataset
table_getter <- function(table_name, max_results = NULL) {
  \(dataset, dataset_table_names) {
    message(glue::glue("--Fetching table {table_name}"))
    if (!(table_name %in% dataset_table_names)) return(tibble())
    suppressWarnings(
      dataset$table(table_name)$to_tibble(max_results = max_results)
    )
  }
}

# given a sql query, return a function that takes a dataset reference and
# returns data from executing that sql query in that dataset
query_getter <- function(table_name, query_str, max_results = NULL) {
  \(dataset, dataset_table_names) {
    message(glue::glue("--Executing query on table {table_name}"))
    if (!(table_name %in% dataset_table_names)) return(tibble())
    suppressWarnings(
      q <- dataset$query(query_str)$get()
    )
    if (q$properties$outputNumRows == 0) return(tibble())
    q$to_tibble(max_results = max_results)
  }
}

# given a dataset spec (list of lists of name and version strings)
# and a function that takes a dataset and returns a tibble
# run the function for each dataset and combine results into one tibble
get_datasets_data <- function(dataset_spec, dataset_fun) {

  # get reference to organization
  org <- redivis::redivis$organization("levante")

  # get reference to each dataset in dataset_spec
  datasets <- dataset_spec |>
    purrr::set_names(purrr::map_chr(dataset_spec, \(dn) dn[["name"]])) |>
    purrr::map(\(dn) org$dataset(name = dn$name, version = dn$version))

  # fetch each dataset to populate its properties
  purrr::walk(datasets, \(ds) ds$get())

  # get each dataset's canonical reference (name + persistent ID + version)
  dataset_refs <- datasets |> purrr::map(\(ds) ds$scoped_reference)

  # apply dataset_fun to each dataset
  dataset_data <- purrr::imap(datasets, \(dataset, dataset_name) {
    message(glue::glue("Fetching data for {dataset_name}"))
    dataset_table_names <- dataset$list_tables() |> purrr::map_chr(\(tbl) tbl$name)
    dataset_fun(dataset, dataset_table_names) |>
      mutate(redivis_source = dataset_refs[[dataset_name]], .before = everything())
  })

  dataset_data |>
    # combine data over datasets
    bind_rows() |>
    # remove schema row
    filter(if_any(matches("_id"), \(v) v != "schema_row"))
}

# construct SQL WHERE clause out of a variable name and vector of allowed values
build_filter <- function(var, vals, allow_null = FALSE) {
  vals_str <- glue::glue("'{vals}'") |> paste(collapse = ", ")
  null <- if (allow_null) glue::glue("{var} IS NULL OR ") else ""
  if (is.null(vals)) "" else glue::glue("WHERE {null}{var} IN ({vals_str})")
}
