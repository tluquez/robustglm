#' Compute Proportions of Clustered Observations
#'
#' This function computes the proportion of observations within each cluster,
#' optionally stratified by additional grouping variables.
#'
#' @param data A data frame containing the dataset.
#' @param id A string specifying the column name of the unique sample identifier.
#' @param cluster A string specifying the column name of the cluster identifier.
#' @param prop_by (Optional) A string specifying columns to group by when computing proportions. Columns to fill with explicit zeroes.
#' @param split_by (Optional) A vector of strings specifying grouping columns
#'   for which separate proportion calculations should be performed. Columns
#'   not to fill with explicit zeroes.
#'
#' @return A data frame with cluster proportions and counts per unique combination
#'   of the grouping variables.
#'
#' @importFrom dplyr is_grouped_df group_vars ungroup full_join bind_rows left_join distinct anti_join group_by
#' @importFrom dplyr summarise filter pull across all_of
#'
#' @examples
#' library(dplyr)
#'
#' # Example dataset
#' set.seed(42)
#' data <- data.frame(
#'   id = rep(1:5, each = 4),
#'   cluster = sample(letters[1:3], 20, replace = TRUE),
#'   prop_by = rep(c("X", "Y"), each = 10),
#'   split_by = rep(c("A", "B"), times = 10)
#' )
#'
#' # Basic usage
#' get_props(data, id = "id", cluster = "cluster")
#'
#' # Add explicit zeroes to all levels of "prop_by"
#' get_props(data, id = "id", cluster = "cluster", prop_by = "prop_by")
#'
#' # Ensure no new combinations outside "split_by" are added
#' get_props(data, id = "id", cluster = "cluster", split_by = "split_by")
#'
#' # It can also be integrated into piped commands. Grouped dataframes are
#' treated as split_by.
#' data %>%
#'   group_by(split_by) %>%
#'   get_props(id = "id", cluster = "cluster", prop_by = "prop_by")
#'
#' @export

get_props <- function(data, id, cluster, prop_by = NULL, split_by = NULL) {

  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  # Respect existing groups when called in a dplyr pipeline
  if (dplyr::is_grouped_df(data)) {
    if (is.null(split_by)) {
      split_by <- dplyr::group_vars(data)
    } else {
      stop("'split_by' on a grouped data frame. use only one syntax to group.")
    }
    data <- dplyr::ungroup(data)
  }

  # Ensure required columns exist
  all_grouping_vars <- unique(c(id, cluster, prop_by, split_by))
  missing_cols <- setdiff(all_grouping_vars, colnames(data))
  if (length(missing_cols) > 0) {
    stop("The following columns are missing: ",
         paste(missing_cols, collapse = ", "))
  }

  # Count cells and compute proportions
  compute_props <- function(data, id, cluster, prop_by) {
    if (!is.null(prop_by)) {
      groups <- c(id, cluster, prop_by)
    } else {
      groups <- c(id, cluster)
    }
    n_cluster <- table(data[groups])
    prop <- as.data.frame(prop.table(n_cluster, margin = id),
                          stringsAsFactors = F, responseName = "prop")
    n_cluster <- as.data.frame(n_cluster, stringsAsFactors = F,
                               responseName = paste0("num_", cluster))
    dplyr::full_join(n_cluster, prop, by = groups)
  }
  if (!is.null(split_by)) {
    # Generate a unique identifier for split_by combinations
    data$split_id <- as.character(interaction(data[split_by], drop = T))
    data_splits <- split(data, data$split_id)

    # Compute proportions for each split
    props_list <- lapply(data_splits, function(data_split) {
      compute_props(data_split, id, cluster, prop_by)
    })
    props <- dplyr::bind_rows(props_list, .id = "split_id")

    # Restore original split_by columns
    split_metadata <- unique(data[c("split_id", split_by)])
    props <- dplyr::left_join(props, split_metadata, by = "split_id")

    # Remove temporary ID
    props$split_id <- NULL
  } else {
    props <- compute_props(data, id, cluster, prop_by)
  }

  # Restore column to original class
  props <- restore_class(props, data, colnames(props))

  # **Checks**

  ## Ensure no new non-zero id-cluster combinations were created
  original_combs <- dplyr::distinct(data[all_grouping_vars])
  result_combs <- props[props[paste0("num_", cluster)] > 0, all_grouping_vars]
  new_combs <- dplyr::anti_join(result_combs, original_combs,
                                by = all_grouping_vars)
  if (nrow(new_combs) > 0) {
    stop("Unexpected id x cluster x prop_by x split_by ",
         "combinations.")
  }

  ## Ensure proportions sum to ~1 (allow for minor floating-point errors)
  props_sum <- props %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id, split_by)))) %>%
    dplyr::summarize(n = round(sum(prop, na.rm = T), 5), .groups = "drop") %>%
    dplyr::filter(n != 0) %>%
    dplyr::pull(n) %>%
    unique()
  if (any(abs(props_sum - 1) > 0.001)) {
    stop("Proportions do not sum to 1.")
  }

  ## Ensure all_grouping_vars levels are preserved
  for (col in all_grouping_vars) {
    if (!is.null(col) && any(!unique(data[[col]]) %in% unique(props[[col]]))) {
      stop(paste0("Column '", col, "' lost some unique levels in the output."))
    }
  }

  return(props)
}

#' Restore Column Classes Based on a Reference Data Frame
#'
#' This function restores the column classes of a data frame (`df`)
#' to match those of a reference data frame (`reference_df`).
#' It ensures that factor levels, numeric types, dates, logicals,
#' and other data types are preserved as closely as possible.
#'
#' @param df A data frame whose column classes need to be restored.
#' @param reference_df A data frame that serves as the reference
#'   for column class restoration.
#' @param columns A character vector of column names to restore.
#'
#' @details
#' The function checks if each specified column exists in both `df`
#' and `reference_df`. If so, it attempts to restore the class of `df[[col]]`
#' to match `reference_df[[col]]`.
#'
#' - Factors: Retains levels from the reference data frame.
#' - Characters: Converts to character if originally character.
#' - Numeric/Integer: Converts safely, avoiding unintended `NA` coercion.
#' - Dates (`Date`, `POSIXt`): Converts from character/factor where possible.
#' - Logical: Ensures valid logical values (`TRUE`, `FALSE`, `T`, `F`, `1`, `0`).
#' - Lists/Complex: Converts to character for compatibility.
#'
#' If an unexpected conversion occurs (e.g., numeric values coerced to `NA`),
#' an error is raised.
#'
#' @return A modified version of `df` with restored column classes.
#'
#' @keywords internal
#'
#' @examples
#' reference <- data.frame(
#'   factor_col = factor(c("A", "B")),
#'   num_col = c(1.1, 2.2),
#'   date_col = as.Date(c("2023-01-01", "2023-01-02")),
#'   logical_col = c(TRUE, FALSE)
#' )
#'
#' df <- data.frame(
#'   factor_col = c("A", "B"),  # Stored as character
#'   num_col = as.character(c(1.1, 2.2)),  # Stored as character
#'   date_col = c("2023-01-01", "2023-01-02"),  # Stored as character
#'   logical_col = c("TRUE", "FALSE")  # Stored as character
#' )
#'
#' restored_df <- restore_class(df, reference, colnames(df))
#' str(restored_df)  # Check restored types

restore_class <- function(df, reference_df, columns) {
  for (col in columns) {
    if (col %in% names(reference_df) && col %in% names(df)) {
      original_class <- class(reference_df[[col]])
      current_class <- class(df[[col]])

      tryCatch({
        withCallingHandlers({
          if ("factor" %in% original_class) {
            # Ensure factor levels are retained
            df[[col]] <- factor(df[[col]], levels = levels(reference_df[[col]]))

          } else if ("character" %in% original_class) {
            # Convert to character safely
            df[[col]] <- as.character(df[[col]])

          } else if (any(c("numeric", "integer") %in% original_class)) {
            # Convert safely to numeric, avoiding NA coercion
            if ("factor" %in% current_class) {
              df[[col]] <- as.numeric(as.character(df[[col]]))
            } else if ("character" %in% current_class) {
              if (all(suppressWarnings(!is.na(as.numeric(df[[col]]))))) {
                df[[col]] <- as.numeric(df[[col]])
              } else {
                stop("Values coerced to NA when converting to original class. ",
                     "If add_supercluster_prop = T and '", col,
                     "' is numeric, consider coverting it to character.")
              }
            } else {
              df[[col]] <- as.numeric(df[[col]])
            }

          } else if (any(c("Date", "POSIXt") %in% original_class)) {
            # Convert character or factor back to Date safely
            if (any(c("character", "factor") %in% current_class)) {
              parsed_dates <- as.Date(as.character(df[[col]]),
                                      format = "%Y-%m-%d")
              if (any(is.na(parsed_dates) & !is.na(df[[col]]))) {
                stop(paste("Column", col, "contains invalid date values."))
              }
              df[[col]] <- parsed_dates
            } else {
              df[[col]] <- as(reference_df[[col]], original_class)
            }

          } else if ("logical" %in% original_class) {
            # Convert to logical safely
            if ("factor" %in% current_class || "character" %in% current_class) {
              logical_vals <- as.logical(as.character(df[[col]]))
              if (any(is.na(logical_vals) & !df[[col]] %in%
                      c("TRUE", "FALSE", "T", "F", "1", "0"))) {
                stop(paste("Column", col, "contains non-logical values."))
              }
              df[[col]] <- logical_vals
            } else {
              df[[col]] <- as.logical(df[[col]])
            }

          } else if (any(c("list", "complex") %in% original_class)) {
            # Convert list or complex to character
            df[[col]] <- as.character(df[[col]])

          } else {
            # Preserve any other class
            df[[col]] <- as(df[[col]], original_class)
          }
        }, warning = function(w) {
          stop(paste("Warning treated as error in column:", col, ":",
                     conditionMessage(w)))
        })
      }, error = function(e) {
        message("Error in column ", col, ": ", conditionMessage(e))
        next  # Skip to the next column if an error occurs
      })
    }
  }
  return(df)
}