#' Compute Proportions
#'
#' This function computes proportions per id across cluster. It can group by
#'  supercluster and compute proportions per group(s). It can also add
#'  supercluster proportions across all its levels. Finally, it summarizes
#'  other columns of data per grouping columns.
#'
#' @param data Data frame containing \code{id} and \code{cluster}.
#' @param id Character. Column name representing the observations identifier.
#' @param cluster Character. Column name representing the cluster identifier.
#' @param supercluster Character. Column name representing the supercluster
#'  identifier. Optional.
#' @param by Character. Column name(s) of grouping variables, a.k.a.
#'  denominators for proportions. Optional.
#' @param add_supercluster_prop Logical indicating whether to include
#'  proportions per id across superclusters.
#' @param add_other_cols Logical indicating whether to summarize other columns of \code{data} by \code{c(id, supercluster, by)}. Numeric columns are summarized
#' to their mean, factor to the reference level and character to the mode.
#'
#' @return A long data frame with computed proportions per group and summary
#'  statistics.
#' - \code{"num_cluster"}: The number of occurrences of each group combination.
#' - \code{"props"}: The proportions of each group combination within its
#'  supercluster.
#' - \code{"supercluster"}: The supercluster associated with each group.
#'
#' @details This function calculates proportions per group based on the counts of another
#' column, and summarizes other columns by group, calculating means for numeric columns,
#' reference levels for factor columns, and the mode for character columns.
#'
#' @examples
#' df <- data.frame(id = rep(1:3, each = 8),
#'                  cluster = rep(c("C1", "C2"), each = 4, times = 3),
#'                  supercluster = rep(c("S1", "S2"), each = 4, times = 3),
#'                  value1 = runif(24),
#'                  value2 = as.factor(LETTERS[1:12]),
#'                  value3 = LETTERS[1:12],
#'                  value4 = NA)
#' df
#' get_props(df, "id", "cluster", "supercluster", add_supercluster_prop = T)
#' get_props(df, "id", "cluster", "supercluster")
#' get_props(df, "id", "cluster")
#'
#' @import dplyr
#' @import tidyr
#' @import rlang
#' @importFrom magrittr %>%
#'
#' @export
#'

get_props <- function(data, id, cluster, supercluster = NULL, by = NULL,
                      add_supercluster_prop = F, add_other_cols = T) {

  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  if (add_supercluster_prop & is.null(supercluster)) {
    stop("No supercluster column provided when add_supercluster_prop == TRUE.")
  }

  # Respect existing groups when called in a dplyr pipeline
  if (dplyr::is_grouped_df(data)) {
    if (is.null(by)) {
      by <- dplyr::group_vars(data)
    }
    data <- dplyr::ungroup(data)
  }

  groups <- c(id, cluster)
  if (!is.null(supercluster)) {
    groups <- c(groups, supercluster)
  }
  if (!is.null(by)) {
    groups <- c(groups, by)
  }

  missing_cols <- setdiff(groups, colnames(data))
  if (length(missing_cols) > 0) {
    stop("The following columns are missing in the data: ",
         paste(missing_cols, collapse = ", "))
  }

  # Count cells and compute proportions
  margins <- setdiff(groups, cluster)
  n_cluster <- table(data[groups])
  prop <- as.data.frame(prop.table(n_cluster, margin = margins),
                        stringsAsFactors = F)
  n_cluster <- as.data.frame(n_cluster, stringsAsFactors = F)
  colnames(prop)[colnames(prop) == "Freq"] <- "prop"
  colnames(n_cluster)[colnames(n_cluster) == "Freq"] <- paste0("num_", cluster)
  props <- merge(n_cluster, prop, by = groups)

  # Exclude non-existing combinations
  props <- props[!is.nan(props$prop), ]

  if (!is.null(supercluster) & add_supercluster_prop) {
    # Count cells per id across superclusters and compute proportions
    if(supercluster %in% groups) {
      groups_supercluster <- setdiff(groups, cluster)
    } else {
      # For consistency, ensure id and supercluster are the first two variables
      groups_supercluster <- c(id, supercluster, setdiff(groups, c(id, cluster)))
    }
    margins_supercluster <- setdiff(groups_supercluster, supercluster)
    n_supercluster <- table(data[groups_supercluster])
    prop_supercluster <- as.data.frame(prop.table(
      n_supercluster, margin = margins_supercluster), stringsAsFactors = F)
    n_supercluster <- as.data.frame(n_supercluster, stringsAsFactors = F)
    colnames(prop_supercluster)[colnames(prop_supercluster) == "Freq"] <- "prop"
    props_supercluster <- merge(n_supercluster, prop_supercluster,
                                by = groups_supercluster)

    # Exclude non-existing combinations
    props_supercluster <- props_supercluster[!is.nan(props_supercluster$prop), ]

    # Add supercluster proportions as tho they are cluster props
    colnames(props_supercluster)[colnames(props_supercluster) == "Freq"] <-
      paste0("num_", cluster)
    colnames(props_supercluster)[
      colnames(props_supercluster) == supercluster] <- cluster
    props_supercluster[supercluster] <- supercluster
    props <- rbind(props, props_supercluster, make.row.names = F)
  }

  # Restore column to original class
  props <- restore_class(props, data, groups)
  # props[[id]] <- as(props[[id]], class(data[[id]]))
  # props[[cluster]] <- as(props[[cluster]], class(data[[cluster]]))
  # if (!is.null(supercluster)) {
  #   props[[supercluster]] <- as(props[[supercluster]],
  #                               class(data[[supercluster]]))
  # }

  # Add other columns if required, excluding grouping columns
  if (add_other_cols) {
    other_columns <- setdiff(names(data), groups)
    if (length(other_columns) > 0) {
      df <- summarize_by_group(data, groups, other_columns)
      props <- merge(props, df, by = groups)
    }
  }

  # **Checks**

  ## Ensure no new non-zero id-cluster combinations were created
  original_combs <- dplyr::distinct(data[groups])
  result_combs <- props[props[[paste0("num_", cluster)]] > 0, groups]
  if (!is.null(supercluster) & add_supercluster_prop) {
    result_combs <- result_combs[result_combs[supercluster] != supercluster,
                                 groups]
  }
  new_combs <- dplyr::anti_join(result_combs, original_combs,
                                by = groups)

  if (nrow(new_combs) > 0) {
    stop("Unexpected id x cluster x by x (supercluster) combinations.")
  }

  ## Ensure proportions sum to ~1 (allow for minor floating-point errors)
  props_sum <- props %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(margins))) %>%
    dplyr::summarize(n = round(sum(prop), 5), .groups = "drop") %>%
    dplyr::filter(n != 0) %>%
    dplyr::pull(n) %>%
    unique()

  if (any(abs(props_sum - 1) > 0.001)) {
    stop("Proportions do not sum to 1.")
  }

  ## Ensure supercluster proportions sum to 1 per id
  if (!is.null(supercluster) && add_supercluster_prop) {
    supercluster_props_sum <- props %>%
      dplyr::filter(!!rlang::sym(supercluster) == !!supercluster) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(groups_supercluster))) %>%
      dplyr::summarize(n = round(sum(prop), 5), .groups = "drop") %>%
      dplyr::pull(n) %>%
      unique()

    if (any(abs(supercluster_props_sum - 1) > 0.001)) {
      stop("Supercluster proportions do not sum to 1.")
    }
  }

  ## Ensure id, cluster and supercluster levels are preserved
  for (col in groups) {
    if (!is.null(col) && any(!unique(data[[col]]) %in% unique(props[[col]]))) {
      stop("Column '", col, "' lost some unique levels in the output.")
    }
  }

  ## Ensure all by groups are preserved
  if (!is.null(by)) {
    input_groups <- dplyr::distinct(data, dplyr::across(dplyr::all_of(by)))
    output_groups <- dplyr::distinct(props, dplyr::across(dplyr::all_of(by)))

    if (nrow(dplyr::anti_join(input_groups, output_groups, by = by)) > 0) {
      stop("Some groups from 'by' are missing in the output.")
    }
  }

  return(props)
}

#' Summarize data by group
#'
#' This function aggregates data by group and summarizes specified columns.
#'
#' @param data Data frame containing \code{group} and \code{columns}.
#' @param group Character. Vector specifying the grouping column(s).
#' @param columns Character. Vector specifying column(s) to summarize. Optional.
#' @return A data frame with summarized data.
#' @examples
#' mixed_df <- data.frame(
#'   Group = rep(letters[1:3], each = 3),
#'   Numeric_Value = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'   Factor_Value = factor(c("low", "medium", "high"),
#'    levels = c("low", "medium", "high")),
#'   Character_Value = c("apple", "banana", "apple", "banana", "apple",
#'    "banana", "apple", "banana", "apple"),
#'   Logical_Value = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE,
#'    TRUE),
#'   Date_Value = as.Date("2022-01-01") + 0:8,
#'   Complex_Value = as.complex(1:9)
#' )
#'
#' # Summarize all columns (default) by Group
#' summarize_by_group(mixed_df, "Group")
#'
#' # Summarize column Numeric_Value by Group
#' summarize_by_group(mixed_df, "Group", "Numeric_Value")
#'
#' # Summarize two columns by Group
#' summarize_by_group(mixed_df, "Group", c("Numeric_Value", "Factor_Value"))
#'
#' @import dplyr
#' @importFrom purrr map imap compact
#'
#' @export

summarize_by_group <- function(data, group, columns = NULL) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("data must be a dataframe")
  }
  if (!is.character(group) || length(group) == 0) {
    stop("group must be a non-empty character vector")
  }
  if (!is.null(columns) && (!is.character(columns) || length(columns) == 0)) {
    stop("columns must be a non-empty character vector")
  }

  # Get the columns to summarize
  if (is.null(columns)) {
    columns <- setdiff(names(data), group)
  }

  # If no columns to summarize, return unique combinations of group columns
  if (length(columns) == 0) {
    df <- dplyr::distinct(data, dplyr::across(dplyr::all_of(group)))
  }

  # If columns need to be summarized, summarize columns by group
  df <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group))) %>%
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(columns),
        ~ {
          if (all(is.na(.))) NA
          else if (is.numeric(.)) mean(., na.rm = T)
          else if (is.factor(.)) levels(.)[1]
          else if (is.character(.)) unique(.)[which.max(table(.))]
          else if (is.logical(.)) unique(.)[which.max(table(.))]
          else if (inherits(., c("Date", "POSIXt"))) as.character(unique(.))[1]
          else if (is.list(.) || is.complex(.)) NA
          else NA
        }
      ),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()

  # **Checks**

  ## Capture original unique value counts for categorical columns
  original_levels <- purrr::map(data[columns], function(col) {
    if (is.character(col) || is.factor(col) || is.logical(col)) {
      unique(col)
    } else {
      NULL
    }
  })

  # Capture final unique values
  final_levels <- purrr::map(df[columns], function(col) {
    if (is.character(col) || is.factor(col) || is.logical(col)) {
      unique(col)
    } else {
      NULL
    }
  })

  # Compare and issue warnings if data loss occurred
  info_loss <- purrr::imap(original_levels, function(orig_values, col_name) {
    final_values <- final_levels[[col_name]]
    if (!is.null(orig_values) && length(orig_values) > 1 &&
        length(final_values) == 1) {
      paste0("Column '", col_name, "' had ", length(orig_values),
             " unique values but was summarized to ", length(final_values), ".")
    } else {
      NULL
    }
  }) %>%
    purrr::compact()

  if (length(info_loss) > 0) {
    warning(paste(info_loss, collapse = "\n"))
  }

  return(df)
}

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