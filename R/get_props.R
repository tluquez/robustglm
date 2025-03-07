#' Summarize data by group
#'
#' This function aggregates data by group and summarizes specified columns.
#'
#' @param data A data frame.
#' @param group A character vector specifying the grouping column(s).
#' @param columns Optional character vector specifying columns to summarize.
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
    df <- dplyr::distinct(data, dplyr::across(tidyselect::all_of(group)))
  }

  # If columns need to be summarized, summarize columns by group
  df <- data %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(group))) %>%
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
          else if (is.list(.)) NA
          else if (is.complex(.)) NA
          else NA
        }
      ),
      .groups = "drop_last"
    ) %>%
    dplyr::ungroup()

  return(df)
}

#' Compute Proportions
#'
#' This function computes proportions per id across cluster. It can group by
#'  supercluster and compute proportions per group. It can also add
#'  supercluster proportions across all its levels. Finally, it summarizes
#'  other columns of data per id.
#'
#' @param data Data frame containing \code{id} and \code{cluster}.
#' @param id The column name representing the observations identifier.
#' @param cluster The column name representing the cluster identifier.
#' @param supercluster Optional. The column name representing the supercluster
#'  identifier.
#' @param add_supercluster_prop Logical indicating whether to
#'  include proportions per id across superclusters.
#' @param add_other_cols Logical indicating whether to summarize other columns of \code{data} by \code{id}. Numeric columns are summarized to their mean, factor to the reference level and character to the mode.
#'
#' @return A long data frame with computed proportions per group and summary
#'  statistics.
#' - \code{"num_cluster"}: The number of occurrences of each group combination.
#' - \code{"props"}: The proportions of each group combination within its
#'  supercluster.
#' - \code{"supercluster"}: The supercluster associated with each group.
#' - \code{"num_supercluster"}: The total number of occurrences of each
#'  \code{supercluster}-\code{id} combination.
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
#' @importFrom magrittr %>%
#'
#' @export
#'

get_props <- function(data, id, cluster, supercluster = NULL,
                      add_supercluster_prop = F, add_other_cols = T) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  # Input Validation
  required_cols <- c(id, cluster)
  if (!is.null(supercluster)) {
    required_cols <- c(required_cols, supercluster)
  }
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("The following columns are missing in the data: ",
         paste(missing_cols, collapse = ", "))
  }
  invalid_args <- c(
    "id" = !is.character(id),
    "cluster" = !is.character(cluster),
    "supercluster" = !is.null(supercluster) && !is.character(supercluster)
  )
  invalid_args <- names(invalid_args)[invalid_args]
  if (length(invalid_args) > 0) {
    stop("Argument(s) ", paste(invalid_args, collapse = ", "),
         " must be character vector(s).")
  }

  # Proportion Calculation
  if (!is.null(supercluster)) {
    # Proportions by supercluster x cluster x id
    data_groups <- split(data, data[[supercluster]])
    props <- lapply(data_groups, function(data_group) {
      # Proportion per supercluster by cluster x id
      nums_cluster <- table(data_group[[id]], data_group[[cluster]])
      prop <- as.data.frame(prop.table(nums_cluster, margin = 1))
      nums_cluster <- as.data.frame(nums_cluster)

      # Count observations per supercluster x id
      nums_supercluster <- data.frame(table(data_group[[id]],
                                            data_group[[supercluster]]))

      # Left join and rename
      prop <- merge(nums_cluster, prop, by = c("Var1", "Var2"))
      prop <- merge(prop, nums_supercluster, by = "Var1")
      colnames(prop) <- c(id, cluster, paste0("num_", cluster), "prop",
                          supercluster, paste0("num_", supercluster))

      return(prop)
    })
    props <- do.call(rbind, props)
    rownames(props) <- NULL

    # Add proportions per supercluster column if requested
    if (add_supercluster_prop) {
      # Proportion by supercluster x id
      nums_supercluster <- table(data[[id]], data[[supercluster]])
      prop <- as.data.frame(prop.table(nums_supercluster, margin = 1))
      nums_supercluster <- as.data.frame(nums_supercluster)

      # Count observations per id across superclusters
      nums_id <- data.frame(table(data[[id]]))
      nums_id$Var2 <- supercluster
      nums_id <- nums_id[, c("Var1", "Var2", "Freq")]

      # Left join and rename
      prop <- merge(nums_supercluster, prop, by = c("Var1", "Var2"))
      prop <- merge(prop, nums_id, by = "Var1")
      colnames(prop) <- c(id, cluster, paste0("num_", cluster), "prop",
                          supercluster, paste0("num_", supercluster))

      props <- rbind(props, prop)
    }
  } else {
    # Proportion by cluster x id
    nums_cluster <- table(data[[id]], data[[cluster]])
    prop <- as.data.frame(prop.table(nums_cluster, margin = 1))
    nums_cluster <- as.data.frame(nums_cluster)

    # Merge and clean column names
    props <- merge(nums_cluster, prop, by = c("Var1", "Var2"))
    colnames(props) <- c(id, cluster, paste0("num_", cluster), "prop")

    props
  }

  # Ensure column types conform to original data
  props[[id]] <- as(props[[id]], class(data[[id]]))
  props[[cluster]] <- as(props[[cluster]], class(data[[cluster]]))
  if (!is.null(supercluster)) {
    props[[supercluster]] <- as(props[[supercluster]],
                                class(data[[supercluster]]))
  }

  # Summarize other columns by id
  other_columns <- setdiff(names(data), required_cols)
  if (add_other_cols && length(other_columns) > 0) {
    df <- summarize_by_group(data, id, other_columns)
    props <- dplyr::left_join(props, df, by = id)
  }

  return(props)
}