# =============================================================================
# Configuration Management
# =============================================================================
# Functions for loading and accessing pipeline configuration
# =============================================================================

library(yaml)
library(here)

.config <- NULL

#' Load pipeline configuration
#'
#' @param config_file Path to YAML configuration file
#' @return List containing configuration
load_config <- function(config_file = here("config/pipeline_config.yaml")) {
  if (!file.exists(config_file)) {
    stop("Configuration file not found: ", config_file, call. = FALSE)
  }

  .config <<- read_yaml(config_file)
  invisible(.config)
}

#' Get configuration value
#'
#' @param ... Path to configuration value (e.g., "general", "random_seed")
#' @param default Default value if not found
#' @return Configuration value
get_config <- function(..., default = NULL) {
  if (is.null(.config)) {
    load_config()
  }

  path.lst <- list(...)
  value <- .config

  for (key in path.lst) {
    if (is.null(value[[key]])) {
      if (!is.null(default)) {
        return(default)
      }
      stop("Configuration key not found: ", paste(path.lst, collapse = " -> "),
           call. = FALSE)
    }
    value <- value[[key]]
  }

  return(value)
}

#' Get data path from configuration
#'
#' @param ... Path keys (e.g., "processed", "covars_fst")
#' @return Full path to data file
get_data_path <- function(...) {
  rel_path <- get_config("data", ...)
  here(rel_path)
}

#' Get script setting
#'
#' @param script_name Name of script (e.g., "gamlss")
#' @param setting Setting name (e.g., "redo_plots")
#' @param default Default value
get_script_setting <- function(..., default = NULL) {
  # Supports nested paths
  # e.g.: get_script_setting("lgcm", "bootstrap", "enabled")
  path_elements.lst <- list(...)
  do.call(get_config, c(list("scripts"), path_elements.lst,
                        list(default = default)))
}

#' Get parameter value
#'
#' @param ... Path to parameter
#' @param default Default value
get_parameter <- function(..., default = NULL) {
  get_config("parameters", ..., default = default)
}

#' Get random seed
#'
get_seed <- function() {
  get_config("general", "random_seed", default = 1618)
}

#' Set random seed from configuration
#'
set_seed <- function() {

  seed <- get_seed()
  set.seed(seed)
  invisible(seed)
}

#' Validate configuration has required fields
#'
#' Checks that all required configuration sections and keys exist.
#' Call after load_config() to ensure config is complete.
#'
#' @param config Configuration list (default: loaded config)
#' @return TRUE if valid, throws error otherwise
validate_config <- function(config = NULL) {
  if (is.null(config)) {
    if (is.null(.config)) {
      load_config()
    }
    config <- .config
  }

  # Required top-level sections
  required_sections.v <- c("general", "data", "parameters", "scripts")
  missing_sections.v <- setdiff(required_sections.v, names(config))
  if (length(missing_sections.v) > 0) {
    stop("Missing required config sections: ",
         paste(missing_sections.v, collapse = ", "), call. = FALSE)
  }

  # Required general settings
  required_general.v <- c("random_seed")
  missing_general.v <- setdiff(required_general.v, names(config$general))
  if (length(missing_general.v) > 0) {
    stop("Missing required general settings: ",
         paste(missing_general.v, collapse = ", "), call. = FALSE)
  }

  # Required data paths
  required_data.v <- c("raw", "derivatives", "models")
  missing_data.v <- setdiff(required_data.v, names(config$data))
  if (length(missing_data.v) > 0) {
    stop("Missing required data sections: ",
         paste(missing_data.v, collapse = ", "), call. = FALSE)
  }

  invisible(TRUE)
}
