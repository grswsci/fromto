#' 简化版多线程解压并合并CSV文件函数（带内存优化）
#'
#' @param target_dir 目标目录路径
#' @param parallel 是否使用并行处理
#' @param max_workers 最大工作进程数
#' @param verbose 是否显示详细信息
#' @param file_pattern_zip ZIP文件匹配模式
#' @param file_pattern_csv CSV文件匹配模式
#' @param csv_read_options CSV读取选项
#' @param add_source_column 是否添加源文件列
#' @param force_gc 是否强制垃圾回收
unzip_and_merge_csv_parallel <- function(
    target_dir = getwd(),
    parallel = TRUE,
    max_workers = 4,
    verbose = TRUE,
    file_pattern_zip = "\\.zip$",
    file_pattern_csv = "\\.csv$",
    csv_read_options = list(stringsAsFactors = FALSE, check.names = FALSE),
    add_source_column = FALSE,
    force_gc = TRUE) {

  # ========== INPUT VALIDATION ==========
  if (!is.character(target_dir) || length(target_dir) != 1) {
    stop("target_dir must be a single character string")
  }

  if (!dir.exists(target_dir)) {
    stop("Target directory does not exist: ", target_dir)
  }

  if (!is.logical(parallel) || length(parallel) != 1) {
    stop("parallel must be a single logical value")
  }

  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("verbose must be a single logical value")
  }

  if (!is.numeric(max_workers) || length(max_workers) != 1 || max_workers < 1) {
    stop("max_workers must be a positive integer")
  }

  if (!is.logical(force_gc) || length(force_gc) != 1) {
    stop("force_gc must be a single logical value")
  }

  # ========== DEPENDENCY CHECKING ==========
  required_packages <- c("future", "furrr", "progressr")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
  }

  # Load required libraries
  library(future)
  library(furrr)
  library(progressr)

  # ========== PARALLEL PROCESSING SETUP ==========
  if (parallel) {
    original_plan <- plan()
    plan(multisession, workers = max_workers)
    on.exit(plan(original_plan), add = TRUE)
  }

  if (verbose) {
    message("Starting parallel ZIP extraction and CSV merging")
    message("Target directory: ", target_dir)
    message("Parallel processing: ", ifelse(parallel, paste("YES (", max_workers, " workers)"), "NO"))
    message("Force garbage collection: ", ifelse(force_gc, "YES", "NO"))
  }

  # ========== PHASE 1: ZIP FILE DISCOVERY AND EXTRACTION ==========
  zip_files <- list.files(path = target_dir, pattern = file_pattern_zip, full.names = TRUE)

  if (verbose) {
    message("Found ", length(zip_files), " ZIP files for extraction")
  }

  extraction_results <- list()
  failed_extractions <- data.frame(
    file_name = character(0),
    operation_type = character(0),
    error_message = character(0),
    timestamp = as.POSIXct(character(0)),
    stringsAsFactors = FALSE
  )

  if (length(zip_files) > 0) {
    if (verbose) message("Starting ZIP extraction phase...")

    extraction_results <- with_progress({
      p <- progressor(steps = length(zip_files))

      future_map(zip_files, function(zip_file) {
        tryCatch({
          # Extract ZIP file
          unzip(zip_file, exdir = target_dir)

          # Get extracted file info
          extracted_files <- unzip(zip_file, list = TRUE)

          result <- list(
            zip_file = zip_file,
            extracted_files = extracted_files$Name,
            file_count = nrow(extracted_files),
            success = TRUE,
            error_message = NA_character_,
            timestamp = Sys.time()
          )

          p(sprintf("✓ Extracted %s (%d files)", basename(zip_file), nrow(extracted_files)))
          return(result)

        }, error = function(e) {
          if (verbose) {
            warning("Extraction failed for ", basename(zip_file), ": ", e$message)
          }

          p(sprintf("✗ Failed %s", basename(zip_file)))

          return(list(
            zip_file = zip_file,
            extracted_files = character(0),
            file_count = 0,
            success = FALSE,
            error_message = e$message,
            timestamp = Sys.time()
          ))
        })
      }, .options = furrr_options(seed = TRUE))
    })

    # 清理文件名作为结果列表的名称
    clean_names <- make.names(basename(zip_files))
    names(extraction_results) <- clean_names

    # Track extraction failures
    failed_extractions_list <- extraction_results[!sapply(extraction_results, function(x) x$success)]
    if (length(failed_extractions_list) > 0) {
      failed_extractions <- rbind(failed_extractions, data.frame(
        file_name = sapply(failed_extractions_list, function(x) basename(x$zip_file)),
        operation_type = "ZIP_EXTRACTION",
        error_message = sapply(failed_extractions_list, function(x) x$error_message),
        timestamp = do.call(c, lapply(failed_extractions_list, function(x) x$timestamp)),
        stringsAsFactors = FALSE
      ))
    }

    # 强制垃圾回收
    if (force_gc) {
      if (verbose) message("Performing garbage collection after ZIP extraction...")
      gc(verbose = FALSE)
    }
  }

  # ========== PHASE 2: CSV FILE DISCOVERY AND READING ==========
  csv_files <- list.files(path = target_dir, pattern = file_pattern_csv, full.names = TRUE)

  if (verbose) {
    message("Found ", length(csv_files), " CSV files for processing")
  }

  if (length(csv_files) == 0) {
    if (verbose) {
      message("No CSV files found. Returning empty result.")
    }

    return(list(
      merged_data = data.frame(),
      file_info = data.frame(),
      extraction_results = extraction_results,
      failed_operations = failed_extractions,
      metadata = list(
        total_zip_files = length(zip_files),
        successful_extractions = sum(sapply(extraction_results, function(x) x$success)),
        total_csv_files = 0,
        successful_csv_reads = 0,
        total_rows = 0,
        total_columns = 0,
        processing_date = Sys.time(),
        parallel_used = parallel,
        max_workers = if (parallel) max_workers else 1,
        target_directory = target_dir,
        extraction_success_rate = if (length(zip_files) > 0) sum(sapply(extraction_results, function(x) x$success)) / length(zip_files) else 1,
        csv_success_rate = 0,
        overall_success_rate = 0,
        force_gc_used = force_gc
      )
    ))
  }

  if (verbose) message("Starting CSV reading phase...")

  # ========== PHASE 3: PARALLEL CSV READING ==========
  csv_results <- with_progress({
    p <- progressor(steps = length(csv_files))

    future_map(csv_files, function(csv_file) {
      if (verbose) {
        message("Reading: ", basename(csv_file))
      }

      tryCatch({
        # Read CSV with custom options
        read_args <- c(list(file = csv_file), csv_read_options)
        data <- do.call(read.csv, read_args)

        # Optionally add source file information
        if (add_source_column) {
          data$source_file <- basename(csv_file)
        }

        # Store original column count (before any modifications)
        original_col_count <- ncol(data) - ifelse(add_source_column, 1, 0)

        # Get file info
        file_info <- file.info(csv_file)

        result <- list(
          csv_file = csv_file,
          data = data,
          row_count = nrow(data),
          col_count = original_col_count,
          file_size = file_info$size,
          success = TRUE,
          error_message = NA_character_,
          timestamp = Sys.time()
        )

        p(sprintf("✓ Read %s (%d rows, %d cols)", basename(csv_file), nrow(data), original_col_count))
        return(result)

      }, error = function(e) {
        if (verbose) {
          warning("CSV reading failed for ", basename(csv_file), ": ", e$message)
        }

        p(sprintf("✗ Failed %s", basename(csv_file)))

        return(list(
          csv_file = csv_file,
          data = NULL,
          row_count = 0,
          col_count = 0,
          file_size = 0,
          success = FALSE,
          error_message = e$message,
          timestamp = Sys.time()
        ))
      })
    }, .options = furrr_options(seed = TRUE))
  })

  names(csv_results) <- make.names(basename(csv_files))

  # 强制垃圾回收
  if (force_gc) {
    if (verbose) message("Performing garbage collection after CSV reading...")
    gc(verbose = FALSE)
  }

  # ========== PHASE 4: DATA PROCESSING AND MERGING ==========
  successful_csv_results <- csv_results[sapply(csv_results, function(x) x$success)]
  failed_csv_results <- csv_results[sapply(csv_results, function(x) !x$success)]

  # Track CSV reading failures
  if (length(failed_csv_results) > 0) {
    csv_failures <- data.frame(
      file_name = sapply(failed_csv_results, function(x) basename(x$csv_file)),
      operation_type = "CSV_READING",
      error_message = sapply(failed_csv_results, function(x) x$error_message),
      timestamp = do.call(c, lapply(failed_csv_results, function(x) x$timestamp)),
      stringsAsFactors = FALSE
    )
    failed_extractions <- rbind(failed_extractions, csv_failures)
  }

  # Merge successful CSV data
  if (length(successful_csv_results) > 0) {
    if (verbose) message("Merging ", length(successful_csv_results), " CSV files...")

    merged_data <- do.call(rbind, lapply(successful_csv_results, function(x) x$data))
    rownames(merged_data) <- NULL

    # 强制垃圾回收
    if (force_gc) {
      if (verbose) message("Performing garbage collection after data merging...")
      gc(verbose = FALSE)
    }

    # Create file info summary
    file_info <- data.frame(
      file_name = sapply(successful_csv_results, function(x) basename(x$csv_file)),
      file_size = sapply(successful_csv_results, function(x) x$file_size),
      row_count = sapply(successful_csv_results, function(x) x$row_count),
      col_count = sapply(successful_csv_results, function(x) x$col_count),
      processing_status = "SUCCESS",
      stringsAsFactors = FALSE
    )

    # Add failed files to file_info
    if (length(failed_csv_results) > 0) {
      failed_file_info <- data.frame(
        file_name = sapply(failed_csv_results, function(x) basename(x$csv_file)),
        file_size = sapply(failed_csv_results, function(x) x$file_size),
        row_count = 0,
        col_count = 0,
        processing_status = "FAILED",
        stringsAsFactors = FALSE
      )
      file_info <- rbind(file_info, failed_file_info)
    }

  } else {
    merged_data <- data.frame()
    file_info <- data.frame(
      file_name = character(0),
      file_size = numeric(0),
      row_count = numeric(0),
      col_count = numeric(0),
      processing_status = character(0),
      stringsAsFactors = FALSE
    )
  }

  # ========== PHASE 5: FINAL STATISTICS AND REPORTING ==========
  successful_extractions <- sum(sapply(extraction_results, function(x) x$success))
  successful_csv_reads <- length(successful_csv_results)

  if (verbose) {
    message("\n", strrep("=", 60))
    message("Processing completed!")
    message("ZIP files: ", successful_extractions, "/", length(zip_files), " extracted successfully")
    message("CSV files: ", successful_csv_reads, "/", length(csv_files), " read successfully")

    if (nrow(merged_data) > 0) {
      original_cols <- ncol(merged_data) - ifelse(add_source_column, 1, 0)
      message("Final dataset: ", nrow(merged_data), " rows, ", original_cols, " columns")
      if (add_source_column) {
        message("Data sources: ", length(unique(merged_data$source_file)), " files")
      }
    }

    if (nrow(failed_extractions) > 0) {
      message("\nFailed operations: ", nrow(failed_extractions))
      for (i in seq_len(min(5, nrow(failed_extractions)))) {
        message("  - ", failed_extractions$operation_type[i], ": ",
                failed_extractions$file_name[i], " (", failed_extractions$error_message[i], ")")
      }
      if (nrow(failed_extractions) > 5) {
        message("  ... and ", nrow(failed_extractions) - 5, " more failures")
      }
    }

    extraction_rate <- if (length(zip_files) > 0) successful_extractions / length(zip_files) else 1
    csv_rate <- if (length(csv_files) > 0) successful_csv_reads / length(csv_files) else 0
    overall_rate <- if (length(zip_files) + length(csv_files) > 0) {
      (successful_extractions + successful_csv_reads) / (length(zip_files) + length(csv_files))
    } else 0

    message("Success rates: Extraction ", round(extraction_rate * 100, 1), "%, ",
            "CSV reading ", round(csv_rate * 100, 1), "%, ",
            "Overall ", round(overall_rate * 100, 1), "%")
    message(strrep("=", 60))
  }

  # 最终强制垃圾回收
  if (force_gc) {
    if (verbose) message("Final garbage collection...")
    gc(verbose = FALSE)
  }

  # ========== RETURN COMPREHENSIVE RESULTS ==========
  return(list(
    merged_data = merged_data,
    file_info = file_info,
    extraction_results = extraction_results,
    failed_operations = failed_extractions,
    metadata = list(
      total_zip_files = length(zip_files),
      successful_extractions = successful_extractions,
      total_csv_files = length(csv_files),
      successful_csv_reads = successful_csv_reads,
      total_rows = nrow(merged_data),
      total_columns = if (nrow(merged_data) > 0) {
        # Calculate original columns (excluding source_file if added)
        ncol(merged_data) - ifelse(add_source_column, 1, 0)
      } else 0,
      processing_date = Sys.time(),
      parallel_used = parallel,
      max_workers = if (parallel) max_workers else 1,
      target_directory = target_dir,
      extraction_success_rate = if (length(zip_files) > 0) successful_extractions / length(zip_files) else 1,
      csv_success_rate = if (length(csv_files) > 0) successful_csv_reads / length(csv_files) else 0,
      overall_success_rate = if (length(zip_files) + length(csv_files) > 0) {
        (successful_extractions + successful_csv_reads) / (length(zip_files) + length(csv_files))
      } else 0,
      force_gc_used = force_gc
    )
  ))
}
