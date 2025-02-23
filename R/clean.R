data_clean = data[rowSums(!is.na(data)) / ncol(data) > 0.2, ]
