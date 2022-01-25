#' @title Normalisation and Scaling
#' @name NMRMetab_norm_scale
#' @export
#' @author Eva Caamano Gutierrez
#' @author Arturas Grauslys
#' @param data a data.frame. Column as variable and rows as sample
#' @param index_col integer value. The column number with first bin/matabolite
#' @param normalisation string. one of 'None'#default, 'PQN', 'TotArea', 'Bin'
#' @param scaling string. one of 'None'#default, 'Auto', 'Pareto', 'Range' or 'Mean'
#' @param writeToFile boolean. write resulting data.frame to csv file. F #
#' @param bin a string. name of the bin/metabolite to normalise against



NMRMetab_norm_scale <- function(data, index_col = 3, normalisation = "None", bin = NA, scaling = "None", writeToFile = F) {

  # separate data from groups
  data_ <- as.matrix(data[, index_col:ncol(data)])
  grp <- data[, 1:index_col - 1]

  # apply normalisation
  if (normalisation == "None") {
    # cat('Normalisation: None\n')
  } else if (normalisation == "PQN") {
    data_ <- PQN(data_)
    # cat('Normalisation: PQN\n')
  } else if (normalisation == "TotArea") {
    data_ <- TotArea(data_)
    # cat('Normalisation: Total area\n')
  } else if (normalisation == "Bin") {
    if (!is.na(bin)) {
      data_ <- NormByBin(data_, bin)
      # cat('Normalisation: Bin\n')
      # cat(sprintf('Selected bin: %s \n', as.character(bin)))
    } else {
      # print(paste('Method does not exist: ', normalisation, sep=''))
    }
  }

  # apply scaling
  if (scaling == "None") {
    # cat('Scaling: None\n')
  } else if (scaling == "Auto") {
    data_ <- apply(data_, 2, AutoScale)
    # cat('Scaling: Auto\n')
  } else if (scaling == "Pareto") {
    data_ <- apply(data_, 2, ParetoScale)
    # cat('Scaling: Pareto\n')
  } else if (scaling == "Range") {
    data_ <- apply(data_, 2, RangeScale)
    # cat('Scaling: Range\n')
  } else if (scaling == "Mean") {
    data_ <- scale(data_, center = T, scale = F)
    # cat('Scaling: Mean\n')
  } else {
    # cat(sprintf('Method does not exist: %s \n', scaling))
  }

  # dataLabs = data[,1:index_col-1]
  out_data <- cbind(grp, data_)

  if (writeToFile) {
    # make output folder and write the data to file
    outDir <- makeTSFolder("PLSDA")
    outPath <- file.path(outDir, "NormScale_data.csv")
    write.csv(out_data, outPath, row.names = F)
    print(paste("Data written to ", outPath, sep = ""))
  }

  return(out_data)
}

#' @param x a vector
#' @noRd

AutoScale <- function(x) {
  (x - mean(x)) / sd(x, na.rm = T)
}


#' @param x a vector
#' @noRd

ParetoScale <- function(x) {
  (x - mean(x)) / sqrt(sd(x, na.rm = T))
}


#' @param x a vector
#' @noRd

RangeScale <- function(x) {
  if (max(x) == min(x)) {
    x
  } else {
    (x - mean(x)) / (max(x) - min(x))
  }
}




#' @param data a df
#' @param loc 'median' or 'mean'
#' @noRd


PQN <- function(data, loc = "median") {
  if (loc == "mean") {
    locFunc <- mean
  } else if (loc == "median") {
    locFunc <- median
  } else {
    cat(sprintf("non such location metric %d", loc))
  }

  # if(ncol(data)>nrow(data)) data <- t(data)
  # data = abs(data)
  data_ <- t(data)
  reference <- apply(data_, 1, locFunc)
  # sometimes reference produces 0s so we turn them into 1s before division
  # so spectrum stays unchanged
  reference[reference == 0] <- 1

  quotient <- data_ / reference
  quotient.withLocFunc <- apply(quotient, 2, locFunc)

  pqn.data <- t(data_) / quotient.withLocFunc
  pqn.data
}


#' @param data a df
#' @noRd

TotArea <- function(data) {
  data_ <- t(data)
  meanInt <- sum(apply(data_, 1, mean))
  scalingFactor <- apply(data_, 2, sum) / meanInt
  data_ <- t(t(data_) / scalingFactor)
  t(data_)
}


#' @param data string name
#' @param bin string name
#' @noRd

NormByBin <- function(data, bin) {
  data_ <- t(data)
  refPeakInt <- data[, bin]
  # adjust the integral for mean peak to preserve scale of spectra in the dataset
  refPeaksAdj <- refPeakInt / mean(refPeakInt)
  data_ <- t(t(data_) / refPeaksAdj)
  t(data_)
}


#' @param prefix string name
#' @noRd

makeTSFolder <- function(prefix) {
  ts <- format(Sys.Date(), "%b_%d_%Y")
  ts <- gsub(":", "-", ts)
  tsDir <- paste(prefix, ts, sep = "_")
  if (!file.exists(file.path(getwd(), tsDir))) dir.create(file.path(getwd(), tsDir))
  return(file.path(getwd(), tsDir))
}





# normalisation RAW DATA --------------------------------------------------

#' @title Normalisation and Scaling raw
#' @name NMRMetab_norm_scale_RAW
#' @export
#' @author Eva Caamano Gutierrez
#' @author Arturas Grauslys
#' @param data a data.frame. Column as variable and rows as sample
#' @param index_col integer value. The column number with first bin/matabolite
#' @param normalisation string. one of 'None'#default, 'PQN', 'TotArea', 'Bin'
#' @param scaling string. one of 'None'#default, 'Auto', 'Pareto', 'Range' or 'Mean'
#' @param writeToFile boolean. write resulting data.frame to csv file. F #
#' @param bin a string. name of the bin/metabolite to normalise against


NMRMetab_norm_scale_RAW <- function(data, normalisation = "None", bin = NA, scaling = "None", writeToFile = F) {

  # --- Parse the command line arguments ---


  data_ <- as.matrix(data)

  # --- Normalise the data ---


  # apply normalisation
  if (normalisation == "None") {
    cat("Normalisation: None\n")
  } else if (normalisation == "PQN") {
    data_ <- PQN_raw(data_)
    cat("Normalisation: PQN\n")
  } else if (normalisation == "TotArea") {
    data_ <- totInt_raw(data_)
    cat("Normalisation: Total area\n")
  } else if (normalisation == "Bin") {
    if (!is.na(bin)) {
      data_ <- refPeak_raw(data_, bin)
      cat("Normalisation: Bin\n")
      cat(sprintf("Selected bin: %s \n", as.character(bin)))
    } else {
      print(paste("Method does not exist: ", normalisation, sep = ""))
    }
  }

  # apply scaling
  if (scaling == "None") {
    cat("Scaling: None\n")
  } else if (scaling == "Auto") {
    data_ <- apply(data_, 2, AutoScale)
    cat("Scaling: Auto\n")
  } else if (scaling == "Pareto") {
    data_ <- apply(data_, 2, ParetoScale)
    cat("Scaling: Pareto\n")
  } else if (scaling == "Range") {
    data_ <- apply(data_, 2, RangeScale)
    cat("Scaling: Range\n")
  } else if (scaling == "Mean") {
    data_ <- scale(data_, center = T, scale = F)
    cat("Scaling: Mean\n")
  } else {
    cat(sprintf("Method does not exist: %s \n", scaling))
  }


  # dataLabs = data[,1:index_col-1]
  out_data <- as.data.frame(data_) %>% dplyr::rename(ppm = V1)


  if (writeToFile) {
    # make output folder and write the data to file
    outDir <- makeTSFolder("PLSDA")
    outPath <- file.path(outDir, "NormScale_data.csv")
    write.csv(out_data, outPath, row.names = F)
    print(paste("Data written to ", outPath, sep = ""))
  }

  return(out_data)
}


#' @param data a df
#' @param loc 'median' or 'mean'
#' @noRd
# --- Normalisation functions ---
PQN_raw <- function(data, loc = "median") {
  if (loc == "mean") {
    locFunc <- mean
  } else if (loc == "median") {
    locFunc <- median
  } else {
    cat(sprintf("non such location metric %d", loc))
  }

  data_ <- data[, 2:ncol(data)]
  reference <- apply(data_, 1, locFunc)
  # sometimes reference produces 0s so we turn them into 1s before division
  # so spectrum stays unchanged
  reference[reference == 0] <- 1

  quotient <- data_ / reference
  quotient.withLocFunc <- apply(quotient, 2, locFunc)

  pqn.data <- t(t(data_) / quotient.withLocFunc)
  cbind(data[, 1], pqn.data)
}


#' @param data a df
#' @noRd
# normalisation to total integral
totInt_raw <- function(data) {
  data_ <- data[, 2:ncol(data)]

  meanInt <- trapezoid(apply(abs(data_), 1, mean))
  scalingFactor <- meanInt / apply(abs(data_), 2, trapezoid)
  data_ <- t(t(data_) * scalingFactor)
  data_ <- cbind(data[, 1], data_)
  # colnames(data_) = names(data)
  data_
}


#' @param data a df
#' @param par a df
#' @noRd

# normalisation to reference peak
refPeak_raw <- function(data, par) {
  data_ <- data[, 2:ncol(data)]
  bin <- as.numeric(strsplit(par, ":")[[1]])
  refPeakInt <- apply(data_[data[, 1] >= min(bin) & data[, 1] <= max(bin), ], 2, trapezoid)
  # adjust the integral for mean peak to preserve scale of spectra in the dataset
  refPeaksAdj <- refPeakInt / mean(refPeakInt)
  data_ <- t(t(data_) / refPeaksAdj)
  data_ <- cbind(data[, 1], data_)
  # colnames(data_) = names(data)
  data_
}
