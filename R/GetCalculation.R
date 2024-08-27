#' Perform Calculations
#'
#' @description This function takes cleaned raw data and performs various analyses,
#' including calculating the time to threshold, Rate of Amyloid Formation (RAF), Max Point Ratio (MPR), 
#' Max Slope (MS), and whether the reaction crosses the threshold (XTH).
#'
#' @param raw Cleaned raw data matrix. Output from `CleanRaw()`.
#' @param meta Cleaned meta data. Output from `CleanMeta()`.
#' @param norm Logical. If TRUE, normalization will be performed. Default is FALSE. 
#' @param norm_ct Sample name used to normalize calculation. 
#' @param threshold_method Method for calculating threshold ('stdv', 'rfu_val', or 'bg_ratio'). 
#' @param time_skip Number of initial time points to skip when checking for threshold crossing.
#'        This helps ignore early crossings that may be due to reasons unrelated to seeding activity.
#' @param sd_fold Fold of standard deviation to calculate the threshold for RAF (for 'stdv' method).
#' @param bg_fold Background fold for threshold calculation (for 'bg_ratio' method).
#' @param rfu Relative fluorescence unit values used for threshold (for 'rfu_val' method).
#' @param cycle_background The cycle number chosen as the background for RAF and MPR calculations.
#' @param binw Bin width for the MS calculation.
#' @return A data frame containing the results of the calculation. 
#' 
#' @references 
#' Henderson DM, Davenport KA, Haley NJ, Denkers ND, Mathiason CK, Hoover EA. 
#' Quantitative assessment of prion infectivity in tissues and body fluids by real-time quaking-induced conversion.
#' J Gen Virol. 2015;96(Pt 1):210-219. doi:10.1099/vir.0.069906-0
#' 
#' Li M, Schwabenlander MD, Rowden GR, Schefers JM, Jennelle CS, Carstensen M, Seelig D, Larsen PA. RT-QuIC detection 
#' of CWD prion seeding activity in white-tailed deer muscle tissues. Sci Rep. 2021;11(1):16759. 
#' doi: 10.1038/s41598-021-96127-8. PMID: 34408204; PMCID: PMC8373970.
#' 
#' Rowden GR, Picasso-Risso C, Li M, Schwabenlander MD, Wolf TM, Larsen PA. 
#' Standardization of Data Analysis for RT-QuIC-Based Detection of Chronic Wasting Disease. Pathogens. 
#' 2023;12(2):309. doi:10.3390/pathogens12020309
#' 
#' Haley NJ, Van de Motter A, Carver S, et al. Prion-seeding activity in cerebrospinal fluid of deer with chronic 
#' wasting disease. PLoS One. 2013;8(11):e81488. doi:10.1371/journal.pone.0081488
#' 
#' @examples
#' # Define the path to the plate data file
#' plate_path <- system.file("extdata/20240716_p3", 
#'                           file = '20240716_p3_plate.xlsx', 
#'                           package = "QuICSeedR")
#'   
#' # Read the plate data
#' plate <- readxl::read_xlsx(plate_path)
#' 
#' # Define the path to the raw data file
#' raw_path <- system.file("extdata/20240716_p3", 
#'                         file = '20240716_p3_raw.xlsx', 
#'                         package = "QuICSeedR")
#' # Read the raw data
#' raw <- readxl::read_xlsx(raw_path)
#' 
#' # Get replicate data
#' replicate <- GetReplicate(plate)
#' 
#' # Ensure time displayed as decimal hours
#' plate_time = ConvertTime(raw)
#' 
#' #Get metadata and display the few rows 
#' meta = CleanMeta(raw, plate, replicate)
#' 
#' #Clean data 
#' cleanraw <- CleanRaw(meta, raw, plate_time)
#' 
#' #Get calculations using positive controls to normalize values. 
#' calculation = GetCalculation(raw = cleanraw, meta, norm = TRUE, norm_ct = 'Pos')
#' 
#' head(calculation)
#' 
#' @importFrom stats sd
#' @importFrom magrittr %>%
#' @export

GetCalculation = function (raw, meta, norm = FALSE, norm_ct, threshold_method = "stdv", 
                           time_skip = 5, sd_fold = 3, bg_fold = 3, rfu = 5000, cycle_background = 4, 
                           binw = 6) 
{
  if (!threshold_method %in% c("stdv", "bg_ratio","rfu_val")) 
    stop("Invalid threshold_method. Use 'stdv', 'bg_ratio', or 'rfu_val'.")
  if (cycle_background > nrow(raw)) 
    stop("cycle_background exceeds number of rows in raw data")
  if (norm && is.null(norm_ct)) 
    stop("norm_ct must be provided when norm is TRUE")
  calculate_mpr <- function(raw, background) {
    apply(raw, 2, max)/background
  }
  calculate_threshold <- function(nv, method, sd_fold, bg_fold, rfu) {
    if (method == "stdv") {
      mean(nv) + sd_fold * sd(nv)
    }
    else if (method == "bg_ratio") {
      nv * bg_fold
    } 
    else {
      rfu
    }
  }
  calculate_raf <- function(raw, threshold, time_skip) {
    time_to_threshold <- apply(raw, 2, function(column) {
      crossing_row <- which(column[-(1:time_skip)] > threshold)[1]
      if (is.na(crossing_row)) 
        NA
      else as.numeric(rownames(raw)[crossing_row + time_skip])
    })
    raf <- 1/time_to_threshold
    raf[is.infinite(raf) | is.na(raf)] <- 0
    list(time_to_threshold = time_to_threshold, raf = raf)
  }
  calculate_ms <- function(raw, binw) {
    n <- nrow(raw)
    smoothed_slope <- apply(raw, 2, function(col) {
      sapply(1:(n - binw), function(i) {
        (col[i + binw] - col[i])/binw
      })
    })
    apply(smoothed_slope, 2, max, na.rm = TRUE)
  }
  background <- raw[cycle_background, ]
  nv <- as.numeric(background)
  threshold <- calculate_threshold(nv, threshold_method, sd_fold, bg_fold, rfu)
  mpr <- calculate_mpr(raw, background)
  raf_results <- calculate_raf(raw, threshold, time_skip)
  ms <- calculate_ms(raw, binw)
  calculation <- data.frame(time_to_threshold = raf_results$time_to_threshold, 
                            RAF = raf_results$raf, MPR = mpr, MS = ms)
  if (norm) {
    sel <- which(meta$content == norm_ct) %>% as.numeric()
    data_pos <- calculation[sel, ]
    terms <- c("time_to_threshold", "RAF", "MPR", "MS")
    data_norm <- data.frame(matrix(nrow = nrow(calculation), 
                                   ncol = length(terms)))
    for (i in 1:length(terms)) {
      ave_terms <- mean(data_pos[, terms[i]])
      data_norm[, i] <- calculation[, terms[i]]/ave_terms
    }
    colnames(data_norm) <- terms
    calculation <- cbind(meta, data_norm)
  }
  else {
    calculation <- cbind(meta, calculation)
  }
  calculation$XTH = ifelse(!is.na(calculation$time_to_threshold) & 
                             calculation$time_to_threshold > 0, 1, 0)
  return(calculation)
}