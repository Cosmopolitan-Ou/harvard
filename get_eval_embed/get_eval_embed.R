
# Parameter Setting
################################################################################

# Data, Dimension & Metric
in_file <- "rpdr_code_cooccurrence_victor_2019.csv"  # Data File to Obtain Embedding
dims <- seq(100, 4000, 100)                          # Dimension Setting
plot_val <- "auc"                                    # Metric for Evaluation (Columns in Summary.Rds Dataframes)


# Other Parameters
HAM_file <- "MultiAxialHierarchy.csv"                # Necessary
ARPWN_file <- "AllRelationPairsWithNull.Rdata"       # Necessary
func_file <- "get_eval_embed_funcs.R"                # Utility Functions
summary_rmd_file <- "summary.Rmd"                    # Summary Rmd file
data_type <- 1                                       # Input data type setting, codi only:1, codi & CUI: 2 
split_patterns <- list("Similarity" = "(sim)", 
                       "Relation" = "(rela)")        # patterns to split pairs into groups

################################################################################


# Get Summary
################################################################################

# Source Function
source(func_file)

# Load Data
cat("Loading data...")
load(ARPWN_file)
CO <- readr::read_csv(in_file)
MAH <- readr::read_csv(HAM_file)

# Change CO Column Names
colnames(CO) <- c("V1", "V2", "V3")

# Align Codes
cat("Aligning codes...")
CO <- CO %>% dplyr::mutate(across(c("V1", "V2"), stringr::str_replace, "-PCS", ""))  

# Get Time
start_t <- Sys.time()

# Run
cat("\n-------------------------------------------------------------------------\n")
cat("\n")
cat(paste0("Dimensions Setting:\n", paste(dims, collapse=", ")))
cat(paste0("\n\nTotal Time Cost Estimation: ", round(length(dims) * 30 / 60, 2), " Hours\n"))
n_dims <- length(dims)
summary <- lapply(1:n_dims, function(i) {
  
  dim <- dims[i]
  cat("-----------------------------------------------------------------------\n")
  cat(paste0("[", i, "/", n_dims, "] ", "Calculating for Dimension: ", dim))
  
  # Get Time Per Dim
  start_sub_t <- Sys.time()
  
  # Obtain SPPMI from cooc
  #########################################################################
  # Get Unique Values for First two Columns of CO
  CO_unique <- unique(c(CO$V1, CO$V2))
  
  # Obtain The Roll Up Dictionary For LOINC Codes:
  cat("\nGetting rollup dict...")
  code_LPcode = get_rollup_dict(CO_unique, MAH)
  
  # Calculate SPPMI
  cat("\nCalculating SPPMI...")
  SPPMI = getSPPMI(CO, data.frame(feature_id = CO_unique), code_LPcode)
  
  # Get Embedding
  cat("\nGetting embedding...")
  embed = getembedding(SPPMI, dim)
  #########################################################################
  
  
  # Embed Generation Evaluation
  #########################################################################
  # Evaluate Embedding
  cat("\nEvaluating...")
  if (data_type == 1) {
    ans = Evaluate_tmp(embed, AllRelationPairs)
  } else if (data_type == 2) {
    ans = Evaluate(embed, AllRelationPairs)
  } else stop("Invalid Value: 'data_type' should be 1 or 2.")
  #########################################################################
  
  
  # Get Time Cost Per Dim
  end_sub_t <- Sys.time()
  time_cost <- round(as.numeric(difftime(end_sub_t, start_sub_t, units = "mins")), 2)
  cat(paste0("\nTime cost: ", time_cost, " mins."))
  cat("\n-----------------------------------------------------------------------\n")

  return(ans)
})

# Get Time Cost
end_t <- Sys.time()
time_cost <- round(as.numeric(difftime(end_t, start_t, units = "mins")), 2)
cat(paste0("\nFinished - Total Time cost: ", time_cost, " mins."))
cat("\n-------------------------------------------------------------------------\n")


# Save Summary Data
dim_str <- strsplit(paste(dims, collapse = ","), ",")[[1]]
names(summary) <- dim_str
save(summary, file = paste0("summary-", dims[1], "-", dims[length(dims)], 
                            "-", dims[2]-dims[1],'.Rdata'))

################################################################################


# Get Summary HTML File
################################################################################

# load summary.Rds if saved
# load("summary.Rdata")

output_file <- paste0("summary-", dims[1], "-", dims[length(dims)], 
                    "-", dims[2]-dims[1],'.html')
rmarkdown::render(summary_rmd_file, output_file = output_file)

################################################################################
