######################
### Initial set-up ###
######################

source("R/load-packages.R") # Load required packages
#source("R/define-parameters.R") # Define fixed parameters, if used
sourceDirectory('R')

targets <- NULL
verbose <- 2
use_parallel <- TRUE
max_cores <- Inf

#############################
### Futures (if required) ###
#############################

if (Sys.getenv("RSTUDIO") != "1" && use_parallel) {
	n_cores <- min(availableCores() - 1, max_cores)
  plan(multiprocess(workers = n_cores))
	jobs <- n_cores
	parallelism <- 'future'
} else {
	jobs <- 1
	parallelism <- 'loop'
}

###################
### Define plan ###
###################

plan <- drake_plan(
# 	data = read_raw_data(),
# 	clean = process_data(),
# 	results = analyse_data(),
# 	output = render_manuscript(knitr_in(!!file_output_rmd), file_out(!!file_output_docx),
# 														 file_in(!!file_word_styles), file_in(!!file_csl), file_in(!!file_bib))
)

##################################
### Create drake_config object ###
##################################

drake_config(
	plan, parallelism = parallelism, jobs = jobs,
	targets = targets,
	verbose = verbose
)
