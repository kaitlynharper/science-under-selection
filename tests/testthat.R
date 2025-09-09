# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)

# Source your R files to make functions available for testing
source("/Users/kaitlynharper/Documents/Github/academiABM2/R/functions.R")
source("/Users/kaitlynharper/Documents/Github/academiABM2/R/model.R")
source("/Users/kaitlynharper/Documents/Github/academiABM2/R/run_simulation.R")

# Run tests in the testthat directory
test_dir("/Users/kaitlynharper/Documents/Github/academiABM2/tests/testthat")
