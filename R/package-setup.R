# 2023-12-07 Bull shark project
# Simon Dedman, simondedman@gmail.com
# Copying Sarah Popov's package approach: https://popovs.github.io/r-pkg-slides
library(devtools)
library(usethis)
# Create package folder and files
usethis::create_package("~/Documents/Si Work/PostDoc Work/FIU/2023-10 Bull Shark Acoustic Telemetry/Code/FIUbullshark")
# Create a script file for functions (your first/main script). Name it relevant to what you're doing.
usethis::use_r("telemetry")
# Document it
devtools::document()
# edit the DESCRIPTION file manually, then
usethis::use_package_doc()
devtools::document() # gets stuck in a loop asking to Overwrite pre-existing file 'DESCRIPTION'?
# (which we don't want) and Overwrite pre-existing file 'FIUbullshark.Rproj' which opens a new RStudio instance,
# but the original instance wants to overwrite description again.
# Rd for package therefore not created.

# In function script: Code > Insert Roxygen skeleton
# (needs a function to be present to work)
devtools::document()

# Add a README
usethis::use_readme_md()

# Add walkthrough vignettes
usethis::use_vignette("bullshark_telemetry")
devtools::document()

# add cited packages to Imports automatically
usethis::use_package("pkgname")

# add data
usethis::use_data_raw()
# put csv files etc in /data-raw/
# manually edit DATASET.R
# creates a data/ folder with rda file
# can be loaded with data("MYDATA")

# Create directory structure for {testhat}
usethis::use_testthat()
# Create a test file
usethis::use_test("telemetry")
# creates test-telemetry.R

# Add a test to test-telemetry.R
# run tests
devtools::test()














