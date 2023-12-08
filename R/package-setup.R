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
devtools::document()
# ISSUE ####
# gets stuck in a loop asking to Overwrite pre-existing file 'DESCRIPTION'?
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

# add a usage license
usethis::use_mit_license()

# add a default code of conduct for interacting with your package
usethis::use_code_of_conduct("simondedman@gmail.com")
# You may also want to describe the code of conduct in your README:
  ## Code of Conduct
# Add this manually

# Initiate Git, make first commit
usethis::use_git()
# R restart required

# push to Github
usethis::use_github()
# There are uncommitted changes and we're about to create and push to a new GitHub repo. Do you want to proceed anyway?
# No.
# Git commit, write commit message. Can't push.
usethis::use_github()
# Error: Unable to discover a GitHub personal access token
# A token is required in order to create and push to a new repo
usethis::create_github_token()
# copy token
# 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
gitcreds::gitcreds_set() # paste token here
usethis::use_github()

# anyone can access
# install.packages("remotes")
# remotes::install_github("SimonDedman/FIUbullshark")

# Publicise
# install.packages("pkgdown")
library(pkgdown)

# Generate website files in docs/
pkgdown::build_site()
# Error: in callr subprocess.
# Caused by error in `library(pkg$package, character.only = TRUE)`: there is no package called ‘FIUbullshark’
# Likely related to issue above.

# push to github: git commit, push:
# prompts to enter uname & pw. my gmail & the token above.
# remote: Invalid username or password.
# fatal: Authentication failed for 'https://github.com/SimonDedman/FIUbullshark.git/'

# tried old/standard passowrd, no longer allowed.

# go to repo settings and tell GitHub to update the website from the docs/ subdirectory
# every time you push changes to the master branch
# https://github.com/SimonDedman/FIUbullshark/settings/pages
# branch = master. next button select /docs. save.

# push changes to git
# git commit, select everything, add a commit message.
# git push.

# check the status of your website deployment
# https://github.com/SimonDedman/FIUbullshark/actions
# check this is right ####
