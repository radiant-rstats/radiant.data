## Resubmission

This is a resubmission. In this version I have fixed a bug that caused problems for users on Windows with a space in their username. See NEWS.md for details.

## Test environments

* macOS, R 4.3.1
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.data.

# Previous cran-comments

## Resubmission

This is a resubmission. In this update I fixed a strange issue related the patchwork package. At least it was strange to me. In the code below `any` should not be needed. However, it seems that a patchwork object can have length == 1 and still have is.na return a vector of length > 1. Perhaps there are other libraries that have objects like this but I have never seen this before.

My apologies for submitting a new version so soon after the previous version. 

```r
length(x) == 0 || (length(x) == 1 && any(is.na(x)))
```

## Test environments

* macOS, R 4.3.1
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.data.


## Resubmission

This is a resubmission. In this update I have added features and removed a bug. See NEWS.md. 

## Test environments

* macOS, R 4.3.1
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.data.

## Resubmission

This is a resubmission. In this update I have added features and cleaned up code to avoid issues with markdown deprecation warnings. See NEWS.md. 

## Test environments

* macOS, R 4.2.2
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.data.

#

## Resubmission

This is a resubmission. In this update I have added features and cleaned up code to avoid issues with ggplot deprecation warnings. See NEWS.md. Also, URLs have been updated from Rstudio to Posit and the Radiant Documentation site is now back online and accessible.

I have also tried to address the build issue connected to calibre. See note below.

"This suggests you open a web browser in non interactive mode. Please use
such calls only conditionally via

if(interactive())

Please fix and resubmit.

Best,
Uwe Ligges
"

## Test environments

* macOS, R 4.2.2
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.data.


## Resubmission

This is a resubmission. In this update I have addressed three issues. See NEWS.md.

## Test environments

* macOS, R 4.2.1
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.data.


## Resubmission

This is a resubmission. In this version I fixed a dependency issue that is essential for correct functioning of the radiant.data shiny application. This feature is difficult to evaluate with automated testing and unfortunately I made a mistake in the submission earlier today. I uncovered the issue after upgrading to R 4.2.1. My apologies.

## Test environments

* macOS, R 4.2.1
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.data.


## Resubmission

This is a resubmission. In this version I have fixed a bug and added features (see NEWS.md for details).

## Test environments

* macOS, R 4.2.0
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.data.


## Resubmission

This is a resubmission. In this version I have fixed bugs and updated documentation (see NEWS.md for details). I also fixed an link issue in the documentation for sshh and sshhr

## Test environments

* macOS, R 4.2.0
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.data.


## Resubmission

This is a resubmission. In this version I have fixed several bugs and added several new features (see NEWS.md for details).

## Test environments

* local Ubuntu 20.04, R 4.1.0
* local Ubuntu 20.04 through WSL2, R 4.0.5
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.data.

## Resubmission

This is a resubmission. In this version I have address a problem linked to issue: https://github.com/yihui/knitr/issues/1864 There are also a number of changes that allow users to change the aesthetics of the app using `bslib` if available.

## Test environments

* local Ubuntu 20.04, R 4.1.0
* local Ubuntu 20.04 through WSL2, R 4.0.5
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.data.

# Previous cran-comments


## Resubmission

This is a resubmission. In this version I have fixed issues related to updates in the `magrittr` and `readr` packages. I  

## Test environments

* Ubuntu 20.04, R 4.0.3
* win-builder (devel)
* ubuntu "bionic" (on travis-ci), R release and devel

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.data.


## Resubmission

This is a resubmission. In this version I have added back a feature that is now supported in dplyr 1.0.1 and made it easier to connect to Google Drive from the file-browser. I also, updated links that CRAN's automated checking listed. 

## Test environments

* local OS X install, R 4.0.2
* local Windows install, R 4.0.2
* win-builder (devel)
* ubuntu "bionic" (on travis-ci), R release and devel

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.data.


## Resubmission

This is a resubmission. In this version I have fixed a bug and removed a feature that no-longer works with dplyr 1.0.0 (see NEWS.md for details). Also, update the link to the ggplot2 documentation

## Test environments

* local OS X install, R 4.0.1
* local Windows install, R 4.0.0
* win-builder

## R CMD check results

There were no ERRORs or WARNINGs. There is one NOTE about the number of imported non-default packages. 

## Resubmission

This is a resubmission. In this version I have fixed several bugs and added several new features (see NEWS.md for details).

## Test environments

* local OS X install, R 3.6.3
* local Windows install, R 3.6.2
* ubuntu "trusty" (on travis-ci), R release and devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 


## Resubmission

This is a resubmission. In this version I have fixed several bugs and added several new features (see NEWS.md for details).

## Test environments

* local OS X install, R 3.6.1
* local Windows install, R 3.6.1
* ubuntu "trusty" (on travis-ci), R release and devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 

## Resubmission

This is a resubmission. In this version I have fixed several bugs and added several new features (see NEWS.md for details).

## Test environments

* local OS X install, R 3.6.1
* local Windows install, R 3.6.1
* ubuntu "trusty" (on travis-ci), R release and devel
* win-builder
* rhub

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 

# Previous cran-comments

## Resubmission

This is a resubmission. In this version I have fixed several bugs and added several new features (see NEWS.md for details).

## Test environments

* local OS X install, R 3.5.2
* local Windows install, R 3.5.2
* ubuntu "trusty" (on travis-ci), R release and devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 
