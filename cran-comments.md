## Resubmission

This is a resubmission (0.8.1). As suggested by Dr. Ripley after radiant.data 
0.8.0 was accepted onto CRAN, in this version I have moved `feather` to 
`suggests` to avoid the `little-endian-only` issue. I wasn't aware of this issue 
and agree this is a better solution.

Please also note that reverse dependency checks will show  deprecation  warnings 
for `mutate_each` in the `radiant.*` packages. This is related to the 
deprecation of the `*_each` functions in dplyr. I will update `radiant.*` when 
(if) the new version of `radiant.data` is available on CRAN

## Test environments

* local OS X install, R 3.4
* local Windows install, R 3.4
* ubuntu 14.04 (on travis-ci), R 3.3.3 and R-dev
* win-builder (release)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE about a possibly mis-
spelled word (see below). The spelling is correct however.

Possibly mis-spelled words in DESCRIPTION:
  Analytics (2:40)

## Resubmission

This is a resubmission. In this version I have fixed several bugs and added 
several new features (see NEWS.md for details)

## Test environments

* local OS X install, R 3.4
* local Windows install, R 3.4
* ubuntu 14.04 (on travis-ci), R 3.3.3 and R-dev
* win-builder (dev and release)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE about a possibly mis-
spelled word (see below). The spelling is correct however.

Possibly mis-spelled words in DESCRIPTION:
  Analytics (2:40)

## Previous cran-comments

## Test environments

* local OS X install, R 3.3.1
* local Windows install, R 3.3.1
* ubuntu 12.04 (on travis-ci), R 3.3.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE: New submission
