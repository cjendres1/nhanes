
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nhanesA

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/nhanesA)](https://cran.r-project.org/package=nhanesA)
[![LICENSE](https://img.shields.io/cran/l/nhanesA)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/nhanesA)](https://cran.r-project.org/package=nhanesA)
<!-- badges: end -->

nhanesA is an R package for browsing and retrieving data from the
National Health And Nutrition Examination Survey (NHANES). This package
is designed to be useful for research and instructional purposes.

The functions in the nhanesA package allow for fully customizable
selection and import of data directly from the [NHANES
website](https://www.cdc.gov/nchs/nhanes/) thus it is essential to have
an active network connection.

## Install from CRAN

``` r
install.packages("nhanesA")
```

### Install from the development repository

``` r
install.packages("devtools")
devtools::install_github("cjendres1/nhanes")
```

## Use nhanesA in Docker

It is also possible to use the nhanesA package in conjunction with a
suitably configured SQL database which contains a snapshot of the data
publicly available at the NHANES website. This eliminates the time spent
in downloading data, enabling faster operations, at the cost of
pre-downloading all the data locally. The simplest way to do this is by
running a docker image [as described
here](https://github.com/deepayan/nhanes-postgres/).

The Docker container includes the data in a SQL database, allowing for
faster access and manipulation directly from the local Docker
environment. The summary of the differences between using the standard
nhanesA and using it inside Docker is as follows:

**Standard nhanesA:**

- When used outside of Docker, the nhanesA functions scrape data
  directly from the CDC website each time they are invoked.

- The advantage is simplicity; users only need to install the nhanesA
  package without any additional setup.

- However, the response time is contingent upon internet speed and the
  size of the requested data.

**Docker-enhanced nhanesA:**

- The Docker container locally hosts most of the NHANES data, allowing
  for significantly faster data access and manipulation.

- Initial setup requires Docker installation and downloading the Docker
  image.

- Some data, such as the youth survey, are not present in the Docker
  database and would need to be fetched from the CDC website.

In essence, the Docker-enhanced version offers fast access to a majority
of the data, and will fetch data in the standard nhanesA manner for
datasets not present in its database.

To use the Docker-enhanced version of nhanesA, one must already have
Docker installed, and then run a command similar to the following.

``` sh
docker run --rm -d -p 8787:8787 -e 'CONTAINER_USER_USERNAME=<USER>' -e 'CONTAINER_USER_PASSWORD=<PASSWORD>' deepayansarkar/nhanes-postgresql:<VERSION>
```

This command will download the (fairly large) docker image the first
time it is run. More details such as the latest version and other useful
options can be found
[here](https://github.com/deepayan/nhanes-postgres/blob/main/README.md).
Once the command runs successfully, one can log into RStudio Server
running inside the docker container *via* <http://localhost:8787> using
the username and password set in the command above. The nhanesA package
is already installed, and configured to use the database.

## Working with nhanesA

- [Manual](https://cran.r-project.org/package=nhanesA/nhanesA.pdf)

- [Vignette: Introducing
  nhanesA](https://cran.r-project.org/package=nhanesA/vignettes/Introducing_nhanesA.html)

<br />
<img src="man/figures/nhanesAsticker.png" alt="drawing" width="400"/>
