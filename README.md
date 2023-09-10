
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

**Install from CRAN**

``` r
install.packages("nhanesA")
```

**Install from the dev repo**

``` r
install.packages("devtools")
install_github("cjendres1/nhanes")
```


**Use nhanesA in Docker**

The Docker container hosts the data, allowing for faster access and manipulation directly from the local Docker environment.

Start Docker on Mac or Linux

```dockerfile
docker run  --rm --name nhanes-workbench \
        -v <YOUR LOCAL PATH>:/mnt/ \
        -d \
        -p 8787:8787 \
        -p 2200:22 \
        -p 1433:1433 \
        -e 'CONTAINER_USER_USERNAME=nhanes' \
        -e 'CONTAINER_USER_PASSWORD=nhanes' \
        -e 'ACCEPT_EULA=Y' \
        -e 'SA_PASSWORD=yourStrong(!)Password' \
         hmsccb/nhanes-workbench:latest
```

Start Docker on Windows

```dockerfile
docker run  --rm --name nhanes-workbench -d  -v <YOUR LOCAL PATH>:/mnt/ -p 8787:8787 -p 2200:22 -p 1433:1433  -e 'CONTAINER_USER_USERNAME=nhanes'  -e 'CONTAINER_USER_PASSWORD=nhanes' -e 'ACCEPT_EULA=Y' -e 'SA_PASSWORD=yourStrong(!)Password' hmsccb/nhanes-workbench:latest
```
Use the Rstudio via: http://localhost:8787/
More details about the [NHANES Docker](https://github.com/ccb-hms/NHANES)

<br/>

**Working with nhanesA**

[Manual](https://cran.r-project.org/package=nhanesA/nhanesA.pdf)

[Vignette: Introducing
nhanesA](https://cran.r-project.org/package=nhanesA/vignettes/Introducing_nhanesA.html)

<br />
<img src="man/figures/nhanesAsticker.png" alt="drawing" width="400"/>
