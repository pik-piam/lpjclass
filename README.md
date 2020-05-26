# LPJ Class Functions

R package **lpjclass**, version **1.15.0**

  

## Purpose and Functionality

Package containing the LPJ-Object-Class together with relevant functions and methods.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("lpjclass")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **lpjclass** in publications use:

Waha K, Bodirsky B, Roliniski S, Philipp Dietrich J, P. P. Alves M (2020). _lpjclass: LPJ Class Functions_. R package version
1.15.0.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {lpjclass: LPJ Class Functions},
  author = {Katharina Waha and Benjamin Bodirsky and Susanne Roliniski and Jan {Philipp Dietrich} and Marcos {P. P. Alves}},
  year = {2020},
  note = {R package version 1.15.0},
}
```

