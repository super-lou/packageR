# Copyright 2023 Louis Héraut (louis.heraut@inrae.fr)*1
#                     
# *1   INRAE, France
#
# This file is part of packageR R package.
#
# packageR R package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# packageR R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with packageR R package.
# If not, see <https://www.gnu.org/licenses/>.


#' @title packing_README.md
#' @description ...
#' @param name ...
#' @param figure_path ...
#' @param lifecycle ...
#' @param git_install ...
#' @param isCovenant ...
#' @param description ...
#' @param documentation ...
#' @return ...
#' @examples
#' ...
#' @export
packing_README.md = function (name, figure_path, lifecycle, git_install, isCovenant, description, documentation) {

    if (lifecycle == "Experimental") {
        lifecycle_badge = "[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](<Redirect-URL>)"
    } else if (lifecycle == "Maturing") {
        lifecycle_badge =                                      "[![Lifecycle:Maturing](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](<Redirect-URL>)"
    } else if (lifecycle == "Stable") {
        lifecycle_badge ="[![Lifecycle:Stable](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](<Redirect-URL>)"
    } else if (lifecycle == "Dormant") {
        lifecycle_badge ="[![Lifecycle:Dormant](https://img.shields.io/badge/Lifecycle-Dormant-ff7f2a)](<Redirect-URL>)"
    } else if (lifecycle == "Retired") {
        lifecycle_badge = "[![Lifecycle:Retired](https://img.shields.io/badge/Lifecycle-Retired-d45500)](<Redirect-URL>)"
    } else {
        lifecycle_badge = ""
    }

    if (isCovenant) {
        Covenant_badge = "[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)"
        Covenant_text = "## Code of Conduct
Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms."
    } else {
        Covenant_badge = ""
        Covenant_text = ""
    }

    paste0(
        "# ", name," [<img src=\"", figure_path, "\" align=\"right\" width=160 height=160 alt=\"\"/>](https://www.inrae.fr/en)

<!-- badges: start -->
", lifecycle_badge, "
![](https://img.shields.io/github/last-commit/", gsub("(.*[(]['])|(['][)])", "", git_install), ")
", Covenant_badge, "
<!-- badges: end -->

", description,"

This project was carried out for National Research Institute for Agriculture, Food and the Environment (Institut National de Recherche pour l’Agriculture, l’Alimentation et l’Environnement, [INRAE](https://agriculture.gouv.fr/inrae-linstitut-national-de-recherche-pour-lagriculture-lalimentation-et-lenvironnement) in french).


## Installation
For latest development version
``` r
", git_install,"
```


## Documentation
", documentation,"



## FAQ
*I have a question.*

-   **Solution**: Search existing issue list and if no one has a similar question create a new issue.

*I found a bug.*

-   **Good Solution**: Search existing issue list and if no one has reported it create a new issue.
-   **Better Solution**: Along with issue submission provide a minimal reproducible example of the bug.
-   **Best Solution**: Fix the issue and submit a pull request. This is the fastest way to get a bug fixed.


", Covenant_text, "
")
} 