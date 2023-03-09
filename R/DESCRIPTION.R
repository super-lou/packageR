# Copyright 2023 Louis Héraut(louis.heraut@inrae.fr)*1
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


#' @title DESCRIPTION
#' @description ...
#' @param name ...
#' @param date ...
#' @param description ...
#' @param author ...
#' @param email ...
#' @param imports ... (default : c("a", "bb"))
#' @return ...
#' @examples
#' ...
#' @export
DESCRIPTION = function (name, date, description, author, email, imports=c("a", "bb")) {
    paste0("Package: ", name,"
Version: 0.0.1
Date: ", date, "
Title: ", name, "
Description: ", description, "
Authors@R: c(
    person(given = \"", gsub(" .*$", "", author) , "\",
           family = \"", gsub("^.* ", "", author) , "\",
           role = c(\"aut\"),
           email = \"", email, "\")
    )
Encoding: UTF-8
License: GPL-3 + file LICENSE
Imports:
    ", paste0(imports, collapse=",\n    "))
}
 