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


#' @title packing_Makefile
#' @description ...
#' @param git_install ...
#' @return ...
#' @examples
#' ...
#' @export
packing_Makefile = function (git_install) {
    paste0("all: doc
.PHONY: doc install check

doc:
	R -e 'devtools::document()'

install:
	R -e \"", git_install,"\"

check:
	R -e 'devtools::check()'
")
} 
