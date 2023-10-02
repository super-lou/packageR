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


#' @title packing
#' @description This function creates an R package from a selection of R files containing functions.
#' @param package_path Path to the directory to be transformed into an R package (default: ".")
#' @param Rfile_pattern Regular expression pattern to select specific files in the package_path directory (default: ".*[.]R$")
#' @param name Name of the package (default: "packageR")
#' @param date Creation date of the package or version (default: Sys.Date())
#' @param description Package description (default: "A simple R package that helps create simple R packages.")
#' @param documentation Simple README documentation explaining how the package works (default: "Navigate to a directory with R files, open an R console, and run `?packageR::packing` to see available arguments. Then, run `packageR::packing(*arguments*)` to create your R package.")
#' @param author Package author (default: "Louis Héraut")
#' @param email Author's email (default: "louis.heraut@inrae.fr")
#' @param imports Required packages for package installation (default: c("roxygen2", "remotes", "devtools", "stringr"))
#' @param git_install R command to install packages from a specified repository (default: "remotes::install_github('super-lou/packageR')")
#' @param figure_path Path to a figure to use in the README
#' @param lifecycle Project lifecycle status (default: "Stable")
#' Lifecycle statuses:
#' - Experimental: The project is in its early stages of development, and the codebase may change frequently.
#' - Maturing: The codebase is being refined, but finer details are likely to change.
#' - Stable: The project is in a reliable state, and major changes are unlikely to occur.
#' - Dormant: The project is not currently under active development, but there are plans for future redevelopment.
#' - Retired: The project is no longer in use and/or supported.
#' @param add_file List of additional files commonly included in packages (default: c("CODE_OF_CONDUCT.md", "DESCRIPTION", "LICENSE", "Makefile", "README.md"))
#' @return A new directory in the working directory formatted as a usable R package, based on the provided input files.
#' 
#' @examples
#' # for default option (which is the option used for create this R package)
#' packageR::packing()
#'
#' # for your personal package
#' packageR::packing(package_path="alice",
#'                   Rfile_pattern=".*[.]R$",
#'                   name="CheshiRCat",
#'                   date="1865-11-01",
#'                   description="Twas brillig, and the slithy toves. Did gyre and gimble in the wabe; All mimsy were the borogoves, And the mome raths outgrabe.",
#'                   documentation="Beware the [*Jabberwock*](https://en.wikipedia.org/wiki/Jabberwocky), my son! The jaws that bite, the claws that catch!</br> Beware the *Jubjub bird*, and shun The frumious *Bandersnatch*!</br> He took his **vorpal sword** in hand:</br></br> - Long time the manxome foe he sought </br> - So rested he by the Tumtum tree, And stood awhile in thought.</br></br> And as in uffish thought he stood `?CheshiRCat::smile`",
#'                   author="Alice",
#'                   email="alice@wonderland.uk",
#'                   imports=c("forcats", "pillar"),
#'                   git_install="remotes::install_github('alice_adventures/CheshiRCat')",
#'                   figure_path=NULL,
#'                   lifecycle="Dormant",
#'                   add_file=c("CODE_OF_CONDUCT.md", "DESCRIPTION", "LICENSE", "Makefile", "README.md"))
#' @export
#' @md
packing = function (package_path=".",
                    Rfile_pattern=".*[.]R$",
                    name="packageR",
                    date=Sys.Date(),
                    description="A simple R package that helps create simple R packages.",
                    documentation="Navigate to a directory with R files, open an R console, and run `?packageR::packing` to see available arguments. Then, run `packageR::packing(*arguments*)` to create your R package.",
                    author="Louis Héraut",
                    email="louis.heraut@inrae.fr",
                    imports=c("roxygen2", "remotes", "devtools", "stringr"),
                    git_install="remotes::install_github('super-lou/packageR')",
                    figure_path=NULL,
                    lifecycle="Stable",
                    add_file=c("CODE_OF_CONDUCT.md", "DESCRIPTION",
                               "LICENSE", "Makefile", "README.md")) {

    package_path_name = file.path(package_path, name)
    
    if (!(dir.exists(package_path_name))) {
        dir.create(package_path_name)
    } else {
        stop (paste0(package_path_name, " directory already exists, choose an other name for the package or delete the existing directory"))
    }

    date = as.Date(date)

    if (is.null(figure_path)) {
        figures = c("https://github.com/super-lou/packageR/raw/main/AE/AE_hex_%2066c1bf.png",
                    "https://github.com/super-lou/packageR/raw/main/AE/AE_hex_000000.png",
                    "https://github.com/super-lou/packageR/raw/main/AE/AE_hex_008c8e.png",
                    "https://github.com/super-lou/packageR/raw/main/AE/AE_hex_275662.png",
                    "https://github.com/super-lou/packageR/raw/main/AE/AE_hex_423089.png",
                    "https://github.com/super-lou/packageR/raw/main/AE/AE_hex_797870.png",
                    "https://github.com/super-lou/packageR/raw/main/AE/AE_hex_9dc544.png",
                    "https://github.com/super-lou/packageR/raw/main/AE/AE_hex_9ed6e3.png",
                    "https://github.com/super-lou/packageR/raw/main/AE/AE_hex_c4c0b3.png",
                    "https://github.com/super-lou/packageR/raw/main/AE/AE_hex_ed6e6c.png",
                    "https://github.com/super-lou/packageR/raw/main/AE/AE_hex_ffffff.png")
        figure_path = figures[sample(1:length(figures), 1)]
    }

    isCovenant = "CODE_OF_CONDUCT.md" %in% add_file

    Rpath = file.path(package_path_name, "R")
    dir.create(Rpath)
    Rfiles = list.files(path=package_path,
                        pattern=Rfile_pattern,
                        recursive=TRUE,
                        full.names=TRUE)

    for (file in Rfiles) {
        Lines = readLines(file)

        Id_function =
            which(
                sapply(Lines, grepl,
                       pattern=
                           "([=][[:space:]]*function[[:space:]]*[(])|([<][-][[:space:]]*function[[:space:]]*[(])"))

        Doc = c()        
        for (i in 1:length(Id_function)) {
            id_function = Id_function[i]
            id_doc = id_function - 1

            if (id_function != 1) {
                if (is.character(Lines[id_doc])) {
                    while (!grepl("[[:graph:]]", Lines[id_doc])) {
                        id_doc = id_doc - 1
                    }
                } else {
                    id_doc = 1
                }
            } else {
                id_doc = 1
            }

            if (id_function == 1 | !grepl("[#][']", Lines[id_doc])) {

                id_end_function = id_function
                while (!grepl("[)][[:space:]]*[{]",
                              Lines[id_end_function])) {
                                  id_end_function = id_end_function + 1
                              }
                
                function_lines = Lines[id_function:id_end_function]
                function_lines = gsub("[[:space:]]+", " ",
                                      paste0(function_lines,
                                             collapse=" "))

                function_name =
                    gsub("([[:space:]]*[=][[:space:]]*function[[:space:]]*[(].*)|([[:space:]]*[<][-][[:space:]]*function[[:space:]]*[(].*)",
                         "", function_lines)

                function_args =
                    gsub("(.*[[:space:]]*[=][[:space:]]*function[[:space:]]*[(])|(.*[[:space:]]*[<][-][[:space:]]*function[[:space:]]*[(])|([)][[:space:]]*[{])",
                         "", function_lines)

                if (nchar(function_args) == 0) {
                    doc = paste0("#' @title ",
                                 function_name,"
#' @description ...
#' @return ...
#' @examples
#' ...
#' @export
#' @md")
                    
                } else {
                    if (grepl("[=]", function_args)) {
                        function_name_args =
                            gsub("([=])|([,])", "", unlist(stringr::str_extract_all(function_args, "(([[:alnum:]])|([_])|([.]))*[[:space:]]*(([=])|([,]))")))
                        function_name_args =
                            function_name_args[function_name_args != ""]
                        function_default_args =
                            unlist(strsplit(function_args,
                                            paste0("(",
                                                   paste0(function_name_args,
                                                          collapse=")|("),
                                                   ")")))
                        function_default_args =
                            function_default_args[function_default_args != ""]

                    } else if (grepl("[,]", function_args)) {
                        function_name_args =
                            unlist(strsplit(function_args,
                                            "[[:space:]]*[,][[:space:]]*"))
                        function_default_args =
                            unlist(strsplit(function_args,
                                            paste0("(",
                                                   paste0(function_name_args,
                                                          collapse=")|("),
                                                   ")")))
                        
                    } else {
                        function_name_args = function_args
                        function_default_args =
                            unlist(strsplit(function_args,
                                            paste0("(",
                                                   paste0(function_name_args,
                                                          collapse=")|("),
                                                   ")")))
                    }

                    function_default_args =
                        gsub("(^[=])|([,][[:space:]]*$)", "",
                             function_default_args)
                    function_default_args =
                        gsub("[[:space:]]+", " ", function_default_args)

                    params = c()
                    for (j in 1:length(function_name_args)) {

                        if (nchar(function_default_args[j]) == 0) {
                            param = paste0("#' @param ",
                                           function_name_args[j],
                                           " ...")
                        } else {
                            param = paste0("#' @param ",
                                           function_name_args[j],
                                           " ... (default : ",
                                           function_default_args[j], ")")
                        }
                        params = c(params, param)                    
                    }
                    doc = paste0("#' @title ",
                                 function_name,"
#' @description ...
", paste0(params, collapse="\n"), "
#' @return ...
#' @examples
#' ...
#' @export
#' @md")
                }
                Doc = c(Doc, doc)
            }
        }
        
        Id_doc = Id_function - 1        
        for (i in 0:(length(Id_doc)-1)) {
            Lines = append(Lines,
                           Doc[i+1],
                           after=(Id_doc[i+1]+i))
        }

        Lines = c(paste0("# Copyright ",
                         format(date, "%Y"), " ",
                         author, " (", email, ")*1
#                     
# *1   INRAE, France
#
# This file is part of ", name, " R package.
#
# ", name, " R package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# ", name, " R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ", name, " R package.
# If not, see <https://www.gnu.org/licenses/>.

"), Lines)
        Lines = paste0(Lines, collapse="\n")     
        cat(Lines, file=file.path(Rpath, basename(file)))
    }


    for (file in add_file) {
        file_path = file.path(package_path_name, file)
        file.create(file_path)

        if (file == "DESCRIPTION") {
            args = "name, date, description, author, email, imports"
            
        } else if (file == "LICENSE") {
            args = "name, date, author"
            
        } else if (file == "Makefile") {
            args = "git_install"

        } else if (file == "README.md") {
            args = "name, figure_path, lifecycle, git_install, isCovenant, description, documentation"
        } else {
            args = ""
        }

        write(eval(parse(text=paste0("packing_", file, "(", args, ")"))),
              file_path, append=TRUE)
    }
    
    devtools::document(package_path_name)
    download.file(figure_path, file.path(package_path_name, basename(figure_path)))

    Lines = readLines(file.path(package_path_name, "NAMESPACE"))
    Lines = Lines[!grepl("export[(].*[)].*[)]", Lines)]
    Lines = paste0(Lines, collapse="\n")   
    cat(Lines, file=file.path(package_path_name, "NAMESPACE"))
}


#' @title source_dev
#' @description Source an R package from the local development directory or load it from installed packages.
#' @param name Name of the package to source (default: "packageR")
#' @param dev_path Path to the local development directory (default: ".")
#' @return Loaded scripts
#' 
#' @examples
#' # Assuming you have a package named "CheshiRCat" in the local development directory "/home/alice/CheshiRCat/",
#' # you can source it using the following command:
#' source_dev(name="CheshiRCat", dev_path="/home/alice/CheshiRCat/")
#'
#' # If the dev_path points to a non-existing directory, this will source the installed package named "CheshiRCat":
#' source_dev(name="CheshiRCat", dev_path="/home/rabbit/carrot")
#'
#' @export
#' @md
source_dev = function (name="packageR", dev_path=".") {
    if (any(file.exists(dev_path))) {
        print(paste0("Loading ", name, " from local directory"))
        list_path = list.files(dev_path, pattern='*.R$', full.names=TRUE)
        for (path in list_path) {
            source(path, encoding='UTF-8')    
        }
    } else {
        print(paste0("Loading ", name, " from installed package"))
        library(name, character.only=TRUE)
    }
}
