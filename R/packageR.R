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



# Lifecycle:Experimental
# The project is in the very early stages of development. The codebase will be changing frequently.

# Lifecycle:Maturing
# The codebase is being roughed out, but finer details are likely to change.

# Lifecycle:Stable
# The project is in a reliable state and major changes are unlikely to happen.

# Lifecycle:Dormant
# The project is currently not under active development, but there are plans to redevelop.

# Lifecycle:Retired
# The project is no longer being used and/or supported.


#' @title packing
#' @description ...
#' @param Rfile_pattern ... (default : ".*[.]R$")
#' @param name ... (default : "packageR")
#' @param date ... (default : Sys.Date())
#' @param description ... (default : "A simple R package that help to create simple R package.")
#' @param documentation ... (default : "Go into a directory where there are some R files and within a R console you can type `?packageR::packing` to see possible arguments and then type `packageR::packing(*arguments*)` to create your R package.")
#' @param author ... (default : "Louis Héraut")
#' @param email ... (default : "louis.heraut@inrae.fr")
#' @param imports ... (default : c("roxygen2", "remotes", "devtools", "stringr"))
#' @param git_install ... (default : "remotes::install_github('super-lou/packageR')")
#' @param figure_path ...
#' @param NULL ...
#' @param lifecycle ... (default : "Stable")
#' @param add_file ... (default : c("CODE_OF_CONDUCT.md", "DESCRIPTION", "LICENSE", "Makefile", "README.md"))
#' @return ...
#' @examples
#' ...
#' @export
packing = function (Rfile_pattern=".*[.]R$",
                    name="packageR",
                    date=Sys.Date(),
                    description="A simple R package that help to create simple R package.",
                    documentation="Go into a directory where there are some R files and within a R console you can type `?packageR::packing` to see possible arguments and then type `packageR::packing(*arguments*)` to create your R package.",
                    author="Louis Héraut",
                    email="louis.heraut@inrae.fr",
                    imports=c("roxygen2", "remotes", "devtools", "stringr"),
                    git_install="remotes::install_github('super-lou/packageR')",
                    figure_path=NULL,
                    lifecycle="Stable",
                    add_file=c("CODE_OF_CONDUCT.md", "DESCRIPTION",
                               "LICENSE", "Makefile", "README.md")) {
    
    if (!(dir.exists(name))) {
        dir.create(name)
    } else {
        stop (paste0(name, " directory already exists, choose an other name for the package"))
    }

    if (is.null(figure_path)) {
        path = dirname(dirname(sys.frame(1)$ofile))

        print(path)
        
        figure_path = list.files(path, "AE", pattern="AE_hex*",
                                 full.names=TRUE)

        print(figure_path)
        figure_path = figure_path[sample(1:length(figure_path), 1)]
    }

    print(figure_path)
    
    isCovenant = "CODE_OF_CONDUCT.md" %in% add_file

    Rpath = file.path(name, "R")
    dir.create(Rpath)
    Rfiles = list.files(pattern=Rfile_pattern, recursive=TRUE,
                        full.names=TRUE)

    for (file in Rfiles) {

        Lines = readLines(file)
        
        Id_function =
            which(
                sapply(Lines, grepl,
                       pattern=
                           "[=][[:space:]]*function[[:space:]]*[(]"))

        Doc = c()        
        for (i in 1:length(Id_function)) {
            id_function = Id_function[i]
            id_doc = id_function - 1

            if (id_function != 1) {
                while (!grepl("[[:graph:]]", Lines[id_doc])){
                    id_doc = id_doc - 1
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
                    gsub("[[:space:]]*[=][[:space:]]*function[[:space:]]*[(].*",
                         "", function_lines)

                function_args =
                    gsub("(.*[[:space:]]*[=][[:space:]]*function[[:space:]]*[(])|([)][[:space:]]*[{])",
                         "", function_lines)

                if (nchar(function_args) == 0) {
                    doc = paste0("#' @title ",
                                 function_name,"
#' @description ...
#' @return ...
#' @examples
#' ...
#' @export")
                    
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
#' @export")
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
        file_path = file.path(name, file)
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
    
    devtools::document(name)
    file.copy(from=figure_path, to=name)

    Lines = readLines(file.path(name, "NAMESPACE"))
    Lines = Lines[!grepl("export[(].*[)].*[)]", Lines)]
    Lines = paste0(Lines, collapse="\n")   
    cat(Lines, file=file.path(name, "NAMESPACE"))
}


#' @title source_dev
#' @description ...
#' @param name ... (default : "packageR")
#' @param dev_path ... (default : ".")
#' @return ...
#' @examples
#' ...
#' @export
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
