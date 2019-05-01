# https://stackoverflow.com/a/4090208

list.of.packages <- c('shiny',
                    'shinydashboard',
                    'shinycssloaders',
                    'DT',
                    'rlang',
                    'data.table',
                    'readxl',
                    'readr',
                    'magrittr',
                    'scales',
                    'ggplot2',
                    'ggthemes',
                    'ggrepel',
                    'gridExtra',
                    'GGally',
                    'lubridate',
                    'anytime',
                    'devtools',
                    'ggiraph')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if (length(new.packages) > 0) {
    install.packages(new.packages)
}


