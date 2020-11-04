my_packages = c("shiny", "plotly", 'dplyr', 'lubridate', 'tidyverse', 'readxl', 'randomcoloR', 'colourpicker', 'shinyFiles')

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))