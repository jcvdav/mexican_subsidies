##########################
## Paths to directories ##
##########################
# Check for OS
sys_path <- ifelse(Sys.info()["sysname"]=="Windows", "G:/","/Users/juancarlosvillasenorderbez/Library/CloudStorage/GoogleDrive-juancarlos@ucsb.edu/")
# Path to our emLab's data folder
data_path <- paste0(sys_path,"Shared drives/emlab/data")
# Path to this project's folder
project_path <- paste0(sys_path,"Shared drives/emlab/projects/current-projects/mexican-subsidies")
# Path to all Mexican data
mex_path <- paste0(sys_path,"Shared drives/emlab/projects/current-projects/mex-fisheries")

# Reset theme
ggplot2::theme_set(startR::ggtheme_plot())

ggplot2::theme_update(
  axis.title.y = ggplot2::element_text(hjust = 1, size = 14),
  axis.title.x = ggplot2::element_text(hjust = 1, size = 14),
  axis.text.y = ggplot2::element_text(size = 12),
  axis.text.x = ggplot2::element_text(size = 12)
)

ggplot2::update_geom_defaults(geom = "point",
                              new = list(color = "black",
                                         fill = "steelblue",
                                         shape = 21,
                                         size = 2))

ggplot2::update_geom_defaults(geom = "col",
                              new = list(color = "black",
                                         fill = "steelblue"))

ggplot2::update_geom_defaults(geom = "bar",
                              new = list(color = "black",
                                         fill = "steelblue"))

ggplot2::update_geom_defaults(geom = "area",
                              new = list(color = "black",
                                         fill = "steelblue"))


# Turn off dplyr's anoying messages
options(dplyr.summarise.inform = FALSE)

# Functions

statab <- function(data, var) {
  count(data, {{var}}) %>% 
    mutate(percent = n / sum(n),
           cum = cumsum(percent) * 100)
}

get_alpha <- function(model){
  beta <- coefficients(model)[1]
  theta <- coefficients(model)[2]
  
  alpha <- round(1 + (theta/beta), 3)
  return(alpha)
}
