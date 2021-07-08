##########################
## Paths to directories ##
##########################
# Check for OS
sys_path <- ifelse(Sys.info()["sysname"]=="Windows", "G:/","/Volumes/GoogleDrive/")
# Path to our emLab's data folder
data_path <- paste0(sys_path,"Shared drives/emlab/data")
# Path to this project's folder
project_path <- paste0(sys_path,"Shared drives/emlab/projects/current-projects/mexican-subsidies")
# Path to all Mexican data
mex_path <- paste0(sys_path,"Shared drives/emlab/projects/current-projects/mex-fisheries")

# Reset theme
ggplot2::theme_set(startR::ggtheme_plot())

ggplot2::theme_update(
  axis.title.y = ggplot2::element_text(hjust = 1),
  axis.title.x = ggplot2::element_text(hjust = 1),
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


# Turn off dplyr's anoying messages
options(dplyr.summarise.inform = FALSE)
