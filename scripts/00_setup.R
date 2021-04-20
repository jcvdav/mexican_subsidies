##########################
## Paths to directories ##
##########################
# Check for OS
sys_path <- ifelse(Sys.info()["sysname"]=="Windows", "G:/","/Volumes/GoogleDrive/")
# Path to our emLab's data folder
data_path <- paste0(sys_path,"Shared drives/emlab/data")
# Path to this project's folder
project_path <- paste0(sys_path,"Shared drives/emlab/projects/current-projects/mexican-subsidies")

# Reset theme
ggplot2::theme_set(startR::ggtheme_plot())

ggplot2::theme_update(
  axis.title.y = element_text(hjust = 1),
  axis.title.x = element_text(hjust = 1),
  axis.text.y = element_text(size = 12),
  axis.text.x = element_text(size = 12)
)


# Turn off dplyr's anoying messages
options(dplyr.summarise.inform = FALSE)
