##########################
## Paths to directories ##
##########################
# Check for OS
sys_path <- ifelse(Sys.info()["sysname"]=="Windows", "G:/","~/Google Drive File Stream/")
# Path to our emLab's data folder
data_path <- paste0(sys_path,"Shared drives/emlab/data")
# Path to this project's folder
project_path <- paste0(sys_path,"Shared drives/emlab/projects/current-projects/mexican-subsidies")

