#' Copies Necessary Components Into Simulation-Specific Directory
#'
#' @param dir.from Directory components are copied FROM - probably dir.admb
#' @param dir.to Directory components are copied TO - probably dir.temp
#'
#' @return
#' @export
#'
#' @examples
copy_admb_sim <- function(dir.from=dir.admb, dir.to=dir.temp) {
  # ADMB Executible
  file.copy(from=file.path(dir.from,"tem.exe"), to=file.path(dir.to,"tem.exe"))
  # Control File
  file.copy(from=file.path(dir.from,"tem.ctl"), to=file.path(dir.to,"tem.ctl"))
  # Dat File
  # Dat File
  file.copy(from=file.path(dir.from,"fixed_data.dat"), to=file.path(dir.to,"fixed_data.dat"))
  file.copy(from=file.path(dir.from,"tem_single2018.dat"), to=file.path(dir.to,"tem_single2018.dat"))
  file.copy(from=file.path(dir.from,"permanant_tem_single2018.dat"), to=file.path(dir.to,"permanant_tem_single2018.dat")) # Not really necessary
}