#' Get the name of the CPU
#'
#' This function attempts to return the name of the CPU model. See Note.
#'
#' @details The function first detects the operating system using the
#'   `Sys.info()` function in R. Depending on the operating system, it uses
#'   OS-specific system commands to retrieve the CPU name:
#' * Windows (PowerShell): `(Get-WmiObject Win32_Processor).Name`
#' * Linux: `lscpu | grep "Model name:"`
#' * macOS: `sysctl -n machdep.cpu.brand_string`
#'
#' For operating systems other than the above, it returns "Unknown CPU."
#'
#' @return A character string representing the name of the CPU.
#'
#' @note This function has only been tested in Windows 11. Users are advised to
#'   manually test this function in their specific environment to ensure it
#'   works as expected.
#'
#' @export
#'
#' @examples
#' get_cpu_name()
get_cpu_name <- function() {
  # Detect the operating system
  os <- Sys.info()["sysname"]

  if (os == "Windows") {
    # Use PowerShell to get the CPU name on Windows
    cpu_name <- system2('powershell', '(Get-WmiObject Win32_Processor).Name', stdout = TRUE)
  } else if (os == "Linux") {
    # Use lscpu to get the CPU name on Linux
    cpu_info <- system('lscpu | grep "Model name:"', intern = TRUE)
    cpu_name <- sub("Model name:", "", cpu_info)  # Remove the "Model name:" part
  } else if (os == "Darwin") {
    # Use sysctl to get the CPU name on macOS
    cpu_name <- system('sysctl -n machdep.cpu.brand_string', intern = TRUE)
  } else {
    cpu_name <- "Unknown CPU"
  }

  return(cpu_name)
}
