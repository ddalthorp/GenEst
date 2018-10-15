update_output_clear_all <- function(rv, output){
  output <- update_output_file_SE_clear(rv, output)
  output <- update_output_file_CP_clear(rv, output)
  output <- update_output_file_DWP_clear(rv, output)
  output <- update_output_file_SS_clear(rv, output)
  output <- update_output_file_CO_clear(rv, output)
  return(output)
}