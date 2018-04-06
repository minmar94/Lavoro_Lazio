# Functions

extract_var <- function(dataset, varname){
  # This function takes a .nc datafile and a variable name as input.
  # It gives back all the data for all the dimensions for that variable as output in array format.
  # It also set to NA the values coded as 1e+20
  array <- ncdf4::ncvar_get(dataset,varname)
  fillvalue <- ncdf4::ncatt_get(dataset,varname,"_FillValue")
  array[array==fillvalue$value] <- NA
  return(array)
}
