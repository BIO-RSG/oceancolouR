#' Read pixEx
#'
#' Read pixEx file output from SNAP Pixel extraction tool
#'
#' @param filename String of .csv file location
#' @return List containing the contents of the file and attributes
#' @export
read_pixEx <- function(filename) {
    # File metadata
    pixex_info <- read.csv(filename, nrows = 5, stringsAsFactors = F, col.names = F)
    pixex_box_size <- stringr::str_split(pixex_info[2,], pattern = " ")[[1]][4]
    pixex_date_create <- stringr::str_split(pixex_info[3,], pattern = "\t")[[1]][2]
    wavelengths <- stringr::str_split(pixex_info[4,], pattern = "\t")[[1]][-1]
    wavelengths <- wavelengths[nchar(wavelengths) > 1]

    #Grab data
    pixex_data <- read.csv(filename, skip = 6, header = T, sep = "\t", fill = T)

    return(list(file_name = filename,
                file_date = pixex_date_create,
                box_size = pixex_box_size,
                wavelengths = wavelengths,
                pixex_data = pixex_data))
}
