#' EOF chlorophyll-a model
#'
#' Calculate chlorophyll-a for input Rrs (remote sensing reflectances) using the EOF (empirical orthogonal function) method tested in the Gulf of Saint Lawrence.
#'
#' The training set should be created using in situ chlorophyll-a values matched to satellite Rrs. Use caution if you try to predict chlorophyll-a from Rrs where the region / water type / time period is very different from those used in the training set. The training set should contain at least 50 matchups (i.e. 50 chla values paired with satellite Rrs, see figure 4 from Laliberté et al, 2018).
#'
#' @param rrs Either a dataframe containing numeric columns of Rrs data with the naming format "Rrs_XXX", where XXX is the wavelength in nanometres, or a RasterStack where each raster contains the Rrs data and follows the same naming format.
#' @param training_set Dataframe with numeric columns of Rrs data with the same naming format as in rrs, and a column named "chla" containing the corresponding in situ chlorophyll-a.
#' @references
#' Laliberté, Julien & Larouche, Pierre & Devred, Emmanuel & Craig, Susanne. (2018). Chlorophyll-a Concentration Retrieval in the Optically Complex Waters of the St. Lawrence Estuary and Gulf Using Principal Component Analysis. Remote Sensing. 10. 10.3390/rs10020265.
#'
#' Function written by Julien Laliberté, July 2020. Source here:
#'
#' https://catalogue.ogsl.ca/data/mpo/dcb7ed2e-bce9-40c5-a197-71ac6b46bd5c/algorithme_predict_chl-a.pdf
#'
#' Modified 18 Dec 2020 by Stephanie Clay for use in the oceancolouR package.
#'
#' @return Numeric vector of chla values (for dataframe input) or raster of chla values (for RasterStack input)
#' @examples
#' # create training set dataframe and rrs dataframe to test the function
#' # source: https://catalogue.ogsl.ca/data/mpo/dcb7ed2e-bce9-40c5-a197-71ac6b46bd5c/algorithme_predict_chl-a.pdf
#' training <- data.frame(id = c(8141L, 8489L, 11385L, 5627L, 9885L, 5027L, 7158L, 5027L, 5028L, 10812L),
#'                        chla = c(0.26, 1.94, 4.33, 0.43, 1.8975, 0.635, 2.3025, 0.51, 0.6025, 0.245),
#'                        Rrs_412 = c(0.00186400086658978, 0.00182400086669077, 0.000450000870159784, 0.00164600086714017,
#'                                    0.00350800086243908, 0.00242000086518601, 0.00187000086657463,
#'                                    0.000630000869705327, 0.000138000870947508, 0.00433400086035363),
#'                        Rrs_443 = c(0.00233000086541324, 0.00199200086626661, 0.000990000868796415,0.00165800086710988,
#'                                    0.00382400086164125, 0.00274000086437809,0.00201400086621106,
#'                                    0.00144000086766027, 0.00072200086947305,0.00408600086097977),
#'                        Rrs_490 = c(0.00282600086416096, 0.00196600086633225,0.00161800086721087, 0.00161200086722602,
#'                                    0.00352400086239868,0.00311600086342878, 0.00220000086574146,
#'                                    0.00204000086614542,0.00124600086815008, 0.0038020008616968),
#'                        Rrs_510 = c(0.00250600086496888,0.00205400086611007, 0.00169800086700889, 0.00153000086743305,
#'                                    0.00315600086332779, 0.00316000086331769, 0.00225200086561017,
#'                                    0.00205600086610502, 0.00140200086775621, 0.00333600086287333),
#'                        Rrs_555 = c(0.00185600086660997, 0.00189000086652413, 0.00181600086671097,0.00126400086810463,
#'                                    0.00214000086589294, 0.00281800086418116,0.0023640008653274,
#'                                    0.00177200086682205, 0.0013380008679178,0.0027960008642367),
#'                        Rrs_670 = c(0.000248000870669784, 0.000220000870740478,0.000292000870558695, 6.40008711343398e-05,
#'                                    0.000324000870477903,0.000432000870205229, 0.000628000869710377,
#'                                    0.000168000870871765,0.000144000870932359, 0.000304000870528398),
#'                        stringsAsFactors = FALSE)
#' rrs <- data.frame(Rrs_412 = c(0.000932000868942851,0.00205000086612017, 0.000216000870750577, 0.00226600086557482,
#'                               0.00264000086463057, 0.000746000869412455, 0.00179000086677661,
#'                               0.0042260008606263, 0.00189200086651908, 0.000468000870114338),
#'                   Rrs_443 = c(0.00167600086706443, 0.00230000086548898, 0.000754000869392257,0.00247000086505977,
#'                               0.00274000086437809, 0.00201800086620096,0.00221400086570611,
#'                               0.00489600085893471, 0.00260200086472651,0.000840000869175128),
#'                   Rrs_490 = c(0.0023600008653375, 0.00269000086450433,0.00139000086778651, 0.00268000086452957,
#'                               0.00283600086413571,0.00316000086331769, 0.00282200086417106,
#'                               0.00516200085826313,0.0027920008642468, 0.00149200086752899),
#'                   Rrs_510 = c(0.00213200086591314,0.00270000086447908, 0.00157000086733206, 0.00254800086486284,
#'                               0.00272200086442353, 0.00337800086276729, 0.00267400086454472,
#'                               0.00416400086078283, 0.00249200086500423, 0.00160400086724621),
#'                   Rrs_555 = c(0.00159800086726136, 0.00292800086390343, 0.00159400086727146,0.00232400086542839,
#'                               0.00223000086566572, 0.00331000086293898,0.00207400086605958,
#'                               0.00264000086463057, 0.00162600086719067,0.0013260008679481),
#'                   Rrs_670 = c(0.000132000870962656, 0.000762000869372059,0.000186000870826319, 0.000774000869341762,
#'                               0.00022800087072028,0.000566000869866912, 0.000358000870392061,
#'                               0.000338000870442556,0.000360000870387012, 6.40008711343398e-05),
#'                   stringsAsFactors = FALSE)
#'
#' # test with a dataframe of rrs
#' chl_vector <- eof_chl(rrs=rrs, training_set=training)
#' print(chl_vector)
#'
#' # transform rrs dataframe into RasterStack, and test it
#' library(raster)
#' library(magrittr)
#' rrs_stack <- rrs[1:9,] %>% lapply(FUN=matrix, nrow=3) %>% lapply(raster) %>% stack()
#' names(rrs_stack) <- colnames(rrs)
#' chl_raster <- eof_chl(rrs=rrs_stack, training_set=training)
#' plot(chl_raster)
#'
#' @export
eof_chl <- function(rrs, training_set) {

    input_class <- class(rrs)[1]

    # get names of rrs in training set
    tset_rrsnames <- sort(colnames(training_set)[grepl("Rrs_[0-9]{3}$", colnames(training_set))])

    # extract rrs and chla columns from training set, and make sure they're in order
    training_set <- dplyr::select(training_set, all_of(c(tset_rrsnames, "chla")))
    # make sure all training data are valid and >0
    training_set <- training_set[rowSums(as.matrix(training_set)>0,na.rm=TRUE)==ncol(training_set),]
    nrow_tset <- nrow(training_set)
    if (nrow_tset==0) {
        stop("No valid training data in training_set. All Rrs and chla must be >0")
    } else if (nrow_tset < 30) {
        warning(paste("Only",nrow_tset,"valid data points in the training set (Rrs and chla must be >0)."))
    }

    # format input rrs
    if (input_class=="data.frame") {

        rrs <- dplyr::select(rrs, all_of(tset_rrsnames))

    } else if (input_class == "RasterStack") {

        # reformat raster input
        rstack <- raster::subset(rrs, tset_rrsnames)
        rrs <- lapply(1:length(tset_rrsnames), function(i) raster::getValues(rstack[[i]]))
        rrs <- as.data.frame(do.call(cbind, rrs))
        colnames(rrs) <- tset_rrsnames

    } else {

        stop("rrs must be a dataframe or RasterStack with names matching the Rrs names in the training set")

    }

    full_eof_chl <- rep(NA, nrow(rrs))

    # get valid indices for rrs and training set
    rrs_mat <- as.matrix(rrs)
    valid_ind <- rowSums(is.finite(rrs_mat) & rrs_mat > 0) == ncol(rrs_mat)
    rrs <- rrs[valid_ind,]

    # combine rrs and training sets
    all <- log10(dplyr::bind_rows(rrs, dplyr::select(training_set, -chla)))

    # perform PCA on the logged rrs columns and retrieve the scores
    my_pca <- princomp(all)
    sc <- data.frame(my_pca$scores)

    # get the row indices of the training set
    i <- (nrow(rrs)+1):nrow(all)

    # create a dataframe with the training set in situ chla and pca scores of the corresponding rrs
    df_chl <- data.frame(chl = as.numeric(training_set$chla), sc = sc[i,])

    # create a dataframe of pca scores from rrs in the testing set
    df_rrs <- data.frame(sc = sc[-i,])

    # regress in situ chla against linear combination of pca scores
    my_lm <- lm(log10(chl) ~ ., data = df_chl)

    # Select subset of PC (remove unnecessary variables)
    step.lm <- step(my_lm, direction = "both", trace = 0)

    # Predict chla with selected PC
    eof_chl <- as.numeric(10^predict(step.lm, newdata = df_rrs))

    # Remove extreme values
    eof_chl[which(eof_chl > 100 | eof_chl < 0.001)] <- NA

    full_eof_chl[valid_ind] <- eof_chl

    # reformat to raster if necessary
    if (input_class == "RasterStack") {
        full_eof_chl <- raster::raster(crs=raster::crs(rstack[[1]]), ext=raster::extent(rstack[[1]]), resolution=raster::res(rstack[[1]]), vals=full_eof_chl)
    }

    # return predicted chlorophyll for the input rrs
    return(full_eof_chl)

}
