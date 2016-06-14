#'
#' run big k-means++ implemented in Big Data Tools (BDT)
#'
#' @param bdt_home installation director for bdt
#' @param data_input an input dataset of the format: input-type@@location,
#' supported types are:
#'    text-mat@@path to a text file
#'    binary-mat@@path to a binary file
#'    bigmat@@path to a matrix outputed by another bdt cmd
#'    bams@@path to a file listing bam files
#'
#' @param data_nrow number of data rows for text-mat, binary-mat
#' @param data_ncol number of data columns for text-mat, binary-mat
#' @param data_col_names column names for text-mat, binary-mat
#' @param data_skip_cols number of first columns to skip for text-mat
#' @param data_skip_rows number of first rows to skip for text-mat
#' @param data_col_sep seperator used in text-mat
#' @param k The number of clusters
#' @param thread_num The number of threads used to do clustering
#' @param dist_type The distance used
#' @param max_iter max number of iteration of the kmeans
#' @param min_expchange min change of explained variance
#' @param seeding_method seeding method can be kmeans++, random, provided
#' @param seeds_input an input dataset as seed matrix
#' @param seeds_nrow number of data rows for text-mat, binary-mat
#' @param seeds_ncol number of data columns for text-mat, binary-mat
#' @param slave_num default 0, for using multiple machines
#' @param start_from advanced argument
#' @param out output dir
#'
#' @return ret a list representing bigKmeans results
#'
#' @export
#'
#' @examples
#'    ##full example at: https://github.com/fangdu64/BDT/tree/master/examples/R/bigKMeans#'
#'    ret = bigKmeans(
#'        bdt_home = bdtHome,
#'        data_input = paste0("output@@", thisScriptDir, '/../s04-export-noRuv/out'),
#'        k = 100,
#'        thread_num = 40,
#'        dist_type = 'Correlation',
#'        max_iter = 100,
#'        min_expchange = 0.0001,
#'        out = paste0(thisScriptDir,"/out"))

bigKmeans <- function(bdt_home = NULL,
                      data_input,
                      data_nrow = NULL,
                      data_ncol = NULL,
                      data_col_names = NULL,
                      data_skip_cols = NULL,
                      data_skip_rows = NULL,
                      data_col_sep = NULL,
                      k = 100,
                      thread_num = 4,
                      dist_type = 'Euclidean',
                      max_iter = 100,
                      min_expchange = 0.0001,
                      seeding_method = NULL,
                      seeds_input = NULL,
                      seeds_nrow = NULL,
                      seeds_ncol = NULL,
                      start_from = NULL,
                      slave_num = NULL,
                      out) {
    binPath = 'bigKmeans'
    if (!is.null(bdt_home)) {
        binPath = paste0(bdt_home, '/', binPath)
    } else {
        binPath = paste0(Sys.getenv('BDT_HOME'), '/', binPath)
    }

    cmds = c(
        'py',
        binPath,
        '--data-input', data_input,
        '--k', as.character(k),
        '--out', out,
        '--thread-num', as.character(thread_num),
        '--dist-type', dist_type,
        '--max-iter', as.character(max_iter),
        '--min-expchg', as.character(min_expchange))
    if (!is.null(data_nrow)) {
        cmds <- append(cmds, c('--data-nrow', as.character(data_nrow)))
    }

    if (!is.null(data_ncol)) {
        cmds <- append(cmds, c('--data-ncol', as.character(data_ncol)))
    }

    if (!is.null(data_col_names)) {
        cmds <- append(cmds, c('--data-col-names', paste0(data_col_names, collapse=",")))
    }

    if (!is.null(seeding_method)) {
        cmds <- append(cmds, c('--seeding-method', seeding_method))
    }

    if (!is.null(seeds_nrow)) {
        cmds <- append(cmds, c('--seeds-nrow', as.character(seeds_nrow)))
    }

    if (!is.null(seeds_ncol)) {
        cmds <- append(cmds, c('--seeds-ncol', as.character(seeds_ncol)))
    }

    if (!is.null(seeds_input)) {
        cmds <- append(cmds, c('--seeds-input', seeds_input))
    }

    if (!is.null(start_from)) {
        cmds <- append(cmds, c('--start-from', start_from))
    }

    if (!is.null(slave_num)) {
        cmds <- append(cmds, c('--slave-num', slave_num))
    }

    if (.Platform$OS.type == "windows") {
        command = cmds[1]
        args = cmds[-1]
    } else {
        command = cmds[2]
        args = cmds[-c(1,2)]
    }

    system2(command, args)

    ret <- readBigKmeansOutput(out)

    return (ret)
}
