#'
#' run bigMat implemented in Big Data Tools (BDT)
#'
#' @param bdt_home installation director for bdt
#' @param input an input dataset of the format: input-type@@location,
#' supported types are:
#'    text-mat@@path to a text file
#'    binary-mat@@path to a binary file
#'    bigmat@@path to a matrix outputed by another bdt cmd
#'    bams@@path to a file listing bam files

#' @param row_cnt number of data rows for text-mat, binary-mat
#' @param col_cnt number of data columns for text-mat, binary-mat
#' @param skip_cols column names for text-mat, binary-mat
#' @param skip_rows number of first columns to skip for text-mat
#' @param col_sep number of first rows to skip for text-mat
#' @param out output dir
#'
#' @export
#'
#' @examples
#'   ##full example at: https://github.com/fangdu64/BDT/tree/master/examples/linux/bigMat

bigMat <- function(bdt_home,
                   input,
                   row_cnt = NULL,
                   col_cnt = NULL,
                   skip_cols = NULL,
                   skip_rows = NULL,
                   col_sep = NULL,
                   out) {
    cmds = c(
        'py',
        paste0(bdt_home,"/bigMat"),
        '--input', input,
        '--out', out)

    if (!is.null(row_cnt)) {
        cmds <- append(cmds, c('--nrow', as.character(row_cnt)))
    }

    if (!is.null(col_cnt)) {
        cmds <- append(cmds, c('--ncol', as.character(col_cnt)))
    }

    if (.Platform$OS.type == "windows") {
        command = cmds[1]
        args = cmds[-1]
    } else {
        command = cmds[2]
        args = cmds[-c(1,2)]
    }

    system2(command, args)

    return (1)
}
