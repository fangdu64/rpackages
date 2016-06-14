#'
#' run bdvd-vd
#' @param bdt_home installation director for bdt
#' @param thread_num number of cpu threads to use
#' @param mem_size how many megabytes of RAM to use
#' @param bdvd_dir bdvd output folder
#' @param unwanted_factors #of unwanted factors to use
#' @param rowidx_from if export for a row range specify the row range [rowidx_from, rowidx_to)
#' @param rowidx_to if export for a row range specify the row range [rowidx_from, rowidx_to)
#' @param artifact_detection can be aggressive, conservative
#' @param known_factors know factors to use
#' @param out output folder

#' @export
#'
#' @examples
#'   ##full example at: https://github.com/fangdu64/BDT/blob/master/examples/analysis/DukeUwDnase/s08-Fstats-conservative/bdvd-fstats.R
#'   ##full example at: https://github.com/fangdu64/BDT/blob/master/examples/analysis/DukeUwExonArray/s02-Fstats-conservative/bdvd-fstats.R
#'    vdTbl = bdvdVd(
#'        bdt_home = bdtHome,
#'        thread_num = num_threads,
#'        mem_size = 16000,
#'        bdvd_dir = paste0(thisScriptDir, '/../s02-bdvd/out'),
#'        artifact_detection = 'conservative',
#'        unwanted_factors = unwanted_factors,
#'        known_factors = known_factors,
#'        out = paste0(thisScriptDir,"/out"))

bdvdVd <- function(bdt_home = NULL,
                 thread_num = 1,
                 mem_size = 1000,
                 bdvd_dir,
                 unwanted_factors,
                 rowidx_from = NULL,
                 rowidx_to = NULL,
                 artifact_detection = NULL,
                 known_factors = NULL,
                 out) {
    binPath = 'bdvdVd'
    if (!is.null(bdt_home)) {
        binPath = paste0(bdt_home, '/', binPath)
    } else {
        binPath = paste0(Sys.getenv('BDT_HOME'), '/', binPath)
    }

    cmds = c(
        'py',
        binPath,
        '--out', out,
        '--thread-num', as.character(thread_num),
        '--memory-size', as.character(mem_size),
        '--bdvd-dir', bdvd_dir,
        '--unwanted-factors', paste0(unwanted_factors, collapse=","))


    if (!is.null(rowidx_from)) {
        cmds <- append(cmds, c('--rowidx-from', as.character(rowidx_from)))
    }

    if (!is.null(rowidx_to)) {
        cmds <- append(cmds, c('--rowidx-to', as.character(rowidx_to)))
    }

    if (!is.null(artifact_detection)) {
        cmds <- append(cmds, c('--artifact-detection', artifact_detection))
    }

    if (!is.null(known_factors)) {
        cmds <- append(cmds, c('--known-factors', paste0(known_factors, collapse=",")))
    }

    if (.Platform$OS.type == "windows") {
        command = cmds[1]
        args = cmds[-1]
    } else {
        command = cmds[2]
        args = cmds[-c(1,2)]
    }

    system2(command, args)

    ret <- readBdvdVdOutput(out)

    return (ret)
}
