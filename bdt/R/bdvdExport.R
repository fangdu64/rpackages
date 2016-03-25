#'
#' run bdvd-export
#' @param bdt_home installation director for bdt
#' @param thread_num number of cpu threads to use
#' @param mem_size how many megabytes of RAM to use
#' @param column_ids which columns (samples) to export
#' @param bdvd_dir bdvd output folder
#' @param component can be signal, artifact, random, signal+random
#' @param unwanted_factors #of unwanted factors to use
#' @param rowidx_from if export for a row range specify the row range [rowidx_from, rowidx_to) to export 
#' @param rowidx_to if export for a row range specify the row range [rowidx_from, rowidx_to) to export 
#' @param export_scale can be mlog or original
#' @param artifact_detection can be aggressive, conservative
#' @param known_factors know factors to use
#' @param export_names a list of names for export result
#' @param rowidxs_input vector to specify which rows are to be used as negative control
#' @param rowidxs_index_base base (e.g., 0, 1) used for rowidxs_input
#' @param start_from advanced argument
#' @param out output folder

#' @export
#'
#' @examples
#'   ##full example at: https://github.com/fangdu64/BDT/blob/master/examples/analysis/DukeUwDnase/s08-Fstats-conservative/bdvd-fstats.R
#'   ##full example at: https://github.com/fangdu64/BDT/blob/master/examples/analysis/DukeUwExonArray/s02-Fstats-conservative/bdvd-fstats.R
#'    exportRet = bdvdExport(
#'        bdt_home = bdtHome,
#'        thread_num = num_threads,
#'        mem_size = 16000,
#'        column_ids = exportSampleIds,
#'        bdvd_dir = paste0(thisScriptDir, '/../s02-bdvd/out'),
#'        component = 'signal+random',
#'        artifact_detection = 'conservative',
#'        unwanted_factors = unwanted_factors,
#'        known_factors = known_factors,
#'        rowidxs_input = paste0("text-rowids@", bdtDatasetsDir, "/DukeUwDnase/100bp/RowIdxs/signalRandom.txt"),
#'        rowidxs_index_base = 0,
#'        out = paste0(thisScriptDir,"/out"))

bdvdExport <- function(bdt_home,
                 thread_num = 1,
                 mem_size = 1000,
                 column_ids,
                 bdvd_dir,
                 component,
                 unwanted_factors,
                 rowidx_from = NULL,
                 rowidx_to = NULL,
                 export_scale = NULL,
                 artifact_detection = NULL,
                 known_factors = NULL,
                 export_names = NULL,
                 rowidxs_input = NULL,
                 rowidxs_index_base = NULL,
                 start_from = NULL,
                 out) {
    cmds = c(
        'py',
        paste0(bdt_home,"/bdvdExport"),
        '--out', out,
        '--thread-num', as.character(thread_num),
        '--memory-size', as.character(mem_size),
        '--column-ids', paste0(column_ids, collapse=","),
        '--bdvd-dir', bdvd_dir,
        '--component', component,
        '--unwanted-factors', paste0(unwanted_factors, collapse=","))


    if (!is.null(rowidx_from)) {
        cmds <- append(cmds, c('--rowidx-from', as.character(rowidx_from)))
    }

    if (!is.null(rowidx_to)) {
        cmds <- append(cmds, c('--rowidx-to', as.character(rowidx_to)))
    }

    if (!is.null(export_scale)) {
        cmds <- append(cmds, c('--scale', export_scale))
    }

    if (!is.null(artifact_detection)) {
        cmds <- append(cmds, c('--artifact-detection', artifact_detection))
    }

    if (!is.null(known_factors)) {
        cmds <- append(cmds, c('--known-factors', paste0(known_factors, collapse=",")))
    }

    if (!is.null(export_names)) {
        cmds <- append(cmds, c('--export-names', paste0(export_names, collapse=",")))
    }

    if (!is.null(rowidxs_input)) {
        cmds <- append(cmds, c('--rowidxs-input', rowidxs_input))
    }

    if (!is.null(rowidxs_index_base)) {
      cmds <- append(cmds, c('--rowidxs-index-base', as.character(rowidxs_index_base)))
    }

    if (!is.null(start_from)) {
        cmds <- append(cmds, c('--start-from', start_from))
    }

    if (.Platform$OS.type == "windows") {
        command = cmds[1]
        args = cmds[-1]
    } else {
        command = cmds[2]
        args = cmds[-c(1,2)]
    }

    system2(command, args)

    ret <- readBdvdExportOutput(out)

    return (ret)
}
