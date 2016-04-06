#'
#' run bigKmeans on slave machines
#'
#' @param bdt_home installation director for bdt
#' @param thread_num The number of threads used to do clustering
#' @param master_host machine host where bigKmeans master is running
#' @param master_port machine port where bigKmeans master is running
#' @param out output dir
#'
#' @return ret a list representing bigKmeans results
#'
#' @export
#'
#' @examples
#'   ##full example at: https://github.com/fangdu64/BDT/tree/master/examples/R/bigKMeans
#'   bigKmeansC(
#'       bdt_home = bdtHome,
#'       thread_num = 2,
#'       master_host = 'localhost',
#'       master_port = 50662,
#'       out = paste0(thisScriptDir,"/02-slave-out"))

bigKmeansC <- function(bdt_home = NULL,
                      thread_num,
                      master_host,
                      master_port,
                      out) {
    binPath = 'bigKmeansC'
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
        '--master-host', master_host,
        '--master-port', as.character(master_port))

    if (.Platform$OS.type == "windows") {
        command = cmds[1]
        args = cmds[-1]
    } else {
        command = cmds[2]
        args = cmds[-c(1,2)]
    }

    system2(command, args)
}
