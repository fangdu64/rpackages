#'
#' read output from bigMat
#' @param outDir output folder for bigMat
#' @export
#'
#' @examples
#'   ##full example at: https://github.com/fangdu64/BDT/blob/master/examples/analysis/DukeUwDnase/s01-bam2mat/binInfo.R
#'   output = readBigMatOutput(paste0(thisScriptDir,"/out"))

readBigMatOutput <- function(outDir) {
    env <- new.env()
    fileName = paste0(outDir,"/logs/output.R")
    sys.source(fileName, env)
    if (env$inputType == 'bams')  {
        ret <- list(bigMat = env$bigMat,
                    binMap = env$binMap)
    } else {
        ret <- list(bigMat = env$bigMat)
    }

    return (ret)
}

#'
#' read output from bigKmeans
#' @param outDir output folder for bigKmeans
#' @export
#'
#' @examples
#'   output = readBigKmeansOutput(paste0(thisScriptDir,"/out"))

readBigKmeansOutput <- function(outDir) {
    env <- new.env()
    fileName = paste0(outDir,"/logs/output.R")
    sys.source(fileName, env)
    ret = list(dataMat = env$dataMat,
               seedsMat = env$seedsMat,
               centroidsMat = env$centroidsMat,
               clusterAssignmentVec = env$clusterAssignmentVec)

    return (ret)
}

#'
#' read output from bdvd
#' @param outDir output folder for bdvd
#' @export
#'
#' @examples
#'   ##full example at: https://github.com/fangdu64/BDT/blob/master/examples/analysis/DukeUwExonArray/s01-bdvd/bdvd.R
#'   bdvdRet = readBdvdOutput(paste0(thisScriptDir, "/out"))
readBdvdOutput <- function(outDir) {
    env <- new.env()
    fileName = paste0(outDir,"/logs/output.R")
    sys.source(fileName, env)
    ret = list(eigenValues = env$eigenValues,
               eigenVectors = env$eigenVectors,
               permutatedEigenValues = env$permutatedEigenValues,
               Wt = env$Wt)

    return (ret)
}

#'
#' read output from bdvd-export
#' @param outDir output folder for bdvd-export
#' @export
#'
#' @examples
#'   ##full example at: https://github.com/fangdu64/BDT/blob/master/examples/analysis/DukeUwDnase/s08-Fstats-conservative/bdvd-fstats.R
#'   exportRet = readBdvdExportOutput(paste0(thisScriptDir,"/out"))
readBdvdExportOutput <- function(outDir) {
    env <- new.env()
    fileName = paste0(outDir,"/logs/output.R")
    sys.source(fileName, env)
    ret = list(mats = env$Mats)

    return (ret)
}
