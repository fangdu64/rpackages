#'
#' plot explained variance fractions as a function of # of unwanted factors
#' @param outPdf pdf file to output
#' @param bdvdRet output from bdvd run
#' @param maxK max K to display
#' @export
#'
#' @examples
#'   bdvdRet = readBdvdOutput(paste0(thisScriptDir, "/out"))
#'   plotBdvdKSelection(outPdf, bdvdRet, 20)

plotBdvdKSelection <- function(outPdf, bdvdRet, maxK) {
    eigenValues = readVec(bdvdRet$eigenValues)
    eigenVectors = readMat(bdvdRet$eigenVectors)
    permutatedEigenValues = readMat(bdvdRet$permutatedEigenValues)
    Wt = readMat(bdvdRet$Wt)

    e = 0.000001
    eigenValues[which(eigenValues<=e)]=0

    L = nrow(eigenValues)

    T = rep(0, L)
    for(k in 1:L){
        T[k] = eigenValues[k] / sum(eigenValues)
    }

    maxK = 20
    grDevices::pdf(file = paste0(plotOutDir, "/k_evalueation.pdf"))
    plotdata <- plot(1:L, T, type = "h", xlab = "", col = "gray", lty = 2,
         ylab = "Proportion", xlab = "K", bty = "n", xaxt = 'n', xlim = c(0.8, maxK))

    graphics::lines(1:L, T, type = "o", lwd = 2, lty = 1, col = "deepskyblue", pch = 19)
    graphics::axis(side = 1, at = 1:L, labels = as.character(1:L))

    ##
    ## null statistics
    ##

    B = ncol(permutatedEigenValues)
    T_0= vector(mode="list", length=L)
    for(k in 1:L) {
        T_0[[k]] = rep(0, B);
    }

    for(b in 1:B){
        pev = permutatedEigenValues[,b];
        pev[which(eigenValues<=e)]=0
        for(k in 1:L) {
            T_0[[k]][b] = pev[k] / sum(pev)
        }
    }

    ##
    ## box plot for null statistics
    ##
    plotdata <- graphics::boxplot(T_0, add = TRUE)
    grDevices::dev.off()
}
