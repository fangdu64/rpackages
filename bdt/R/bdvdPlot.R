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
    plotdata <- plot(1:L, T, type = "h", xlab = "K", col = "gray", lty = 2,
         ylab = "Proportion", bty = "n", xaxt = 'n', xlim = c(0.8, maxK))

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

#'
#' plot variance decomposition using different # of unwanted factors
#' @param outPdf pdf file to output
#' @param vdTbl  from bdvdVd run

#' @export
#'
#' @examples
#'   vdTbl = readBdvdVdOutput(paste0(thisScriptDir, "/out"))
#'   plotVD(outPdf, vdTbl)
plotVD <- function(outPdf, vdTbl) {
    KsCnt=nrow(vdTbl)
    bu = rep(0, KsCnt)
    b = rep(0, KsCnt)
    b_bl = rep(0, KsCnt)
    b_wl = rep(0, KsCnt)

    for(i in 1:KsCnt){
        k = vdTbl[i,"k"]
        extW = vdTbl[i, "extW"]
        SS_t = vdTbl[i, "SS_t"]
        SS_r = vdTbl[i, "SS_r"]
        SS_bu = vdTbl[i, "SS_bu"]
        SS_b = vdTbl[i, "SS_b"]
        SS_u = vdTbl[i, "SS_u"]

        b_SS_t = vdTbl[i, "b_SS_t"]
        b_SS_bl = vdTbl[i, "b_SS_bl"]
        b_SS_wl = vdTbl[i, "b_SS_wl"]
        
        # re-order the run configs to put known factor to rightmost
        runRowID = getConfigOrder(vdTbl[,"k"], vdTbl[,"extW"], k, extW)

        # Biological + Unwanted 
        bu[runRowID] = SS_bu/SS_t
        b[runRowID] = SS_b/SS_t
        b_bl[runRowID] = b[runRowID]*(1-b_SS_bl/b_SS_t)
        b_wl[runRowID] = b[runRowID]*(1-b_SS_wl/b_SS_t)
    }
    
    runNames = getConfigTexts(vdTbl[,"k"],vdTbl[,"extW"])
    
    colIdxs=c(1:KsCnt)
    propMat=cbind(b_bl, b-b_bl, bu-b, 1-bu)
    colnames(propMat)=c("b_wl","b_bl","u","e")
    rownames(propMat)=runNames[colIdxs]

    ##
    ## stacked barplot
    ##
    grDevices::pdf(file = outPdf)
    #plot bars first
    colIdxs=c(1:KsCnt)
    graphics::barplot(t(propMat),col=c("#4dac26", "#b8e186", "#e7298a", "#f4cae4"),
        xlab="k", ylab="Proportion")
    grDevices::dev.off()
}
