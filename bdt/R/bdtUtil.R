#'
#' read output from bigMat
#' @param matInfo mat meta info outputed from bdt cmds
#' @return matrix with double values
#' @export
#'
#' @examples
#'   ##full example at: https://github.com/fangdu64/BDT/blob/master/examples/analysis/DukeUwExonArray/s01-bdvd/bdvd.R
#'   bdvdRet = readBdvdOutput(paste0(thisScriptDir, "/out"))
#'   eigenVectors = readMat(bdvdRet$eigenVectors)

readMat <- function(matInfo) {
    mat = readBigMatrixAuto(matInfo$colCnt, matInfo$rowCnt, matInfo$storePathPrefix)
    return (mat)
}

#'
#' read output from bigVec
#' @param vecInfo vector meta info outputed from bdt cmds
#' @return vector with double values
#' @export
#'

#' @examples
#'   ##full example at: https://github.com/fangdu64/BDT/blob/master/examples/analysis/DukeUwExonArray/s01-bdvd/bdvd.R
#'   bdvdRet = readBdvdOutput(paste0(thisScriptDir, "/out"))
#'   eigenValues = readVec(bdvdRet$eigenValues)

readVec <- function(vecInfo) {
    mat = readBigMatrixAuto(1, vecInfo$rowCnt, vecInfo$storePathPrefix)
    return (mat)
}

#'
#' read output from integer matrix
#' @param matInfo mat meta info outputed from bdt cmds
#' @return matrix with integer values
#' @export
#'
#' @examples
#'   ##full example at: https://github.com/fangdu64/BDT/blob/master/examples/R/bigKMeans/01-kmeans.R
#'   clusterAssignments = readIntVec(ret$clusterAssignmentVec)

readIntMat <- function(matInfo) {
    storePath = paste0(matInfo$storePathPrefix,".bfv")
    mat = readBigMatrixInt(matInfo$colCnt, matInfo$rowCnt, storePath)
    return (mat)
}

#'
#' read output from bigVec
#' @param vecInfo vector meta info outputed from bdt cmds
#' @return vector with integer values
#' @export
#'
#' @examples
#'   ##full example at: https://github.com/fangdu64/BDT/blob/master/examples/R/bigKMeans/01-kmeans.R
#'   clusterAssignments = readIntVec(ret$clusterAssignmentVec)
readIntVec <- function(vecInfo) {
    storePath = paste0(vecInfo$storePathPrefix,".bfv")
    mat = readBigMatrixInt(1, vecInfo$rowCnt, storePath)
    return (mat)
}

#'
#' get script dir
#' @return directory containing current R script
#' @export
#'
#' @examples
#'   thisScriptDir = getScriptDir()
getScriptDir <- function() {
    args = commandArgs()
    m <- regexpr("(?<=^--file=).+", args, perl=TRUE)
    scriptDir <- dirname(regmatches(args, m))
    if(length(scriptDir) == 0) stop("can't determine script dir: please call the script with Rscript")
    if(length(scriptDir) > 1) stop("can't determine script dir: more than one --file argument detected")
    return (scriptDir)
}

#'
#' get Genome position by bin idx
#' @param binMap binMap
#' @param binIdx 0-based
#' @export
#'
#' @examples
#'   ##full example at: https://github.com/fangdu64/BDT/blob/master/examples/analysis/DukeUwDnase/s01-bam2mat/binInfo.R
#'   rowIdxs = c(101, 5055549, 11946429, 16950329, 23039435, 26701809, 30362725, 30362945)
#'   for (binIdx in rowIdxs) {
#'       ret = getGenomePosByBinIdx(binMap, binIdx)
#'       ## bpFrom and bpTo are 0-based
#'       print(paste0('chromosome: ',ret$ref, ', bp:[',ret$bpFrom, ', ',ret$bpTo,')'))
#'   }

getGenomePosByBinIdx <- function(binMap, binIdx) {
    refIdx = Position(function(i) {
        return((binMap$refBinFroms[i] <= binIdx) && (binMap$refBinTos[i] > binIdx))
    }, 1:length(binMap$refBinFroms))

    localBpFrom = (binIdx - binMap$refBinFroms[refIdx]) * binMap$binWidth
    localBpTo = localBpFrom + binMap$binWidth
    ret = list(ref = binMap$refNames[refIdx],
               bpFrom = localBpFrom,
               bpTo = localBpTo)
    return (ret)
}

#'
#' get bin idx by Genome position
#' @param binMap binMap
#' @param refName chromosome e.g., chr1
#' @param bpIdx base-pair position within the chromosome, 0-based
#' @export
#'
#' @examples
#'   ##full example at: https://github.com/fangdu64/BDT/blob/master/examples/analysis/DukeUwDnase/s01-bam2mat/binInfo.R
#'   refs = c("chr1", "chr3", "chr6")
#'   bps = c(10140, 13104810, 132100750)
#'   for (i in 1:length(refs)) {
#'       binIdx = getBinIdxByGenomePos(binMap, refs[i], bps[i])
#'       print(paste0('rowIdx: ', binIdx))
#'   }

getBinIdxByGenomePos <- function(binMap, refName, bpIdx) {
    refIdx = Position(function(i) {
        return(binMap$refNames[i] == refName)
    }, 1:length(binMap$refNames))

    binIdx = as.integer(bpIdx / binMap$binWidth) + binMap$refBinFroms[refIdx]
    return (binIdx)
}

#'
#' readBigMatrix
#' @param colCnt colCnt
#' @param rowCnt rowCnt
#' @param bfvFile bfv file path
#' @export
#' @examples
#'   mat = readBigMatrix(colCnt,rowCnt,bfvFile)

readBigMatrix<-function(colCnt,rowCnt,bfvFile)
{
    totalValueCnt = rowCnt*colCnt
    con=file(bfvFile, "rb")
    Y = readBin(con, double(),n=totalValueCnt,endian = "little")
    Y=matrix(data=Y,nrow=rowCnt,ncol=colCnt,byrow=TRUE)
    close(con)
    return (Y)
}

#'
#' readBigMatrixAuto
#' @param colCnt colCnt
#' @param rowCnt rowCnt
#' @param bfvFilePrefix bfv file prefix
#' @param batchFileSizeMB batchFileSizeMB
#' @export
#'
#' @examples
#'   mat = readBigMatrixAuto(matInfo$colCnt, matInfo$rowCnt, matInfo$storePathPrefix)
readBigMatrixAuto<-function(colCnt,rowCnt,bfvFilePrefix, batchFileSizeMB=1024)
{
    szValueType=8
    batchFileSize=batchFileSizeMB*1024*1024
    rowBytesSize=colCnt*szValueType
    if(batchFileSize%%rowBytesSize!=0){
        #data row will not split in two batch files
        batchFileSize = batchFileSize-(batchFileSize%%rowBytesSize)
    }
    valueCntPerBatchFile = as.integer(batchFileSize / szValueType)
    rowCntPerBatchFile = as.integer(valueCntPerBatchFile / colCnt)
    batchFileIdxFrom=0
    batchFileIdxTo= as.integer((rowCnt-1)/rowCntPerBatchFile)
    if(batchFileIdxTo==batchFileIdxFrom)
    {
        bfvFile=paste(bfvFilePrefix,".bfv",sep="")
        return (readBigMatrix(colCnt,rowCnt,bfvFile))
    }
    else
    {
        return (readBigMatrixBatches(colCnt,rowCnt,bfvFilePrefix,batchFileSizeMB))
    }

}

#'
#' readBigMatrixBatches
#' @param colCnt colCnt
#' @param rowCnt rowCnt
#' @param bfvFilePrefix bfv file prefix
#' @param batchFileSizeMB batchFileSizeMB
#' @export
#'
#' @examples
#'   mat = readBigMatrixBatches(colCnt,rowCnt,bfvFilePrefix,batchFileSizeMB)

readBigMatrixBatches<-function(colCnt,rowCnt,bfvFilePrefix, batchFileSizeMB=1024)
{
    szValueType=8
    batchFileSize=batchFileSizeMB*1024*1024
    rowBytesSize=colCnt*szValueType
    if(batchFileSize%%rowBytesSize!=0){
        #data row will not split in two batch files
        batchFileSize = batchFileSize-(batchFileSize%%rowBytesSize)
    }
    valueCntPerBatchFile = as.integer(batchFileSize / szValueType)
    rowCntPerBatchFile = as.integer(valueCntPerBatchFile / colCnt)
    batchFileIdxFrom=0
    batchFileIdxTo= as.integer((rowCnt-1)/rowCntPerBatchFile)
    Y=matrix(NA,nrow=rowCnt,ncol=colCnt)
    for(i in batchFileIdxFrom:batchFileIdxTo)
    {
        bfvFile=paste(bfvFilePrefix,"_",i,".bfv",sep="")
        rowIDFrom = i*rowCntPerBatchFile
        if(i==batchFileIdxTo){
            thisBatchRowCnt = rowCnt%%rowCntPerBatchFile
        }
        else{
            thisBatchRowCnt = rowCntPerBatchFile
        }

        y=readBigMatrix(colCnt,thisBatchRowCnt,bfvFile)
        batchRowIDs=(rowIDFrom+1):(rowIDFrom+thisBatchRowCnt)
        Y[batchRowIDs,]=y
    }
    return (Y)
}

#'
#' readBigMatrixInt
#' @param colCnt colCnt
#' @param rowCnt rowCnt
#' @param bfvFile bfv file path
#' @export
#' 
#' @examples
#'   mat = readBigMatrixInt(matInfo$colCnt, matInfo$rowCnt, storePath)

readBigMatrixInt<-function(colCnt,rowCnt,bfvFile)
{
    totalValueCnt = rowCnt*colCnt
    con=file(bfvFile, "rb")
    Y = readBin(con, integer(),size=4,n=totalValueCnt,endian = "little")
    Y=matrix(data=Y,nrow=rowCnt,ncol=colCnt,byrow=TRUE)
    close(con)
    return (Y)
}

#'
#' readVectorFromTxt
#' @param txtFile text file path
#' @export
#'
#' @examples
#'   ##full example at: https://github.com/fangdu64/BDT/blob/master/examples/analysis/DukeUwDnase/s13-correlation-aggresive/bdvd-correlation.R
#'   rowIDs_s1 = readVectorFromTxt(paste0(bdtDatasetsDir, "/DNaseExonCorrelation/100bp/s01-TSS-PairIdxs/DNase_RowIDs.txt"))

readVectorFromTxt<-function(txtFile)
{
    vec=utils::read.table(txtFile, sep="\t")
    vec=vec[,1]
    return (vec)
}

#'
#' vecListToString
#' @param vecList vecList
#' @export
#'
#' @examples
#'   known_factors = c(1,2,3)
#'   strParam = vecListToString(known_factors)

vecListToString<-function(vecList)
{
    outStr = c()
    for (v in vecList) {
        outStr <- append(outStr, paste0('[', paste0(v, collapse=","), ']'))
    }
    return (paste0(outStr, collapse=","))
}


#'
#' getConfigOrder
#' @param ks unwanted factors
#' @param k known factors
#' @param k this unwanted factor
#' @param n this known factor
#'
#' @examples
#'   ks = c(1, 1, 2, 3)
#'   ns = c(1, 0, 0, 0)
#'   getConfigOrder(ks, ns, 1, 0)

getConfigOrder <- function(ks, ns, k, n) {
    oval = unique(ks+ns*1000) #put known factors to rightmost
    oval = oval[order(oval)]
    v = k + n*1000
    for( i in 1:length(oval)) {
        if(v==oval[i])
            return (i)
    }
    return (0)
}

#'
#' getConfigTexts
#' @param ks unwanted factors
#' @param k known factors
#'
#' @examples
#'   ks = c(1, 1, 2, 3)
#'   ns = c(1, 0, 0, 0)
#'   getConfigTexts(ks, ns)
getConfigTexts <- function(ks, ns) {
    oval=unique(ks+ns*1000) #put known factors to rightmost
    oval=oval[order(oval)]
    txts=rep("", length(oval))
    for( i in 1:length(oval)) {
        if(oval[i] >= 1000) {
            txts[i]="KF"
        } else {
            txts[i] = as.character(oval[i]%%1000)
        }
    }
    return (txts)
}