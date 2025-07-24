#' @title对非参组间比较的结果进行绘图
#' 
#' @description \code{plot.oneway}对非参组间比较的结果进行绘图
#' 
#' @details 
#' 这个函数使用标记了的并排箱线图对\code{\link{oneway}}函数所生成的非参组间比较结果进行绘图，
#' 中位数和样本量被放置在图的上方没总体中位数用一条虚横线进行表示
#' 
#' @param x一个\code{oneway}类型的对象
#' @param ...被传递给\code{\link{boxplot}}函数的额外参数
#' @method plot oneway
#' @export
#' @return NULL
#' @author Lenardar <hangzhouliuhaoran@163.com>
#' @example
#' result <- oneway(y ~ x, data = mydata)
#' plot(result)


plot.oneway <- function(x, ...){
    # 检查输入
    if(!inherits(x, "oneway")){
        stop("The object must be of class 'oneway'")
    }

    # 数据准备
    data <- x$data
    y <- data[, 1]
    g <- data[, 2]
    stats <- x$sumstats
    lbl <- paste("md=", stats[2, ], "\nn=", stats[1, ], sep="")

    # 生成箱线图
    opar <- par(no.readonly = TRUE)
    par(mar=c(5, 4, 8, 2))
    boxplot(y~g, ...)
    abline(h=median(y), lty=2, col="darkgrey")
    axis(3, at=1:length(lbl), labels=lbl, cex.axis=.9)
    par(opar)
}