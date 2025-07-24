#' @title打印多重比较的结果
#' 
#' @description
#' \code{print.oneway}打印了多重组间比较的结果
#' 
#' @details 这个函数打印出用\code{\link{oneway}}函数所创建的Wilcoxon成对多重比较的结果
#' 
#' @param x一个\code{\oneway}类型的变量
#' @param ...要传输给函数的额外变量
#' @method print oneway
#' @export
#' @return静默返回输入的对象
#' @author Lenardar <hangzhouliuhaoran@163.com>
#' @examples 
#' result <- oneway(hlef~region, life)
#' print(result)


print.oneway <- function(x, ...){
    # 检查输入
    if (!inherits(x, "oneway")){
        stop("输入必须是oneway对象")
    }

    # 打印头部
    cat("data:", x$vnames[1], "by", x$vnames[2], "\n\n")
    cat("Multiple Comparisons (Wilcoxon Rank Sun Tests)\n")
    cat(paste("Probability Adjustment = ", x$method, "\n", sep=""))

    # 打印表格
    print(x$wmc, ...)
}