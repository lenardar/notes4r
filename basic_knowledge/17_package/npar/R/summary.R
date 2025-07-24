#' @title汇总单因子非参分析的结果
#' 
#' @description \code{summary.oneway}汇总单因子非参分析的结果
#' nonparametric analysis.
#' 
#' @details
#' 这个函数对\code{\link{oneway}}函数所分析的结果进行汇总并打印，这包括了每一组的描述性统计量，
#' 一个综合Krusatal-Wallis检验的结果，以及一个Wicoxon成对多重比较的结果。
#' 
#' @param object一个\code{oneway}类型的对象
#' @param ...额外的参数
#' @method summary oneway
#' @export 
#' @return静默返回输入的对象
#' @author Lenardar <hangzhouliuhaoran@163.com>
#' @example 
#' result <- oneway(y ~ x, data = mydata)
#' summary(result)


summary.oneway <- function(object, ...){
    # 检查输入
    if(!inherits(object, "oneway")){
        stop("The object must be of class 'oneway'")
    }

    if (!exists("digits")) digits <- 4L

    kw <- object$kw
    wmc <- object$wmc
    cat("data:", object$vnames[1], "on", object$vnames[2], "\n\n")

    # Kruskal-Wallis检验
    cat("Omnibus Test\n")
    cat(paste(
        "Kruskal-Wallis chi-squared = ",
        round(kw$statistic, 4),
        ", df = ",
        round(kw$parameter, 3),
        ", p-value = ",
        format.pval(kw$p.value, digits = digits),
        "\n\n",
        sep = ""
    ))

    # 描述性统计量
    cat("Descriptive Statistics\n")
    print(object$sumstats, ...)

    # 表格标记
    wmc$stars <- " "
    wmc$stars[wmc$p.value < 0.001] <- "***"
    wmc$stars[wmc$p.value < 0.01 & wmc$p.value >= 0.001] <- "**"
    wmc$stars[wmc$p.value < 0.05 & wmc$p.value >= 0.01] <- "*"
    wmc$stars[wmc$p.value < 0.1 & wmc$p.value >= 0.05] <- "."
    names(wmc)[which(names(wmc)=="stars")] <- " "

    # 成对分组比较
    cat("\nMultiple Comparisons (Wilcoxon Rank Sum Tests)\n")
    cat(paste("Probability Adjustment = ", object$method, "\n", sep=""))
    print(wmc, ...)
    cat("---\n")
    cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' '\n")
}