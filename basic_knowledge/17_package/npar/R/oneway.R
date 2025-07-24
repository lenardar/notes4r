#' @title 非参组件比较
#' 
#' @description 
#' \code{oneway}计算非参组间比较，包括综合检验和事后组间比较
#' 
#' @details
#' 这个函数计算了一个综合Kruskal-Wallis检验，用于检验组别是否相等，接着使用Wilcoxon秩和检验来
#' 进行成对比较。如果因变量之间没有相互依赖的话，可以计算精确的Wilcoxon检验，使用\cde{\link{p.
#' adjust}}来对多重比较所得到的p值进行调整。
#' 
#' @param formula一个formula类的对象，用于表示因变量和分组变量之间的联系
#' @param data一个包含了模型里变量的数据框
#' @param exact逻辑型变量。如\code{TRUE}，则使用精确的Wilcoxon检验
#' @param sort逻辑型变量。如果\code{TRUE}，用因变量中位数对组别进行排序
#' @param method字符串变量。取("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", 
#' "fdr", "none")，用于调整多重比较p值的方法
#' 
#' @export
#' @return一个有7个元素的列表：
#' \item{CALL}{ 函数调用 }
#' \item{data}{ 包含因变量和组件变量的数据框} 
#' \item{sumstats}{ 包含每组的描述性统计量的数据框 }
#' \item{kw}{ Kruskal-Wallis检验的结果 }
#' \item{method}{ 用于调整多重比较p值的方法 }
#' \item{wmc}{ 包含多重比较的数据框 }
#' \item{vnames}{ 变量名 }
#' 
#' @author Lenardar <hangzhouliuhaoran@163.com>
#' @examples
#' result <- oneway(hlef~region, life)
#' summary(results)
#' plot(results)

# 函数调用
oneway <- function(
    formula, 
    data, 
    exact=FALSE,
    sort=TRUE,
    method="holm"
){
    # 检查参数
    if (
        missing(formula) ||
        class(formula)!= "formula" ||
        length(all.vars(formula))!=2
    ){
        stop("'formula' is missing or incorrect")
    }

    method <- match.arg(method)

    # 设定数据
    df <- model.frame(formula, data)
    y <- df[1]
    g <- as.factor(df[[2]])
    vnames <- names(df)

    # 重新排序
    if (sort) g <- reodrder(g, y, FUN=median)
    groups <- levels(g)
    k <- nlevels(g)

    # 总体统计量
    getstats <- function(x)(c(N =length(x), Median=median(x), MAD=mad(x)))
    sumstats <- t(aggregate(y, by=list(g), FUN=getstats)[2])
    rownames(sumstats) <- c("n", "median", "mad")
    colnames(sumstats) <- groups

    # 统计检验
    kw <- kruskal.test(formula, data)
    wmc <- NULL
    for (i in 1:(k-1)){
        for (j in (i+1):k){
            y1 <- y[g==groups[i]]
            y2 <- y[g==groups[j]]
            test <- wicox.test(y1, y2, exact=exact)
            r <- data.frame(
                Group.1=groups[i],
                Group.2=groups[j],
                W=test$statistic[[1]],
                p=test$p.value
            )
            # 注意使用[[]]来提取单个元素
            wmc <- rbind(wmc, r)
        }
    }
    wmc$p <- p.adjust(wmc$p, method=method)

    # 返回结果
    data <- data.frame(y, g)
    names(data) <- vnames
    results <- list(
        CALL=match.call(),
        data=data,
        sumstats=sumstats,
        kw=kw,
        method=method,
        wmc=wmc,
        vnames=vnames
    )
    class(results) <- c("oneway", "list")
    return(results)
}