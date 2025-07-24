#' @title 65岁时的健康预期寿命
#'
#' @description表示65岁时健康预期寿命（预期在良好健康状况下持续多少年）的数据集，基于美国不同州
#' 在2007到2009年的数据。男性女性的预期值分开记录
#'
#' @docType data
#' @keywords datasets
#' @name life
#' @usage life
#' @format一个包含了50行和4个变量的数据框。变量分别为
#' \describe{
#'   \item{region}{一个有4个类别的因子型变量（North Central（中北部）、Northeast（东北部）、
#'                     South（南部）、West（西部））}
#'    \item{state}{一个包含了美国50个州的因子型变量，每个州用ISO标准的2个字母进行表示}
#'    \item{hlem}{用年份表示的男性健康预期寿命}
#'    \item{hlef}{用年份表示的女性健康预期寿命}
#' }
#' @source数据\code{hlem}和\code{hlef}从疾病预防和控制中心\emph{Morbidity and Mortality
#' Weekly Report}的\url{http://www.cdc.gov/mmwr/preview/mmwrhtml/mm6228a1.htm?s_
#' cid=mm6228a1_w}网站上获取
#' 变量\code{region}从\code{\link[datasets]{state.region}}数据集中所获取
NULL