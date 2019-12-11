library("ggplot2")
library("MASS")
lpPlot <- function(result, limit, limitValue) {
    for (i in 1:length(limitValue)) {
        if (limit[i,2] == 0) {
            # 斜率无穷大，画垂直线
            pointXList <<- c(pointXList, fractions(limitValue[i] / limit[i,1]))
            pointYList <<- c(pointYList, 0)
            plant <<- plant + geom_vline(
                xintercept = c(fractions(limitValue[i] / limit[i,1])),
                size = 0.4,
                color = "green"
            )
        } else if (limit[i,1] == 0) {
            # 斜率为0，画横线
            pointXList <<- c(pointXList, 0)
            pointYList <<- c(pointYList, fractions(limitValue[i] / limit[i,2]))
            plant <<- plant + geom_hline(
                yintercept = c(fractions(limitValue[i] / limit[i,2])),
                size = 0.4,
                color = "green"
            )
        } else {
            # 斜线
            intercept <- fractions(limitValue[i] / limit[i,2])
            slope <- fractions(-limit[i,1] / limit[i,2])
            # 计算两个点
            plant <<- plant + geom_point(
                aes(
                    x = 0,
                    y = intercept
                ),
                size = 0,
                color = "#ffffff"
            )
            pointXList <<- c(pointXList, 0)
            pointYList <<- c(pointYList, intercept)
            plant <<- plant + annotate(
                geom = "text",
                x = 0,
                y = intercept,
                label = paste("(", 0, ",", intercept, ")"),
                vjust = 1.5,
                size = 2,
                color = "purple"
            )
            plant <<- plant + geom_point(
                aes(
                    x = fractions(-intercept / slope),
                    y = 0
                ),
                size = 0,
                color = "#ffffff"
            )
            pointXList <<- c(pointXList, fractions(-intercept / slope))
            pointYList <<- c(pointYList, 0)
            plant <<- plant + annotate(
                geom = "text",
                x = fractions(-intercept / slope),
                y = 0,
                label = paste("(", fractions(-intercept / slope), ",", 0, ")"),
                vjust = 1.5,
                size = 2,
                color = "purple"
            )
            plant <<- plant + geom_abline(
                intercept = intercept,
                slope = slope,
                size = 0.4,
                color = "red"
            )
        }
    }
}