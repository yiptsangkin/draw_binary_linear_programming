library("Rglpk")
source('utils.R')
library("MASS")
# 设雇佣A x1天，雇佣B x2天
# 因此目标函数为min z = 25x1 + 22x2
obj <- c(25, 22)
# 约束条件
# A和B打扫会议室不少于5间
# x1 + x2 >= 5
# A和B清理桌子不少于12张
# 3x1 + 2x2 >= 12
# A和B清理货架不少于18个
# 3x1 + 6x2 <= 18
# 约束值
rhs <- c(5, 12, 18, 0, 0)
# 系数矩阵为
mat <- matrix(c(1,3,3,1,0,1,2,6,0,1), nrow=length(rhs))
# 约束符号
dir <- c(">=", ">=", ">=", ">=", ">=")
# 输出最大结果
max <- Rglpk_solve_LP(obj, mat, dir, rhs, max=TRUE)
# 输出最小结果
min <- Rglpk_solve_LP(obj, mat, dir, rhs, max=FALSE)

plant <- ggplot()
pointXList <- c()
pointYList <- c()

if (max$status == 0) {
    print(paste("最大费用：", max$optimum, "元"))
    print(paste("雇佣A", max$solution[1][1], "天，B", max$solution[2][1], "天"))
    # 绘制目标函数的范围
    plant <- plant + geom_point(
        aes(
            x = max$solution[1],
            y = max$solution[2]
        ),
        size = 2,
        color = "purple"
    )

    plant <- plant + annotate(
        geom = "text",
        x = max$solution[1],
        y = max$solution[2],
        label = paste("max:(", max$solution[1], ",", max$solution[2], ")"),
        vjust = 1.5,
        size = 2,
        color = "purple"
    )
    # 最大值曲线
    slope <- -fractions(obj[1] / obj[2])
    intercept <- -slope * max$solution[1] + max$solution[2]
    plant <- plant + geom_abline(
        intercept = intercept,
        slope = slope,
        size = 0.4,
        color = "red",
        
        linetype = "dashed"
    )
} else {
    print("最大解无可行解")
}
if (min$status == 0) {
    print(paste("最小费用：", min$optimum, "元"))
    print(paste("雇佣A", min$solution[1][1], "天，B", min$solution[2][1], "天"))
    # 画图
    lpPlot(min, mat, rhs)
    plant <- plant + geom_point(
        aes(
            x = min$solution[1],
            y = min$solution[2]
        ),
        size = 2,
        color = "purple"
    )

    plant <- plant + annotate(
        geom = "text",
        x = min$solution[1],
        y = min$solution[2],
        label = paste("min:(", min$solution[1], ",", min$solution[2], ")"),
        vjust = 1.5,
        size = 2,
        color = "purple"
    )
    # 最小值曲线
    slope <- -fractions(obj[1] / obj[2])
    intercept <- -slope * min$solution[1] + min$solution[2]
    plant <- plant + geom_abline(
        intercept = intercept,
        slope = slope,
        size = 0.4,
        color = "red",
        linetype = "dashed"
    )
    pointXList <- c(pointXList, min$solution[1])
    pointYList <- c(pointYList, min$solution[2])
} else {
    print("最小解无可行解")
}

areaList <- data.frame(x1=c(0, 2, 4, 6, 6), x2=c(6, 3, 1, 0, 6))

plant <- plant + geom_polygon(aes(x=x1, y=x2), data = areaList, fill="red", alpha=0.4)

print(plant)

