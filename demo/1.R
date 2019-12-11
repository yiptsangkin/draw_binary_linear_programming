library("Rglpk")
source('utils.R')
library("MASS")
# 设公司每天生产x1件甲产品，x2件乙产品
# 因此目标函数为max z = 300x1 + 200x2
obj <- c(300, 200)
# 约束条件
# 甲每天需求量不会超过5单位
# x1 <= 5
# 加工中心A每天有效时间为20
# x1 + 2x2 <= 20
# 加工中心B每天有效时间为18
# 3x1 + x2 <= 18
# 约束值
rhs <- c(5, 20, 18, 0, 0)
# 系数矩阵为
mat <- matrix(c(1,1,3,1,0,0,2,1,0,1), nrow=length(rhs))
# 约束符号
dir <- c("<=", "<=", "<=", ">=", ">=")
# 输出最大结果
max <- Rglpk_solve_LP(obj, mat, dir, rhs, max=TRUE)
# 输出最小结果
min <- Rglpk_solve_LP(obj, mat, dir, rhs, max=FALSE)

plant <- ggplot()
pointXList <- c()
pointYList <- c()

if (max$status == 0) {
    print(paste("最大利润：", max$optimum, "元"))
    print(paste("公司每天生产甲", max$solution[1][1], "件，乙", max$solution[2][1], "件"))
    # 画图
    lpPlot(max, mat, rhs)
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
    print("无可行解")
}
if (min$status == 0) {
    print(paste("最小利润：", min$optimum, "元"))
    print(paste("公司每天生产甲", min$solution[1][1], "件，乙", min$solution[2][1], "件"))
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
    print("无可行解")
}

areaList <- data.frame(x1=c(0, 0, 3.2, 6), x2=c(0, 10, 8.4, 0))

plant <- plant + geom_polygon(aes(x=x1, y=x2), data = areaList, fill="red", alpha=0.4)

print(plant)

