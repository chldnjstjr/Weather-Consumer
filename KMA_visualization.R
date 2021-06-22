# 시각화

#3D Graph
#도매시장 경락 데이터 시각화: http://agdatalab.com/post/wholesale_market_data2/

### 3D graph
qty_year_month <- trans %>% group_by(YEAR,MONTH) %>% summarise(input=sum(QTY,na.rm=T)) %>% spread(MONTH,input,fill=0) %>% ungroup()
d5 <- as.matrix(qty_year_month[,-1])
hist3D(z = d5, x = as.numeric(t(qty_year_month[,1])), y = 1:12, scale = F,expand = 0.000009, bty = "g", phi = 30, theta = 20, col = jet.col(100), alpha = 0.5,border = "black", shade = 0.2, ltheta = 90, space = 0.4, ticktype = "detailed",d = 2, xlab='연도',ylab='월',zlab='구매량')
plot3D::scatter3D(as.numeric(trans$MONTH), as.numeric(trans$YEAR), QTY)

# set the x, y, and z variables
x <- as.numeric(trans$MONTH)
y <- Advertising$TV
z <- Advertising$sales

# Compute the linear regression 
fit <- lm(z ~ x + y)

# create a grid from the x and y values (min to max) and predict values for every point
# this will become the regression plane
grid.lines = 40
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

# create the fitted points for droplines to the surface
fitpoints <- predict(fit)

# scatter plot with regression plane
scatter3D(x, y, z, pch = 19, cex = 1,colvar = NULL, col="red", 
          theta = 20, phi = 10, bty="b",
          xlab = "Radio", ylab = "TV", zlab = "Sales",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = TRUE, fit = fitpoints, col=ramp.col (col = c("dodgerblue3","seagreen2"), n = 300, alpha=0.9), border="black"), main = "Advertising")

