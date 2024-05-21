attach(mtcars)
cor(mpg, wt)



plot(dist ~ speed, data = cars,
     xlab = "Speed (mph)",
     ylab = "Stopping Distance (ft)",
     pch  = 16, col = "blue",
     xlim = c(-20,30),
     ylim = c(-20,120))

lm_res = lm(dist ~ speed, data = cars)
abline(lm_res, col = 'red')
abline(c(0, 0), col = 'black')
abline(c(0, -100000), col = 'black')

predict(lm_res, 
        newdata = list(speed = 19.5),
        interval = "prediction")
