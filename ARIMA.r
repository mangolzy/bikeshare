require(stats)
require(forecast)

#period means the shift of the seasoal window
args <- commandArgs(TRUE)
train_file <- args[1]
shift <- as.integer(args[2])

#train_0 formats as 'number,0,2,...''
train_0 <- read.csv(train_file)
train_1 <- ts(train_0$number, start = 1, frequency = 168)

res = tryCatch({
    split <- stl(train_1, s.window='periodic')
    seasonal <- split$time.series[,1]
    trend <- split$time.series[,2]
    remainder <- split$time.series[,3]

    train_2 <- ts(as.integer(remainder), start = 1, frequency = 24)
    fit <- auto.arima(train_2)
    fcast <- forecast(fit, h = 24)
    predict <- as.integer(seasonal[(1 + 24 * (shift - 1)) : (24 * shift)]) + mean(trend) + as.integer(fcast$mean)
    output <- data.frame(predict)
    colnames(output) <- c('number')
    if(length(output$number == 24)){
        write.csv(output, file = "predict.csv", row.names = FALSE)
        print("1") 
    } else {
        stop("predict value error!!!")
    }
}, error = function(e){
     print("0\n")
     print(paste("ERROR: ", e))  
})


