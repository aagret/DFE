
avg <- function(Q= Quantity, P= Price) {
    
    Position <- cumsum(Q)
    Cost     <-  P
    PnL      <- rep(0, length(Cost))
    
    if (length(Q) != 1) {
        
        # check if trade generate PnL
        check <- !(sign(shift(Position)) + sign(Position) == 0) %in% FALSE
        
        for (n in 2:length(Position)) {
            
            if (!check[n] & (sign(Position[n-1]) == sign(Q[n]))) {
                
                Cost[n] <- 
                    ((Cost[n-1] * Position[n-1]) +
                         (Cost[n] * Q[n])) /
                    Position[n]
            }
            
            if (!check[n] & (sign(Position[n-1]) != sign(Q[n]))) {
                
                Cost[n] <- Cost[n-1]
                
            }
            
            if (sign(Position[n-1]) != sign(Q[n]))
                
                PnL[n] <-
                    min(abs(Q[n]), abs(Position[n-1])) *
                    ifelse(Q[n] < 0, -1 , 1) *
                    (Cost[n-1] - P[n])
            
        }
        
        #trad[is.nan(Cost), Cost:=0]
        PnL[is.na(PnL)] <- 0
        PnL <- cumsum(PnL)
        
        data.table(Position, Cost, PnL)
        
    }
    
    data.table(Position, Cost, PnL)
    
}

#open exel trade history
library(readxl)
library(data.table)
library(plyr)

trades <- setDT(read_xlsx(
    path = "/home/Alexandre/DFEquity_Trades/DFE_Trades.xlsx"))[, c(1,2,5,8,12,14,15)]

colnames(trades) <- c("Port", "Date", "Trade", "Ticker", "Quantity", "Price", "Fx")

trades[, Date:=as.Date(as.character(Date), format="%Y%m%d")]

#trades[Trade != "SL", Trade:= "Purchase"]
#trades[Trade == "SL", Trade:= "Sale"]
trades[Trade == "Sale", Quantity:= -Quantity]

setkey(trades, Date, Ticker)

# extract Fx trades
#fx <- trades[grep("Curncy", Ticker), ]
#trades <- trades[!fx,]

# calc average prices & trade P&L
trades[, c("Position", "Cost","PnL"):= avg(Quantity, Price), by= .(Date, Ticker)]
trades[, c("FxCost"):= avg(Quantity, Fx)[,2], by= .(Date, Ticker)]

# fx[ , Ticker:= paste0(substr(Ticker, 1, 3), " Curncy")]
# fx[ , Price:= Fx]
# 
# fx[ , c("Position", "Cost", "PnL"):= avg(Quantity, Price), by=.(Date, Ticker)]
# fx[!grepl("EUR", Ticker), Position:= Position / 1000]
# 
# fx[, c("FxCost"):= avg(Quantity, Fx)[, 2], by= .(Date, Ticker)]
# fx <- fx[, .SD[.N], by= .(Date, Ticker)][ , .(Port, Date, Ticker, Position, Cost, FxCost)]
# 

# create portfolio changes
port <- trades[, .SD[.N], by= .(Date, Ticker)][ , .(Port, Date, Ticker, Position, Cost, FxCost)]
# port <- rbind(port, fx)


# get initial portfolio
p0 <- port[Date == min(Date)]

# construct daily portfolio
newPort <- list()
newPort[[1]] <- copy(p0)

for (x in 2:length(unique(port$Date))) {
    
    dt  <- unique(port$Date)[x]
    old <- newPort[[x-1]][!Ticker %in% port[Date== dt, Ticker]]
    old[, Date:= dt]
    
    newPort[[x]] <- rbind(old, port[Date == dt,])
    
}

newPort <- rbindlist(newPort)

# remove repeated empty positions
newPort <- rbind(newPort[Position !=0,],
                 newPort[Position ==0, .SD[1], by= Ticker])

setkey(newPort, Date, Ticker)

trades <- trades[!grepl("Curncy", Ticker), ]

# write.csv(x = newPort, file = "/home/artha/R-Projects/DFE/upload/positionsDFE.csv")
write.csv(x = trades,  file = "/home/artha/R-Projects/DFE/upload/tradesDFE.csv")

# get most recent portfolio
# pn <- newPort[Date == max(Date),]

