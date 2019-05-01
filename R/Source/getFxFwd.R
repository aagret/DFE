
# function to extract FxForwards
getFxFwd <- function(db= nav) {
    
    db <- db[Cat =="CAT" & Type != "AD1" & Price == 0,]
    db <- db[, ':=' (Ticker= paste(Ccy, 
                                   "/EUR ", 
                                   format(as.Date(Maturity, "%d/%m/%Y"), "%m/%d/%Y"),
                                   " Curncy", sep=""),
                     Price= NA)][, .(Port, Date, Ticker, Amount, Price)]
}
