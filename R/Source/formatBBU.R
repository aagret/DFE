
# format data for Bloomberg BBU uplad
formatBBU <- function(db1= secPos, db2= allCash) {
    

    
    db2 <- melt(db2[, .(Date, Ccy, Fees, OffsetCash)], 
                id.vars= c("Date", "Ccy"), 
                mesure.vars= c( "Fees", "Tax", "OffsetCash"),
                variable.factor= FALSE)
    db2 <- db2[value !=0]
    
    db2[, Price:= numeric()]
    db2[, Port:=  "DF CREDIT"]
    
    db2[variable == "OffsetCash", variable:= paste(Ccy, "Curncy", sep=" ")]
    
    db2[variable == "Fees", ':=' (variable= ".DRCFEES LX Equity",
                                  Price= -value,
                                  value= -1)]
    
    
    # db2[variable == "Tax", ':=' (variable= paste(".TAX_", Ccy, " LX", sep=""),
    #                              Price= -value,
    #                              value= -1)]
    
    db2[, Ccy:=NULL]
    colnames(db2)[2:3] <- c("Ticker", "Amount")
    
    db <- rbind(db1, db2)
    
}
