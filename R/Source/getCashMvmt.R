
#retrieve cash movements from KBL files
getCashMvmt <- function(fileList= fileList) {
    
    fileSelect <- fileList[grepl("CshStmt_JRN", fileList)]
    
    db <- getData(fileSelect)[grep("EQUITY FUND", ClientName), 
                              c(2, 6:11, 13)]
    
    colnames(db) <- c("Port", "Ccy", "Type", "Ref", "Name", 
                      "Value", "Date", "Amount")
    
    db[, ':=' (Port=  "DF EQUITY",
               Value= as.Date(Value, "%Y-%m-%d"),
               Date=  as.Date(Date,  "%Y-%m-%d"),
               Amount= as.numeric(gsub(",", ".", gsub("\\.", "", Amount)))), ]
    
    db[Type == "IPC", Type:="PaidInterest"]
    db[Type == "OCD", Type:="NetDividend"]
    db[grep("Redemption", Name), Type:= "SubRed"]
    db[!Type %in% c("PaidInterest", "NetDividend", "SubRed"), Type:= "PaidFees"]
    
    setDT(db, key= c("Date", "Ccy"))
    
    db <- db[, sum(Amount), by= .(Date, Port, Ccy, Type)]
    colnames(db)[5] <- "Amount"
    
    # add whitholdding tax datas
    tax <- calcPaidTax(db)
    
    db <- rbind(tax,db)
    
}
