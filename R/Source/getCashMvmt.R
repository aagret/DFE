
#retrieve cash movements from KBL files
getCashMvmt <- function(fileList= fileList) {
    
    fileSelect <- fileList[grepl("CshStmt_JRN", fileList)]
    
    db <- getData(fileSelect)[grep("GLOBAL CREDIT", ClientName), 
                              c(2, 6:11, 13)]
    
    colnames(db) <- c("Port", "Ccy", "Type", "Ref", "Name", 
                      "Value", "Date", "Amount")
    
    db[, ':=' (Port=  "DF CREDIT",
               Value= as.Date(Value, "%Y-%m-%d"),
               Date=  as.Date(Date,  "%Y-%m-%d"),
               Amount= as.numeric(gsub(",", ".", gsub("\\.", "", Amount)))), ]
    
    # remove funds purchase/sub/prepayment
    db <- db[!Type %in% c("BPC", "EMC", "BOC", "EPC",
                          "DSC", "DCC", "DTC")]
    
    
    db[Type == "IPC", Type:="PaidInterest"]
    db[Type == "OCD", Type:="NetDividend"]
    db[grep("Redemption", Name), Type:= "SubRed"]
    db[grep("TFR", Name), Type:= "SubRed"]
    db[!Type %in% c("PaidInterest", "NetDividend", "SubRed", "Redemption"), 
       Type:= "PaidFees"]
    
    
    setDT(db, key= c("Date", "Ccy"))
    
    db <- db[, sum(Amount), by= .(Date, Port, Ccy, Type)]
    colnames(db)[5] <- "Amount"
    
    # add whitholdding tax datas
    #tax <- calcPaidTax(db)
    
    #db <- rbind(tax,db)
    
    return(db)
}
