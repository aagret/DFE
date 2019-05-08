
# extract datas from administrator weekly NAV file
getNavData <- function(fileList= fileList) {
    
    # get all positions from NAV
    fileSelect <- fileList[grepl("ffpos1", fileList)]
    
    db <- ldply(fileSelect, fread)
    db <- setDT(db[, c(8:11, 16, 21, 36, 42, 44:54)])
    
    # format Nav data
    colnames(db) <- c("Date", "Code", "Cat", "Name", "Ccy", "Maturity", "Type",  
                      "L/S", "Price", "PriceOvr", "EurAMount", "Amount",
                      "Position", "Cost", "EurCost", "Accrued","EurAccrued",
                      "PrepaidInterest", "EurPrepaidInterest")
    
    db[, ':=' (Port=  "DF CREDIT",
               Date= as.Date(Date, format= "%d/%m/%Y"))]
    
    setkey(db, Date, Ccy)
    
}
