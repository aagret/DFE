
# extract from NAV data the (ex)Dividend announced but not yet paid
getPendingDiv <- function(db= nav) {
    
    db <- db[Cat =="CPON", .(Type= "PendingDividend",
                             Amount= sum(Amount)), by= .(Date, Port, Ccy)]
    
    ptax <- calcPaidTax(db)
    
    db <- rbind(ptax, db)
    
    db[Type == "Wh_Tax", Type:="PendingWh_Tax"]
}
