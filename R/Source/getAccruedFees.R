
# extract Accrued Fees from NAV datas
getAccruedFees <- function(db= nav) {
    
    db <- db[Cat == "TRES" & !substr(Code, 1, 2) %in% c("DC", "BA"), ]
    db <- db[, .(Type= "AccruedFees",
                 Amount=sum(Amount)), by= .(Date, Port, Ccy)]
} 
