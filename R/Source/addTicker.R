
# add Bloomberg Tickers (need separate data file for Isin/ticker relation)
addTicker <- function(db= secPos) {
    
    tickers <- read.csv("~/R-Projects/DFC/Config/tickers.csv", header= FALSE, stringsAsFactors = FALSE)
    colnames(tickers) <- c("Isin", "Ticker")
    
    setDT(tickers, key= c("Isin", "Ticker"))
    setkey(db, Isin)
    
    db <- tickers[db]
    
}
