############################################
#
#   Selecting Activity data
#   uses actTradedAt to eliminate days with forcasts before and after target dates


actQestnIdExp <- actQestnId
actTradedAtExp <- actTradedAt
actnumTradesExp <- actnumTrades

actTradedAtExp[actTradedAtExp<expStart] <- NA
actTradedAtExp[actTradedAtExp>=expStop] <- NA

