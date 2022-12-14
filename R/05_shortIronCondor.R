#'Calculates per share Profit and Loss (PnL) at expiration for Short Iron Condor Option Strategy and draws its Bar Plot displaying PnL in the Plots tab.
#'@description
#'This volatility strategy is a combination of a bear put spread and a bull call spread and consists of a short position in an OTM put option (out of the money put : put Strike price is lower than spot price X1L )  with a strike price X1L, a long position in put option with higher Strike X2 price and a long position  OTM (out of the money)  call option with a strike price X3, and a short position in call option  with a higher strike price X4H. The strikes are equidistant: X2 minus X1L equals to X4H minus X3 . This is a net debit trade. The trader or investor has an outlook that is  neutral (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Short Iron Condor Option Strategy and draw its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param X1L Lower Strike Price or eXercise price for one OTM shorted Put.
#'@param X2 Strike Price or eXercise price for one bought Put.
#'@param X3 Strike Price or eXercise price for one bought Call.
#'@param X4H Higher Strike Price or eXercise price for one OTM shorted Call.
#'@param P1L Put Premium or Put Price received for the first OTM shorted Put.
#'@param P2 Put Premium or Put Price paid for the bought Put.
#'@param C3 Put Premium or Put Price paid for the bought Call.
#'@param C4H Call Premium or Put Price received for the one OTM shorted Call.
#'@param PnL Profit and Loss
#'@param spot Spot Price
#'@param pl Profit and Loss
#'@param myData Data frame
#'@param myTibble tibble
#'@param hl lower bound value for setting lower-limit of x-axis displaying spot price.
#'@param hu upper bound value for setting upper-limit of x-axis displaying spot price.
#'@return graph of the strategy
#'@importFrom magrittr %>%
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 geom_point
#'@importFrom ggplot2 scale_fill_manual
#'@importFrom ggplot2 scale_color_manual
#'@importFrom ggplot2 geom_col
#'@importFrom tibble as_tibble
#'@importFrom dplyr mutate
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 element_line
#'@importFrom ggplot2 element_rect
#'@importFrom ggplot2 element_text
#'@importFrom ggplot2 geom_line
#'@importFrom ggplot2 geom_text
#'@importFrom ggplot2 labs
#'@importFrom ggplot2 scale_colour_manual
#'@importFrom ggplot2 scale_y_continuous
#'@importFrom ggplot2 theme
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Cohen, G. (2015). The Bible of Options Strategies (2nd ed.). Pearson Technology Group.\cr
#'Kakushadze, Z., & Serur, J. A. (2018, August 17). 151 Trading Strategies. Palgrave Macmillan. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3247865 \cr
#'R Graphics Cookbook. (n.d.). Coloring Negative and Positive Bars Differently. https://r-graphics.org/recipe-bar-graph-color-neg \cr
#'Gross C, Ottolinger P (2016)._ggThemeAssist: Add-in to Customize 'ggplot2' Themes_. R package version 0.1.5, <URL: https://CRAN.R-project.org/package=ggThemeAssist>.
#'@examples
#'shortIronCondor(405,400,410,420,430,8,11,13,9,hl=0.95,hu=1.1)
#'@export
shortIronCondor <- function (ST,X1L,X2,X3,X4H,P1L,P2,C3,C4H,hl=0,hu=2,spot=spot,pl=pl,myData=myData,myTibble=myTibble,PnL=PnL){
  V0Dr= P2+C3 -P1L -C4H
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- (pmax((X2-myData$spot),0)) + (pmax((myData$spot-X3),0)) -   (pmax((X1L-myData$spot),0))- (pmax((myData$spot-X4H),0)) - V0Dr
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  myTibble <- as_tibble(myData)
  myTbColored <- myTibble %>% mutate(PnL = pl >= 0)
  ggplot(myTbColored,aes(x=spot,y=pl,fill=PnL,label=pl)) +
    geom_col(position = "identity") +
    scale_fill_manual(values = c("lavenderblush3","#061C13"), guide= "none" ) +
    geom_point(aes(color=PnL), size=4)+
    scale_color_manual(values = c("rosybrown3","#197A51"), guide= "none" ) +
    geom_text(nudge_y = 0.7,size= 3.5, color="black")+
    theme(plot.caption = element_text(colour  =  'lightsteelblue3'))+
    theme(axis.ticks = element_line(size  =  1,colour = "red"))+
    theme(panel.grid.major = element_line(colour  =  'lavender',size  =  0.45))+
    theme(panel.grid.minor = element_line(colour  =  'thistle2',size  =  0.35))+
    theme(axis.title = element_text(colour  =  'midnightblue'))+
    theme(plot.title = element_text(colour  =  'brown3', vjust  =  1))+
    theme(panel.background = element_rect(fill  =  '#EFF6FF'))+
    theme(plot.background = element_rect(fill  =  '#E5F0FF', colour  =  'aquamarine4', linetype  =  'dashed'))+
    labs(title  =  'Short Iron Condor Strategy', x  =  'Spot Price ($) at Expiration', y  =  'PnL ($) at Expiration', subtitle  =  'High Volatility / Neutral Outlook', caption  =  'condorOptions / MaheshP Kumar')
}

