cl-stocks is a project to test various stock purchasing strategies.

SCENARIOS
* Automatic-Investment  : Start with 10K, Automatic Investment Management

* Start-Investment      : Start with 10K.
* Monthly-Investment    : Start with 10K, invest 2K monthly.
* Quarterly-Investment  : Start with 10K, invest 6K quarterly.
* Yearly-Investment     : Start with 10K, invest 24K yearly.


STRATEGIES
Buy And Sell (All or None)
* In-The-Bank           : Screw the market, go banking savings account.
* Buy-When-Possible     : Buy whenever you get the money.
* Thirty-Percent-Rule   : When a stock dips below the 30% mark of the range
                          between it's 52-week high and 52-week low, BUY.
                          When a stock goes above the 60% mark of the range
                          between it's 52-week high and 52-week low, SELL.
                          Only sell at a price higher than purchase price.
* Dividends-Dont-Lie    : When yield for current year is above 90% of the
                          high yield of the previous year, SELL.
                          When yield for current year is below 110% of the
                          low yeild of the previous year, BUY.
                          * Should probably be using last decade average,
                            not last year's values.

Purchasing Dollar Cost Averaging Strategies
* Moneypaper-Invest     : Invest% formula from Moneypaper (52 week high/low).
* Drip-Invest-Calc      : Drip Invest Calculator (Simple Moving Average).
* Twinvest              : Periodic purchasing system from AIM inventor.


HOW TO RUN
sbcl --load "testing" --eval "(run-tests)" --eval "(quit)"


SAMPLE OUTPUT (K from 1990/01/01 to 2011/12/31)
Scenario: AIM-SCENARIO          Strategy: NIL                 Result:   32861.46

Scenario: START-INVESTMENT      Strategy: IN-THE-BANK         Result:   12451.93
Scenario: START-INVESTMENT      Strategy: BUY-WHEN-POSSIBLE   Result:   48279.52
Scenario: START-INVESTMENT      Strategy: THIRTY-PERCENT-RULE Result:   41905.09
Scenario: START-INVESTMENT      Strategy: MONEYPAPER-INVEST   Result:   48279.93
Scenario: START-INVESTMENT      Strategy: DRIP-INVEST-CALC    Result:   48279.52
Scenario: START-INVESTMENT      Strategy: TWINVEST            Result:   41899.09

Scenario: MONTHLY-INVESTMENT    Strategy: IN-THE-BANK         Result:  600564.40
Scenario: MONTHLY-INVESTMENT    Strategy: BUY-WHEN-POSSIBLE   Result: 1113175.10
Scenario: MONTHLY-INVESTMENT    Strategy: THIRTY-PERCENT-RULE Result: 1128806.40
Scenario: MONTHLY-INVESTMENT    Strategy: MONEYPAPER-INVEST   Result: 1114670.80
Scenario: MONTHLY-INVESTMENT    Strategy: DRIP-INVEST-CALC    Result: 1117426.40
Scenario: MONTHLY-INVESTMENT    Strategy: TWINVEST            Result: 1110954.60

Scenario: QUARTERLY-INVESTMENT  Strategy: IN-THE-BANK         Result:  596077.80
Scenario: QUARTERLY-INVESTMENT  Strategy: BUY-WHEN-POSSIBLE   Result: 1098250.10
Scenario: QUARTERLY-INVESTMENT  Strategy: THIRTY-PERCENT-RULE Result: 1109154.00
Scenario: QUARTERLY-INVESTMENT  Strategy: MONEYPAPER-INVEST   Result: 1098463.00
Scenario: QUARTERLY-INVESTMENT  Strategy: DRIP-INVEST-CALC    Result: 1096763.50
Scenario: QUARTERLY-INVESTMENT  Strategy: TWINVEST            Result: 1093396.10

Scenario: YEARLY-INVESTMENT     Strategy: IN-THE-BANK         Result:  575862.30
Scenario: YEARLY-INVESTMENT     Strategy: BUY-WHEN-POSSIBLE   Result: 1052243.80
Scenario: YEARLY-INVESTMENT     Strategy: THIRTY-PERCENT-RULE Result: 1064209.00
Scenario: YEARLY-INVESTMENT     Strategy: MONEYPAPER-INVEST   Result: 1052498.90
Scenario: YEARLY-INVESTMENT     Strategy: DRIP-INVEST-CALC    Result: 1055256.10
Scenario: YEARLY-INVESTMENT     Strategy: TWINVEST            Result: 1049532.50


cl-stocks is licensed under the ISC licence; see LICENSE.txt for details.
