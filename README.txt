cl-stocks is a project to test various stock purchasing strategies.

SCENARIOS
* Start-Investment      : Start with 10K, that's all you have to work with.
* Monthly-Investment    : Start with 10K, invest 2K monthly. Watch constantly.
* Monthly-Invest-Advice : Start with 10K, invest 2K monthly. Only make changes
                          (buy, sell) to your portfolio on investment day.
* Quarterly-Investment  : Start with 10K, invest 6K quarterly.
* Yearly-Investment     : Start with 10K, invest 24K yearly.


STRATEGIES
Buy And Sell (All or None)
* In-The-Bank           : Screw the market, go banking savings account.
* Buy-When-Possible     : Buy whenever you get the money.
* Random-Advice         : Typical stock advisor / home investor.
* Thirty-Percent-Rule   : When a stock dips below the 30% mark of the range
                          between it's 52-week high and 52-week low, BUY.
                          When a stock goes above the 60% mark of the range
                          between it's 52-week high and 52-week low, SELL.
                          Only sell at a price higher than purchase price.

Buy And Sell (Quantity Rule-Based)
* Automatic-Investment  : Automatic Investment Management (**Incomplete**)

Monthly Rule-Based Dollar Cost Averaging Strategies
* Moneypaper-Invest     : Invest% formula from Moneypaper (52 week high/low).
* Drip-Invest-Calc      : Drip Invest Calculator (Simple Moving Average).
* Twinvest              : Periodic purchasing system from AIM inventor.


HOW TO RUN
(load "testing.lisp")
(run-tests)


SAMPLE OUTPUT (GE from 1990/01/01 to 2011/12/31)
Scenario: START-INVESTMENT          Strategy: IN-THE-BANK               Result:   12451.87
Scenario: START-INVESTMENT          Strategy: BUY-WHEN-POSSIBLE         Result:   48028.80
Scenario: START-INVESTMENT          Strategy: RANDOM-ADVICE             Result:   38120.87
Scenario: START-INVESTMENT          Strategy: THIRTY-PERCENT-RULE       Result:   24711.68
Scenario: START-INVESTMENT          Strategy: AUTOMATIC-INVESTMENT      Result:   25328.41

Scenario: MONTHLY-INVESTMENT        Strategy: IN-THE-BANK               Result:  600567.60
Scenario: MONTHLY-INVESTMENT        Strategy: BUY-WHEN-POSSIBLE         Result:  963009.80
Scenario: MONTHLY-INVESTMENT        Strategy: RANDOM-ADVICE             Result:  803329.90
Scenario: MONTHLY-INVESTMENT        Strategy: THIRTY-PERCENT-RULE       Result:  700835.20
Scenario: MONTHLY-INVESTMENT        Strategy: AUTOMATIC-INVESTMENT      Result:  579217.10

Scenario: MONTHLY-INVEST-ADVICE     Strategy: IN-THE-BANK               Result:  600567.60
Scenario: MONTHLY-INVEST-ADVICE     Strategy: BUY-WHEN-POSSIBLE         Result:  956457.90
Scenario: MONTHLY-INVEST-ADVICE     Strategy: RANDOM-ADVICE             Result:  891539.80
Scenario: MONTHLY-INVEST-ADVICE     Strategy: THIRTY-PERCENT-RULE       Result:  561717.10
Scenario: MONTHLY-INVEST-ADVICE     Strategy: AUTOMATIC-INVESTMENT      Result:  709038.20

Scenario: QUARTERLY-INVESTMENT      Strategy: IN-THE-BANK               Result:  596078.94
Scenario: QUARTERLY-INVESTMENT      Strategy: BUY-WHEN-POSSIBLE         Result:  946115.44
Scenario: QUARTERLY-INVESTMENT      Strategy: RANDOM-ADVICE             Result:  866814.94
Scenario: QUARTERLY-INVESTMENT      Strategy: THIRTY-PERCENT-RULE       Result:  688940.90
Scenario: QUARTERLY-INVESTMENT      Strategy: AUTOMATIC-INVESTMENT      Result:  572229.94

Scenario: YEARLY-INVESTMENT         Strategy: IN-THE-BANK               Result:  575864.56
Scenario: YEARLY-INVESTMENT         Strategy: BUY-WHEN-POSSIBLE         Result:  869020.30
Scenario: YEARLY-INVESTMENT         Strategy: RANDOM-ADVICE             Result:  814488.06
Scenario: YEARLY-INVESTMENT         Strategy: THIRTY-PERCENT-RULE       Result:  659378.60
Scenario: YEARLY-INVESTMENT         Strategy: AUTOMATIC-INVESTMENT      Result:  550617.30

Scenario: MONTHLY-INVEST-RULE       Strategy: MONEYPAPER-INVEST         Result:  939748.60
Scenario: MONTHLY-INVEST-RULE       Strategy: DRIP-INVEST-CALC          Result:  989538.90
Scenario: MONTHLY-INVEST-RULE       Strategy: TWINVEST                  Result:  936773.80


cl-stocks is licensed under the ISC licence; see LICENSE.txt for details.
