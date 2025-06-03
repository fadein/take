# Bugs

-   When a directive is dropped due to an unparseable timespec, a warning message should be printed
-   Because of the status of the word "budget", I cannot specify tasks that include it.
    -   After the 1st budget is detected, return the rest of argv unchanged

# Feature wish list

-   `take until 3pm to ...`

# Fixed

*   v2.1 When a timespec wasn't recognized, a directive with `(time 0)` was created.
    *   I now filter those directives out.
*   v2.1 When the countdown time is an hour or greater, the countdown bar is too wide for the screen, and updates create new lines
*   v2.3 "take ten thousand seconds to ..." crashed
    *   Fix "within" range for `no1` to be 1..10 instead of 1..9
*   v2.4 fixed
    *   "take thousand seconds to ..." tallied up to 0 seconds
    *   "take X seconds" w/o an action starting with "to" crashed
*   v2.4.1 fixed
    *   Timespecs like HH:MM:SS are ignored
*   v2.5
    *   Package as an egg
*   v2.8
    *   Timer can be paused
*   v3.0 enhancements
    *   Support proportional (percentage) timespecs
*   v3.1
    *   Proportional timespecs were taken out of the complete budget; now they are figured from the remaining time *after* fixed timespecs are taken out
