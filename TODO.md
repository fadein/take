# Bugs

-   When a directive is dropped due to an unparseable timespec, an error message should be printed

# Features

-   `take until 3pm to ...`

# Fixed

*   v2.1 When a timespec wasn't recognized, a directive with `(time 0)` was created.
    *   I now filter those directives out.
*   v2.1 When the countdown time is an hour or greater, the countdown bar is too wide for the screen, and updates create new lines
*   v2.3 "take ten thousand seconds to ..." crashed
    *   Fix "within" range for `no1` to be 1..10 instead of 1..9
*   v2.4 fixes
    *   "take thousand seconds to ..." tallied up to 0 seconds
    *   "take X seconds" w/o an action starting with "to" crashed
*   v2.4.1 fixes
    *   Timespecs like HH:MM:SS are ignored
*   v2.5
    *   Package as an egg
*   v2.8
    *   Timer can be paused
