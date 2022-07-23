# Bugs

-   Timespecs like HH:MM:SS are ignored
    -   I suspect that the `((irregex-search "(\\d\\d):(\\d\\d)(:(\\d\\d))" (car timespec))` on line 139 isn't matching...
-   When a directive is dropped due to an unparseable timespec, an error message should be printed

# Fixed

*   v2.1 When a timespec wasn't recognized, a directive with `(time 0)` was created.
    *   I now filter those directives out.
*   v2.1 When the countdown time is an hour or greater, the countdown bar is too wide for the screen, and updates create new lines
*   "take ten thousand seconds to ..." crashed
    *   Fix "within" range for `no1` to be 1..10 instead of 1..9
