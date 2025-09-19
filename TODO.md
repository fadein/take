# Bugs

-   When a directive is dropped due to an unparseable timespec, a warning message should be printed
-   Because of the status of the word "budget", I cannot specify tasks that include it.
    -   After the 1st budget is detected, return the rest of argv unchanged

# Feature wish list

-   Save lines of text piped in on STDIN to display on screen above the countdown timer
    -   In this mode read keystrokes directly from the TTY instead of STDIN
-   Support command strings of the form `take until 3pm to ...`
    -   Decide how this should work with budgeting
