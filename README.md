# take

A simple command-line timer tool for time management, inspired by the Pomodoro Technique.

## Description

`take` is a command-line tool that allows you to set timers using natural language. You can specify durations in hours, minutes, and seconds, and chain multiple timers together to create a sequence of timed events. It also supports setting a total budget and then specifying timers as percentages of that budget.

#### Features

-   **Natural language time parsing:** Specify durations using words (e.g., "five minutes", "one and a half hours").
-   **Chained timers:** String multiple timers together using "then".
-   **Proportional timers:** Define a total time budget and then allocate time using percentages.
-   **Interactive controls:** Pause, skip, and adjust timers on the fly.
-   **Visual countdown:** A progress bar shows the time remaining.

## Usage

```
take [duration] to [task description] then [duration] to [task description] ...
```

### Examples

-   **Simple timer:**
    ```
    take 25 minutes to work on the report
    ```

-   **Chained timers:**
    ```
    take 25 minutes to work on the report then take 5 minutes to take a break
    ```

-   **Budget-based timers:**
    ```
    take a budget of 1 hour then take 50% to work on feature A then take 50% to work on feature B
    ```

-   **More complex natural language:**
    ```
    take two and a half hours to finish the prototype
    ```

## Controls

While a timer is running, you can use the following keys:

-   `p` or `space`: Pause/resume the timer.
-   `q` or `Ctrl+C`: Quit the program.
-   `+` or `=`: Add 5% to the current timer.
-   `-` or `_`: Subtract 5% from the current timer.
-   `r`: Restart the current timer.
-   `z` or `0`: Skip the current timer.

## Installation

This program is written in [Chicken Scheme](https://www.call-cc.org/) and can be installed with `chicken-install`.  Open a shell in this repository and run

```bash
chicken-install
```

Or, if you need administrator privileges:

```bash
chicken-install -s
```

## License

This project is licensed under the terms of the GPLv3 license.
