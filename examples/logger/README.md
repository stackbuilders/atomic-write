# logger-example

This Haskell code example demonstrates a basic logging simulation.

It emulates a logging operation that sequentially generates a set of log messages. Within a loop, it prints messages to the console, ranging from "Log message 1" to "Log message 5". Each message is printed alongside a confirmation that it has been logged, with a 1-second delay between messages.

Upon reaching "Log message 5", the process shifts from console output to writing this message into a log file.


## How to use this example

### Required dependencies

- atomic-write
- text
- filepath


### Run Example

- Use the command: `stack run`
