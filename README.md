# Haskell Interpreter
## Ciaran Costello - 13321463

### Use
* Running the main will cause the interpreter to run with the program.txt file in the test folder.
* Instructions:
	* y - executes the next statement
	* i - inspect the program variables contents and history
	* h - inspect the executed statements ie. History
	* b - move back throught the history by one statement

### Part 1
*Turn your project into a 'stack' project so that it's easy for me to build and run it when you submit. Add a Readme file (it can be plain text or markdown formatted, your choice). When you submit the project this file should (a) describe how to use the final interpreter, and (b) for each of the numbered sections of the project there should be a section of the readme describing how much of the section you have completed.*

Completed.

### Part 2
*Provide a monadic interpreter for the language that allows us to step through a program. Your program must read a source file and execute the program contained in it line by line. You can use automatically derived read instances to do the parsing, and no special error messages are needed if a parse fails. Running the program should execute the first statement, and then prompt the user to run the next statement. *

Provides an option to execute the next statement before each statement (apart from the first statement, not including seq). Entering **y** moves the interpreter on to the next statement.
Completed.

### Part 3
*Extend the interpreter so that there is a new Inspect command that the user can issue to examine the contents of the programs variables (you can give the choice by having the user type in commands, or present a menu of options, the UI of the interpreter is up to you).*

By typing **i** instead of y you can inspect the contents of the program variables.

### Part 4
*Augment the monadic interpreter so that it records the history of each assignment - that is, each variable should have not only it's most recent value in the variable store, but also all of the previous values that the variable contained. Inspecting the variable during interpretation should show the full history.*

Completed.

### Part 5
*Now add a "step backwards" command to the interpreter that allows the user to move the program back through statements. Use the variable history to restore the previous state of any variables as the program steps back through any assignment statements.*

By entering **h** you can view the history of executed Statements. Entering **b** causes the interpreter to move back through these statement by 1. 

To allow it to move backwards over flow control, like if statements, the actual executions of these statements is recorded instead of the statements themselves. This means that the implementation for While was changed to produce the sequential execution of the inner statement and then the while loop itself again.

Complete.

### Part 6
*Finally, you can add some static analysis to the interpreter to report on errors before the program is run. Add one of the following, or something you decide on yourself (note: this section is potentially very time consuming if you decide to do it very thoroughly -- note how few marks are available for it, and adjust your effort accordingly!)*

Incomplete.