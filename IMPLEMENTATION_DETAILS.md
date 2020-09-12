# Solution description

## File structure

source code is in the `src` folder

- `Interpreter.hs` - this file executes the:
    1. parser
    2. static analyser
    3. interpreter
- `CommonDeclarations.hs` - declaration of helper functions and variables
- `Expressions.hs` - code responsible for evaluating expressions
- `Statements.hs` - code responsible for executing statements
- `TypeChecker.hs` - code responsible for static analysis and type checking

## Wykonanie instrukcji

Because of jump instructions
(return, break, continue), the function executing statements
is separated into two parts: `exec` function, which checks
if the next statement should be executed
and `internalExec` function, executing the actual statement.

## Variables, functions, environment

To implement static variable visibility, the **interpretation environment**
is split into two pars: `Environment` and `Store`.
Additionally, apart from variables and functions, the environment stores information
about return values (`ReturnParameter`)
and flags enabling `break`s  and `continue`s to be executed properly.
(`BreakParameter` and `ContinueParameter` respectively).

**Type checking environment** is similar to the interpretation environment.
The difference is that it holds types instead of actual values.
Additionally, there is no need to hold any information about `break`s and
`continue`s (outside of loops they are ignored).

## Krotki i tablice

Tuple and array values are held in `Data.Vector` instead of lists.
This allows constant time access.

## Examples

Correct program examples are in the `good` folder.
Incorrect program examples are in the `bad` folder.
