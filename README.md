# :man_dancing: Boogie :man_dancing:

A (not yet) complete implementation of the [Boogie](https://github.com/boogie-org/boogie) grammar, with a parser and pretty printer.

This includes the asynchronous extensions introduced by CIVL.

As of this latest commit, there are 24 / 708 failing input files (compared against the boogie test suite). Of these remaining failures several seem related to pre-processor directives of some kind (which are unsupported), several are intentional parse errors (it seems), and a few are legitimate issues with edge cases in the syntax. 

At this stage, while the parser still needs work, that work is mostly filling out the AST, cleaning the code and refactoring.

