# :man_dancing: Boogie :man_dancing:

A (mostly) complete implementation of the [Boogie](https://github.com/boogie-org/boogie) grammar, with a parser and pretty printer.

This includes the asynchronous extensions introduced by CIVL.

## Status

- Parser: 24 / 708 failing files from Boogie test suite. 
- Parser Roundtrip (Weak, Parse error occurs) : 83 / 708 failures
- Parser Roundtrip (Strong, Equality failure) :  176 / 708 failures

Remaining parser failures several seem related to pre-processor directives of some kind (which are unsupported), several are intentional parse errors (it seems), and a few are legitimate issues with edge cases in the syntax. 

The pretty printer is implemented using the `pretty` crate, and has not yet been fully debugged, issues are expected related to parenthesization of expressions. 

The AST also needs cleaning up and refactoring, certain types should be extract or shared more universally (thinking specifically about the various handlings of name-type pairs).
