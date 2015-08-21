## INTRODUCTION

 - format of talk (live coding, interruptions welcome, reasonably structured - I hope)
 - Assumptions (vague familiarity with scalaz; monads, functors etc)

Statement (SLIDE)
 >  "I want the compiler to help me check the correctness of my programs"

3 minutes

## EXCEPTIONS
  - JAVA EXAMPLE (talk about the fact that exceptions are part of the type signature of a method)
    - cannot add exceptions in subclasses
    - forced to handle checked exceptions
    - cannot handle *checked* exceptions which are not thrown within the `try/catch` block

5 minutes (8)

  - SCALA EXAMPLE
    - compiler does not help you
    - annotations are worse than useless - completely unverified by the compiler, easy to be out-of-sync
    - simple refactoring or upgrading can break a program silently. Worse still, it will only become apparent 
      that you have a runtime issue at precisely the instant you were trying to handle a problem 
      (reference Mostly Harmless & the Grebulons)

5 minutes (13)

## CONCLUSION 1
   - "We cannot use exceptions to handle failure" (SLIDE)
   - so what *can* we use?

## Scalaz and \/
  - `disjunction1`. Cannot use `try/catch`. Must catch all throwables/non-fatals (5 minutes, 18) [disjunction1](src/main/scala/oxbow/part1/exceptions/disjunction1/ScalazDisjunction1a)
  - `disjunction2`. Using the methods `map/flatMap/getOrElse/orElse` etc for control flow. Should be very simple (similarly with how we program with Option) (5 minutes, 23)
                                                                                                                                                                           
  - `disjunction3`. A "real world example" - it's pretty easy to forget to use `\/.fromTryCatch` for Java calls. You have to be rigorous (5 minutes, 28)  

## State
  - As we start to write more complicated programs, it becomes necessary for us to interleave State transitions, calculations with failures
    - `state1` Simple modification of the program (3 minutes, 31)

(maybe leave out `state2`)

  - We're going to start looking later at state transitions which themselves may fail

## CONCLUSION 2
  - It's pretty easy to replace exceptions with disjunction, but I want to think a bit more about how we deal with failures and the sort of functions we are starting to write
     * The functions are able to describe by their type signature, what they can do
     * The functions cannot have non-local effects (DIGRESSION HERE ABOUT ferret AND THE DIFFICULTY OF WORKING WITH CODE USING global mutable state)              

5 minutes (36)

## Reader
  - We have hijacked the return type of our functions to describe only one aspect of that function's interaction with the real world
 - there are other aspects (State being one, reading from config being another)

 - We can embed these concerns in the very type of our program (`part4.reader`) (5 minutes, 41)
  * We create a type plus some type constructors, based on a single base-combinator (5 minutes, 46)

The strength of our program is now in how it is composed of small pieces that can all be individually-reasoned about
Refactoring and modification becomes easy because the pieces cannot affect global state except via their order
But what about IO? This is just global state. We cannot have referential transparency in order to pull apart and re-combine our programs if they mutate state.

5 minutes (51)

Let's take control of that (`part5.readert`) (3 minutes, 54)

## RWST  
  - Let's just throw the whole kitchen sink in and admit that our program is very possibly dealing with state as well   
    * I tend to use `Unit` as `W` because accumulation of log messages is both unrealistic and annoying (when they are not interleaved with those of imperative APIs) (`part6.rwst`) (5 minutes, 61)

  - The previous example has not really used State, so I want to think a little bit about how you might manage state in some  long-running program with a given set of interactions. 
  One mechanism I use is to insert the global state into an atomic reference and apply state transitions to it (`part7.atomic`) (9 minutes, 70)
  
  - We ....
    * construct a simple `testAndSet` (2 minutes)
    * extend our testAndSet to a simple State transition (with optional return) (2 minutes)
    * extend this to StateT in order that we can run our actions inside IO (5 minutes)

But we haven't considered exceptions in this (`part8.outro`) (might gloss over the detail of pulling the StateT out of a RWST, suffice to say it's `state`)

## CONCLUSION 3
  - We can write our programs using `EitherT[RWST]` and embed failure-handling, state transitions and configuration
  - We can use interactions written as state transitions to run across actual mutable state held safely in an atomic reference

But: this is an application. What does a library look like? How would you, for example, write an IO utility without knowing 
the shape of the use case? (`part9.IOUtil`) (6 minutes, 76)

## Putting it all together
  - There's a lot of boilerplate for any program. How do we get rid of that? (`part999.Program`) (3 minutes, 79)
  - Let's pull out our little CSV class into a library (`part999.Csv`) (3 minutes, 82)
  - Our actual program now becomes quite simple and isolated
