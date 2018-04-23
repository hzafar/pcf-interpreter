# PCF Interpreter Reference

## ~~ Needs updating ~~
Also, things are broken as I clean this up

                             /\
                            /..\
                           /....\
                          --------
                         /\@@@@@@/\
                        /..\@@@@/..\
                       /....\@@/....\
                      ----------------
                     /\@@@@@@@@@@@@@@/\
                    /..\@@@@@@@@@@@@/..\
                   /....\@@@@@@@@@@/....\
                  --------@@@@@@@@--------
                 /\@@@@@@/\@@@@@@/\@@@@@@/\
                /..\@@@@/..\@@@@/..\@@@@/..\
               /....\@@/....\@@/....\@@/....\
              --------------------------------

A Racket-based implementation of the PCF dynamics described in Robert Harper's *Practical Foundations for Programming Languages*.

### PCF Constructs

`z` - The value 0.

`(succ e)` - Take the successor of `e`.

`(ifz e e0 e1)` - If `e` is 0, produce `e0`. Otherwise, `e` is of the form `(succ e')`, so produce `e1(e')`.

`(lam x e)` - A lambda with parameter `x` and body `e`.

`(ap e1 e2)` - Apply `e1` to the argument `e2`.

`(fix e)` - Produce the recursive definition that is the fixed point of `e`.

### SPCF Constructs

As for PCF above, with the following:

`(new s e)` - Declare a new symbol `s` within the body of `e`. In the scoped dynamics, `s` is dropped from the environment once `e` is evaluated. In the scope-free dynamics, `s` may be referenced from outside `e`.

`(quoted s)` - Produce a symbol reference to `s`.

`(if-sym s e e1 e2)` - If `e` is a reference to the symbol `s`, produce `e1`. Otherwise, produce `e2`.

### LPCF Constructs

As for PCF above, but with lazy evaluation dynamics.

-----

For each language, there is a `step` function that performs one reduction step, and an `evaluate` function which reduces an expression (if possible) all the way down to a value. See the tests for concrete examples.