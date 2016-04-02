# hanoi-types
Solve the Tower of Hanoi at the type level in Haskell

You know, in case you have a type that happens to be the initial state of the well-known game, and you really want to solve it...

Example:

```haskell
steps :: (t ~ Towers 5 0 0, history ~ Solve t, n ~ (Length history - 1))
         => Proxy '(history, n)
steps = Proxy
```
The type of `steps` will be a tuple consisting of the change history and the number of steps (i.e the length of the history - 1, which is 2^n - 1).

```
Prelude> :t steps
steps
  :: Proxy
       '(((((((((((((((((((((((((((((((('Nil ':> Towers 5 0 0)
                                       ':> Towers 4 0 1)
                                      ':> Towers 3 1 1)
                                     ':> Towers 3 2 0)
                                    ':> Towers 2 2 1)
                                   ':> Towers 3 1 1)
                                  ':> Towers 3 0 2)
                                 ':> Towers 2 0 3)
                                ':> Towers 1 1 3)
                               ':> Towers 1 2 2)
                              ':> Towers 2 2 1)
                             ':> Towers 3 1 1)
                            ':> Towers 3 2 0)
                           ':> Towers 2 2 1)
                          ':> Towers 1 3 1)
                         ':> Towers 1 4 0)
                        ':> Towers 0 4 1)
                       ':> Towers 1 3 1)
                      ':> Towers 1 2 2)
                     ':> Towers 0 2 3)
                    ':> Towers 1 1 3)
                   ':> Towers 1 2 2)
                  ':> Towers 2 2 1)
                 ':> Towers 3 1 1)
                ':> Towers 3 0 2)
               ':> Towers 2 0 3)
              ':> Towers 1 1 3)
             ':> Towers 1 2 2)
            ':> Towers 0 2 3)
           ':> Towers 1 1 3)
          ':> Towers 1 0 4)
         ':> Towers 0 0 5,
         31)
```
