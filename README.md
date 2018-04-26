# PCF Interpreter

                                            /\
                                           /..\
                                          /....\
                                         /------\
                                        /\@@@@@@/\
                                       /..\@@@@/..\
                                      /....\@@/....\
                                     /--------------\
                                    /\@@@@@@@@@@@@@@/\
                                   /..\@@@@@@@@@@@@/..\
                                  /....\@@@@@@@@@@/....\
                                 /------\@@@@@@@@/------\
                                /\@@@@@@/\@@@@@@/\@@@@@@/\
                               /..\@@@@/..\@@@@/..\@@@@/..\
                              /....\@@/....\@@/....\@@/....\
                             /\~~~~/oo\~~~~/oo\~~~~/oo\~~~~/\
                            /..\~~/oooo\~~/oooo\~~/oooo\~~/..\
                           /....\/oooooo\/oooooo\/oooooo\/....\
                          /------\------/oooooooo\------/------\
                         /\@@@@@@/\~~~~/oooooooooo\~~~~/\@@@@@@/\
                        /..\@@@@/..\~~/oooooooooooo\~~/..\@@@@/..\
                       /....\@@/....\/oooooooooooooo\/....\@@/....\
                      /--------------\--------------/--------------\
                     /\@@@@@@@@@@@@@@/\~~~~/oo\~~~~/\@@@@@@@@@@@@@@/\
                    /..\@@@@@@@@@@@@/..\~~/oooo\~~/..\@@@@@@@@@@@@/..\
                   /....\@@@@@@@@@@/....\/oooooo\/....\@@@@@@@@@@/....\
                  /------\@@@@@@@@/------\------/------\@@@@@@@@/------\
                 /\@@@@@@/\@@@@@@/\@@@@@@/\~~~~/\@@@@@@/\@@@@@@/\@@@@@@/\
                /..\@@@@/..\@@@@/..\@@@@/..\~~/..\@@@@/..\@@@@/..\@@@@/..\
               /....\@@/....\@@/....\@@/....\/....\@@/....\@@/....\@@/....\
              --------------------------------------------------------------


This repository contains two implementations of PCF as described in Robert Harper's
Practical Foundations for Programming Languages. The first one parses approximately
the grammar given in the book to a quoted list representation which is then converted
to higher-order abstract syntax. The second generates a HOAS representation directly
using macros. Both versions can be evaluated using the dynamics defined in pcf-dynamics.rkt.

See the tests for usage examples.