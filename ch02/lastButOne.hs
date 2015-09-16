lastButOne :: [a] -> a
lastButOne xs = let len = length xs in if len == 2
                                       then head xs
                                       else if len < 2
                                            then error "lastButOne takes as its argument an array of at least 2 elements."
                                            else lastButOne (tail xs)
