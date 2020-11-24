-- file: lastButOne.hs
-- returns the element before the last

lastButOne :: [a] -> a
lastButOne xs = if length xs > 2
    then lastButOne (tail xs)
    else head xs