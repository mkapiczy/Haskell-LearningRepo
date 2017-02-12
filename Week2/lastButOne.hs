lastButOne :: [a] -> a
lastButOne xs =
  if null xs
    then error "you can't enter an empty list."
    else if length xs == 1
      then head xs
        else if null (tail (tail xs))
          then head xs
          else lastButOne (tail xs)
