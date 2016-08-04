inx :: Int -> [(Int, Int, Int)]
inx x = [x| x <- x, y <- x-1, z <- x-2] 