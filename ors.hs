ors             :: Bool -> Bool -> Bool
ors True True   = True
ors _ True      = True
ors _ _         = False