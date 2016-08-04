odds n = map (\x -> x*2 + 1) [0..n-1]


safetail x 	| null(x) == False	= tail(x)
		| otherwise		= []