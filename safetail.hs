-- Simple exercise to demonstrate the 3 main 
-- structures for building functions in Haskell.
-- The function performs the same actions as 'tail'
-- but won't return an error if an empty set is 
-- provided


safetailPattern		:: [a] -> [a]
safetailPattern (x:xs)	= xs 
safetailPattern []	= []
		  
		  		  
safetailGuard x | null(x) == False	= tail(x)
		| otherwise		= []	

			
safetailConditional 	:: [a] -> [a]
safetailConditional n 	= if null n == False then tail n else [] 		
			
			