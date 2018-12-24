factorial 0=1
factorial n=n*factorial(n-1)

main=do putStrLn "Cuanto es 5!"
	x<-readLn
	if x==factorial 5
		then putStrLn "good!"
		else putStrLn "bad!"
