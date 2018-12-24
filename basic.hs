-- lista por comprension
addDouble::[Int]->Int
addDouble l=sum[2*x|x<-l]

--lista con descripcion cabeza y cola
addDouble'::[Int]->Int
addDouble' []=0
addDouble' (x:xs)=2*x+addDouble' xs

-- creando tipos

data Animal=Cat|Dog
greet::Animal->String
greet Cat="Meeow"
greet Dog="Woof"

type Zoo=[Animal]

greetAll::Zoo->String
greetAll z=concatMap greet z

main=putStrLn "Hello, world"

