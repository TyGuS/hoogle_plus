# hoogle_plus
Better than HoogleBook

## build
To build this project, you need to have z3-4.7.1 and gradle-4.10.2 installed

## usage
Execute in the `hoogle_plus` directory:
```
./build.sh
stack exec -- synquid generate -p "base" -m Data.Maybe
stack exec -- synquid synthesis [DESIRED TYPE] --path=PetriNet [OPTIONAL ARGS]
```

Example:
`stack exec -- hplus generate -p base -m "Data.Maybe"` to generate the componenet set 
`stack exec -- hplus "Maybe a -> Pair a b -> Pair a b"`. Then you will get a solution:

`SOLUTION: (,) (Data.Maybe.fromMaybe (fst arg0) arg1) (snd arg0)`

## Docker image:
First run `docker pull aaron069/hoogle-plus:v2`
Then go to your hoogle_plus repo, run `docker run -v ./:/home/hoogle-plus -it aaron069/hoogle-plus:v2`
If you want to redirect port on localhost:3000, add this flag: `-p 3000:3000` on above command.

