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
