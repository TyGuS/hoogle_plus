# hoogle_plus
Better than HoogleBook

## build
To build this project, you need to have z3-4.3.1 and gradle-4.10.2 installed

## usage
Execute in the `hoogle_plus` directory:
```
stack setup && stack build
stack exec -- synquid generate -p "bytestring"
stack exec -- synquid synthesis [DESIRED TYPE] --graph --succinct [OPTIONAL ARGS]
```
