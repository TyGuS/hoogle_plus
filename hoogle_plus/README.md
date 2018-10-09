# hoogle_plus
Better than HoogleBook

## usage
Execute in the `hoogle_plus` directory:
```
stack setup && stack build
stack exec -- synquid generate -p "bytestring"
stack exec -- synquid synthesis [DESIRED TYPE] --graph --succinct [OPTIONAL ARGS]
```