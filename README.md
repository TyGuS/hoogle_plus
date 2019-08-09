# hoogle_plus
Type-driven, component based synthesis, showcasing TYpe Guided Abstract Refinement (TYGAR).

## build
To build this project, you need to have z3-4.7.1.

## usage
Execute in the `hoogle_plus` directory:
```
stack exec -- hplus generate --preset icfptotal
stack exec -- hplus [DESIRED TYPE] [OPTIONAL ARGS]
```

## Example:
`stack exec -- hplus generate -p base -m "Data.Maybe"` to generate the componenet set
`stack exec -- hplus "Maybe a -> Pair a b -> Pair a b"`. Then you will get a solution:

`SOLUTION: (,) (Data.Maybe.fromMaybe (fst arg0) arg1) (snd arg0)`


## Artifacts
You may run any of these with `stack exec -- <artifactname>`:
- `hplus` : This is the CLI for running single queries
- `evaluation`: For re-running the evaluation script and getting a sense of the overall performance.
- `webapp`: Hosts a simple web interface at localhost:3000

## Sample genererate:
You need to generate the component library that's used for synthesis.

For the components that are only total functions used in the ICFP submission use:
```
stack exec -- hplus generate --preset icfptotal
```

For the superset of components to ICFP total that includes partial functions like `head`:
```
stack exec -- hplus generate --preset icfppartial
```

If you have your own file(s) you want to use, you may specify them. You will then use all the modules within the files. At this time you may not filter within the file:
```
stack exec -- hplus generate -f <your-file-here>
```

Of course, you can specify the exact packages (from hackage) and modules you want to include:
```
stack exec -- hplus generate -p base  -p bytestring -m "Data.Word" -m "Data.Int" -m "Data.Maybe" -m "Data.ByteString.Builder"       -m "Data.ByteString.Lazy" -m "Data.List" -m "Data.Tuple" -m "GHC.List" -m "GHC.Char" -m "Data.Bool"  -m "Text.Show"
```

## Docker image:
First run `docker pull aaron069/hoogle-plus:v5`
Then go to your hoogle_plus repo, run `docker run -v ./:/home/hoogle-plus -it aaron069/hoogle-plus:v5`
If you want to redirect port on localhost:3000, add this flag: `-p 3000:3000` on above command.

