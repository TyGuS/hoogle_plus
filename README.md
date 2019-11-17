# hoogle+
Type-driven, component based synthesis, showcasing TYpe Guided Abstract Refinement (TYGAR).

# Using Evaluation Docker Image
## System Prerequisites:
- Docker
- Latex (for compiling completed evaluation results)

## Evaluation Results
H+ will rerun its evaluation to produce five files:
- `table.tex` and `table_results.tex` These are the latex files to produce the table seen in the evaluation section of the paper.
You will need use your system's latex installation `pdflatex table.tex` to combine them.
- `major_variants.pdf` This is a plot of the 4 major search strategies (TYGARQ, TYGARQB10, TYGAR0, NOGAR)
- `bounded_variants.pdf` This is a plot of the 4 bounded abstraction searches (TYGARQB5, 10, 15, 20)
- `quality.csv` This is a CSV of the top 5 solutions for each benchmark produced within 120 seconds.
It is easiest to view this file by importing it into a Google Sheets spreadsheet.

There is also an executable you may interact with, `hplus`.

## Re-running the evaluation
First we must build a docker image:
1. Build it with `docker build --tag hoogleplus_aec:latest .` (This can take between 40 minutes and 2 hours)
2. Run the docker file interactively with a desired output directory.
Hoogle+ will put the evaluation results into that directory.
```
docker run -v /absolute/path/to/output/dir:/output -it hoogleplus_aec:latest /bin/bash
```
3. Now navigate to the internal hoogle plus directory: `cd /home/hoogle_plus`
4. Run the evaluation script: `./evaluation.sh` (This can take about 150 minutes). Don't feel like waiting that long? Use `./evaluation-short.sh`. This should take ~5 minutes. It relies on a smaller set of benchmarks, only 3. You may wish to modify `benchmark/suites/aec-short.yml` adding more in as you like from the master set, located at `benchmark/suites/working.yml`.

At this point, you should have 5 new files in your output directory.
These are the results of the evaluation.

## Usage
```
stack exec -- hplus "Maybe a -> [a] -> a"
```
Replace the type query with whatever your heart fancies.
The default search mode is `TYGARAQ` as described in the paper: unbounded abstraction refinement.

You may try searches with different variants with the following command line args:
- TYGARQ: Unbounded abstraction refinement. This is the default mode. The initial abstract cover are the types in the query.
- TYGARQ0: Unbounded abstraction refinmenet. The initial abstract cover is empty. Use `stack exec -- hplus --use-refine=tygar0 "<query>"`
- NOGAR: No refinement. Use `stack exec -- hplus --use-refine=nogar "<query>"`
- TYGARQB: Bounded abstraction refinement. Use `stack exec -- hplus --stop-refine=True --stop-threshold=10 "<query>"`. Replace `10` with any number. This is the maximum refinements HooglePlus will make.


# Building from scratch, for the developers
## build
To build this project, you need to have z3-4.7.1.
You will need to ensure that the z3 library is in your `LD_LIBRARY_PATH` variable.

## usage
Execute in the `hoogle_plus` directory:
```
stack exec -- hplus generate --preset icfptotal
stack exec -- hplus [OPTIONAL ARGS] [DESIRED TYPE]
```

## Example:
`stack exec -- hplus generate -p base -m "Data.Maybe"` to generate the componenet set
`stack exec -- hplus "Maybe a -> Pair a b -> Pair a b"`. Then you will get a solution:

`SOLUTION: (,) (Data.Maybe.fromMaybe (fst arg0) arg1) (snd arg0)`


## Artifacts
You may run any of these with `stack exec -- <artifactname>`:
- `hplus` : This is the CLI for running single queries
- `webapp`: Hosts a simple web interface at localhost:3000

## Database generation (before running any queries):
You need to generate the component library that's used for synthesis.

For the full set of components, which include partial functions like `head`:
```
stack exec -- hplus generate --preset popl2020
```

For the components that are only total functions use:
```
stack exec -- hplus generate --preset icfptotal
```



If you have your own file(s) you want to use, you may specify them. You will then use all the modules within the files. At this time you may not filter within the file:
```
stack exec -- hplus generate -f <your-file-here>
```

Of course, you can specify the exact packages (from hackage) and modules you want to include:
```
stack exec -- hplus generate -p base  -p bytestring -m "Data.Word" -m "Data.Int" -m "Data.Maybe" -m "Data.ByteString.Builder"       -m "Data.ByteString.Lazy" -m "Data.List" -m "Data.Tuple" -m "GHC.List" -m "GHC.Char" -m "Data.Bool"  -m "Text.Show"
```

## Another docker image:
First run `docker pull aaron069/hoogle-plus:v2`
Then go to your hoogle_plus repo, run `docker run -v ./:/home/hoogle-plus -it aaron069/hoogle-plus:v2`
If you want to redirect port on localhost:3000, add this flag: `-p 3000:3000` on above command.
