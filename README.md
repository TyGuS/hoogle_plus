# hoogle_plus
Type-driven, component based synthesis, showcasing TYpe Guided Abstract Refinement (TYGAR).
Try it at <https://hoogleplus.goto.ucsd.edu>

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
stack exec -- hplus --json='{"query": "a -> [Maybe a] -> a", "inExamples": [{"inputs": ["1", "[Nothing ,Just 3, Nothing]"], "output":"3"}]}'
```
Replace the type query and examples with whatever your heart fancies.
The input should follow the json format that
```json
{
    "query": str,
    "inExamples": [
        {
            "inputs": [str],
            "output": str
        }
    ]            
}
```
The default search mode is `TYGARAQB10` as described in the paper: bounded abstraction refinement of size 10.

The default argument names will be `arg0, 1, ...`.
You may specify your own argument names. For example, the previous type query can be written as `d: a -> xs: [Maybe a] -> a`.

You may try searches with different variants with the following command line args:
- TYGARQ: Unbounded abstraction refinement. This is the default mode. The initial abstract cover are the types in the query.
- TYGARQ0: Unbounded abstraction refinmenet. The initial abstract cover is empty. Use `stack exec -- hplus --use-refine=tygar0 "<query>"`
- NOGAR: No refinement. Use `stack exec -- hplus --use-refine=nogar "<query>"`
- TYGARQB: Bounded abstraction refinement. Use `stack exec -- hplus --stop-refine=True --stop-threshold=10 "<query>"`. Replace `10` with any number. This is the maximum refinements HooglePlus will make.


# Building from scratch, for the developers

## build
To build this project, you need to have z3-4.7.1.

## usage
Execute in the `hoogle_plus` directory:
```
stack exec -- hplus generate --preset partialfunctions
stack exec -- hplus [DESIRED QUERY] [OPTIONAL ARGS]
```

## Example:
`stack exec -- hplus generate -p base -m "Data.Maybe"` to generate the componenet set
`stack exec -- hplus --json='{"query": "Maybe a -> (a, b) -> (a, b)", "inExamples": []}'`. Then you will get a solution:

`SOLUTION: (,) (Data.Maybe.fromMaybe (fst arg0) arg1) (snd arg0)`


## Artifacts
You may run any of these with `stack exec -- <artifactname>`:
- `hplus` : This is the CLI for running single queries
- `new_webapp` and `web_server`: Hosts a simple web interface at localhost:3000

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


## Developing inside a Container

The project could be developed inside a docker container with [Visual Studio Code Remote][vscode-remote],
which has a configured Haskell development environment powered by HIE.
Configurations for the development environment can be found [here](/.devcontainer).

Simply clone and open the project with Visual Studio Code, then select _Reopen in Container_ in the pop-up menu.
After VSCode set up the pre-defined container, we may build the project and run its unit tests with
```bash
$ stack build && stack test
```

[vscode-remote]: <https://code.visualstudio.com/docs/remote/containers>


