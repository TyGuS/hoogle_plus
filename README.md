# hoogle_plus
Type-driven, component based synthesis, showcasing TYpe Guided Abstract Refinement (TYGAR).
Try it at [https://hplus.programming.systems](https://hplus.programming.systems)

# Using Evaluation Docker Image
## System Prerequisites:
- Docker

## Kick-the-Tires Test
First we must build a docker image:
1. Build it with `docker build --tag hoogleplus:latest .` (This can take between 40 minutes and 2 hours)
2. Run the docker file interactively with a desired output directory.
Hoogle+ will put the evaluation results into that directory.
```
docker run -p 3000:3000 -p 5000:5000 -v /absolute/path/to/output/dir:/home/hoogle_plus/output -it hoogleplus:latest /bin/bash
```
3. Now navigate to the internal Hoogle+ directory: `cd /home/hoogle_plus`
4. Run the short evaluation script: `python3 scripts/run_all.py --small`. (This takes about 10 minutes)
If you don't encounter any error from using this script, you should be good to run the entire artifact.

## Evaluation Results
Hoogle+ will rerun its evaluation to produce three files corresponding to three
figures in the submitted paper:
- `inference-heatmap.png`: the heatmap graph in Fig 9 (Left)
- `inference.tsv`: the table in Fig 9 (Right)
- `filtering.png`: the histogram graph in Fig 10

All of these files reside in `/home/hoogle_plus/output`.

## Re-running the entire evaluation
0. We assume you already have the docker container running
1. Navigate to the root Hoogle+ directory: `cd /home/hoogle_plus`
2. Run the evaluation script: `python3 scripts/run_all.py --all` (This can take about 150 minutes).

At this point, you should have three new files in your output directory.
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

## Build
To build this project, you need to have z3-4.7.1.

## Usage
Execute in the `hoogle_plus` directory:
```
stack exec -- hplus generate --preset partialfunctions
stack exec -- hplus --json='{"query": <DESIRED TYPE>, "inExamples": [<OPTIONAL EXAMPLES>]}' [OPTIONAL ARGS]
```

If you would like to provide examples for synthesis, examples are in the
following json format:
```
{
  "inputs": [str],
  "output": str
}
```

## Example Usages:
`stack exec -- hplus generate --preset partialfunctions` to generate the componenet set.
Then run
```
stack exec -- hplus --json='{"query": "mb: Maybe a -> p: (a, b) -> (a, b)", \
                             "inExamples": [{ \
                                 "inputs": ["Just 1", "(2, 3)"], \
                                 "output": "(1, 3)" \
                             }]}'
```
Wait for several seconds, you will get a solution:
`\mb p -> ((Data.Maybe.fromMaybe (fst p) mb), (snd p))`


## Artifacts
- A CLI for running single queries. You may run it with `stack exec -- hplus`
- A ReachJS web interface at `new_webapp`. You may run it with `yarn && REACT_DEVELOPMENT_ENV=hplusback.programming.system yarn start`,
the web interface will be hosted at `localhost:3000`

## Sample genererate:
You need to generate the component library that's used for synthesis.

Use our bigger set of components here:
```
stack exec -- hplus generate --preset partialfunctions
```

If you would just like to consider functions that are total (i.e., well defined
on all inputs), use:
```
stack exec -- hplus generate --preset totalfunctions
```


If you have your own file(s) you want to use, you may specify them.
You will then use all the modules within the files. At this time you may not filter within the file:
```
stack exec -- hplus generate -f <your-file-here>
```

Of course, you can specify the exact packages (from hackage) and modules you want to include:
```
stack exec -- hplus generate -p base  -p bytestring -m "Data.Word" -m "Data.Int" -m "Data.Maybe" -m "Data.ByteString.Builder" -m "Data.ByteString.Lazy" -m "Data.List" -m "Data.Tuple" -m "GHC.List" -m "GHC.Char" -m "Data.Bool"  -m "Text.Show"
```

## Docker image:
We have a Dockerfile configuration in the root directory.
First go to the hoogle_plus repo and run `docker build --tag hoogleplus:latest .` to build a docker image.
After the building finished, run `docker run -p 3000:3000 -p 5000:5000 -it hoogle-plus:latest`.

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


