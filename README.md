# hoogle_plus
Type-driven, component based synthesis, showcasing TYpe Guided Abstract Refinement (TYGAR).
Try it at [https://hplus.programming.systems](https://hplus.programming.systems)

# Using Evaluation Docker Image
## System Prerequisites:
- Docker

## Kick-the-Tires Test
First we must build a docker image:
1. Build it with `docker build --tag=hoogleplus:latest .` (This can take between 40 minutes and 2 hours)
2. Run the docker file interactively with a desired output directory.
Hoogle+ will put the evaluation results into that directory.
```
docker run -p 3000:3000 -p 5000:5000 -v /absolute/path/to/output/dir:/home/hoogle_plus/output -it hoogleplus:latest /bin/bash
```
3. Now navigate to the internal Hoogle+ directory: `cd /home/hoogle_plus`
4. Run the short evaluation script: `python3 scripts/run.py --small`. (This takes about 10 minutes)
If you don't encounter any error from using this script, you should be good to run the entire artifact.

## Evaluation Results
Hoogle+ will rerun its evaluation to produce three files corresponding to three
figures in the submitted paper:
- `inference-heatmap.png`: the heatmap graph in Fig 9 (Left)
- `inference-stats.tsv`: the table in Fig 9 (Right)
- `filtering.png`: the histogram graph in Fig 10
- `inference-user.png`: the histogram corresponding to front part of Section 5.1

All these files reside in `/home/hoogle_plus/output`.

## Re-running the entire evaluation
0. We assume you already have the docker container running
1. Navigate to the root Hoogle+ directory: `cd /home/hoogle_plus`
2. Run the evaluation script: `python3 scripts/run.py --oopsla --full` (This can take about 2-3 hours).

At this point, you should have four new files in your output directory. 
These are the results of the evaluation.

3. You may also run the evaluations separately:
- To run the evaluation for type inference on user-provided data, use `python3 scripts/run.py --type-inference --use-study-data`
- To run the evaluation for type inference on randomly generated data, use `python3 scripts/run.py --type-inference --full`
- To run the evaluation for candidate elimination, use `python3 scripts/run.py --filtering --full`

## Usage
```
stack exec -- hplus --json='{"query": "Eq a => [a] -> [a]", \
                             "inExamples": [{ "inputs": ["\"aaabbbab\""], "output": "\"abab\""}]}'
```
Replace the type query with whatever your heart fancies.

You may try different searche modes with the following command line args:
- Search by type only: you may leave the `inExamples` field an empty list in the input json file.
- Search by both type and examples: when you provide `inExamples`, they will be used to filter the generated candidates.
- Search by examples only: you may leave the `query` field empty and use `stack exec -- hplus --json='{"query": "Eq a => [a] -> [a]", "inExamples": [{ "inputs": ["\"aaabbbab\""], "output": "\"abab\""}]}' --search-type=searchtypes`. The results for this command will be a json string with the top 10 inferred types from your provided examples.
- Search with candidate elimination: use `stack exec -- hplus --json='YOUR JSON STR' --disable-filtering=False --cnt=5`. This will try to find the top 5 solutions that will not crash and none of them have the same behavior as others.

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
stack exec -- hplus --json='{"query": "mb: Maybe a -> p: (a, b) -> (a, b)", "inArgNames":[], \
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

## Running Hoogle+ in Docker

We provide a Dockerfile for running Hoogle+. To build the Docker image, navigate to the `hoogle_plus` repository and run:

```bash
$ docker build --tag hoogleplus:latest .
```

After the image is built successfully, run the container:

```bash
$ docker run -p 3000:3000 -p 5000:5000 -it hoogleplus:latest
```

## Running Hoogle+ with Nix (updated 7/14/2025)

Alternatively, Hoogle+ can be run using Nix; this method is preferred if you do not wish to install Haskell or Stack directly on your system. A recent installation of Nix (version 2.18.1 or higher) is the only prerequisite.

First, build a custom version of GHC, Hoogle+, and its dependencies. This process could take up to 1-3 hours depending on your setup. The following command builds these dependencies and enters a configured environment:

```bash
$ nix develop
```

Once in the development environment, generate the hoogle database:

```bash
$ hoogle generate
```

Then, you can run an example query following the instruction above:

```bash
$ hplus generate --preset partialfunctions
$ hplus --json='{"query":"mb: Maybe a -> p: (a, b) -> (a, b)","inExamples":[{"inputs":["Just 1","(2, 3)"],"output":"(1, 3)"}],"inArgNames":[]}'

...

RESULTS:{"outCandidates":[{"qualSolution":"\\mb p -> ((Data.Maybe.fromMaybe (fst p) mb) , (snd p))","outExamples":[{"inputs":["Just 1","(2, 3)"],"output":"(1, 3)"}],"unqualSolution":"\\mb p -> ((fromMaybe (fst p) mb) , (snd p))"}],"outDocs":[{"functionSig":"a -> Maybe a -> a","functionName":"fromMaybe","functionDesc":"The fromMaybe function takes a default value and a Maybe\nvalue. If the Maybe is Nothing, it returns the default\nvalue; otherwise, it returns the value contained in the Maybe.\n\nExamples\n\nBasic usage:\n\n\n>>> fromMaybe \"\" (Just \"Hello, World!\")\n\"Hello, World!\"\n\n\n\n>>> fromMaybe \"\" Nothing\n\"\"\n\n\nRead an integer from a string using readMaybe. If we fail to\nparse an integer, we want to return 0 by default:\n\n\n>>> import GHC.Internal.Text.Read ( readMaybe )\n\n>>> fromMaybe 0 (readMaybe \"5\")\n5\n\n>>> fromMaybe 0 (readMaybe \"\")\n0\n\n"},{"functionSig":"f a -> g a -> Product (f k -> Type) (g k -> Type) (a k)","functionName":"Pair","functionDesc":""},{"functionSig":"(a, b) -> a","functionName":"fst","functionDesc":"Extract the first component of a pair.\n"},{"functionSig":"(a, b) -> b","functionName":"snd","functionDesc":"Extract the second component of a pair.\n"},{"functionSig":"Maybe (a)","functionName":"mb","functionDesc":""},{"functionSig":"(a , b)","functionName":"p","functionDesc":""}],"outError":""}
```
