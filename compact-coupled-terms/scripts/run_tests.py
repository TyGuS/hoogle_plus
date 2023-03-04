import argparse
import re
import sys
from subprocess import Popen, PIPE

RUN_CMD = ["stack", "exec", "--", "compact-coupled-terms-exe"]
PICKLE_FILE = "results.pkl"
CSV_FILE = "results.csv"


class Benchmark:
    def __init__(self, name, size, solution, arguments, return_type):
        self.name = name
        self.size = size
        self.solution = solution
        self.arguments = arguments
        self.return_type = return_type

    def __str__(self):
        return "Benchmark{{bmName=\"{}\", bmSize={}, bmSolution={}, bmArguments={}, bmGoalType={}}}".format(
            self.name, self.size, self.solution, self.arguments, self.return_type)


hplus_benchmarks = [
    Benchmark("appBoth", 5, '(Term "app" [Term "app" [Term "Pair" [], Term "app" [Term "f" [], Term "x" []]], Term "app" [Term "g" [], Term "x" []]])',
              '[("f",TFun (TVar "a") (TVar "b")),("g",TFun (TVar "a") (TVar "c")),("x",TVar "a")]', 'TCons "Pair" [TVar "b",TVar "c"]'),
    Benchmark("test", 5, '(Term "app" [Term "app" [Term "app" [Term "Data.Bool.bool" [], Term "Data.Maybe.Nothing" []], Term "app" [Term "Data.Maybe.Just" [], Term "x" []]], Term "b" []])',
              '[("b",TCons "Bool" []),("x",TVar "a")]', 'TCons "Maybe" [TVar "a"]'),
    Benchmark("both", 7, '(Term "app" [Term "app" [Term "Pair" [], Term "app" [Term "f" [], Term "app" [Term "Data.Tuple.fst" [], Term "p" []]]], Term "app" [Term "f" [], Term "app" [Term "Data.Tuple.snd" [], Term "p" []]]])',
              '[("f",TFun (TVar "a") (TVar "b")),("p",TCons "Pair" [TVar "a",TVar "a"])]', 'TCons "Pair" [TVar "b",TVar "b"]'),
    Benchmark("mapEither", 4, '(Term "app" [Term "Data.Either.partitionEithers" [], Term "app" [Term "app" [Term "GHC.List.map" [], Term "f" []], Term "xs" []]])',
              '[("f",TFun (TVar "a") (TCons "Either" [TVar "b",TVar "c"])),("xs",TCons "List" [TVar "a"])]', 'TCons "Pair" [TCons "List" [TVar "b"],TCons "List" [TVar "c"]]'),
    Benchmark("mapMaybes", 4, '(Term "app" [Term "Data.Maybe.listToMaybe" [], Term "app" [Term "app" [Term "Data.Maybe.mapMaybe" [], Term "f" []], Term "xs" []]])',
              '[("f",TFun (TVar "a") (TCons "Maybe" [TVar "b"])),("xs",TCons "List" [TVar "a"])]', 'TCons "Maybe" [TVar "b"]'),
    Benchmark("mergeEither", 4, '(Term "app" [Term "app" [Term "app" [Term "Data.Either.either" [], Term "Data.Either.Left" []], Term "id" []], Term "e" []])',
              '[("e",TCons "Either" [TVar "a",TCons "Either" [TVar "a",TVar "b"]])]', 'TCons "Either" [TVar "a",TVar "b"]'),
    Benchmark("mbToEither", 5, '(Term "app" [Term "app" [Term "app" [Term "Data.Maybe.maybe" [], Term "app" [Term "Data.Either.Left" [], Term "x" []]], Term "Data.Either.Right" []], Term "mb" []])',
              '[("x",TVar "a"),("mb",TCons "Maybe" [TVar "b"])]', 'TCons "Either" [TVar "a",TVar "b"]'),
    Benchmark("cartProduct", 6, '(Term "app" [Term "app" [Term "GHC.List.map" [], Term "app" [Term "GHC.List.zip" [], Term "xs" []]], Term "app" [Term "app" [Term "GHC.List.map" [], Term "GHC.List.repeat" []], Term "ys" []]])',
              '[("xs",TCons "List" [TVar "a"]),("ys",TCons "List" [TVar "b"])]', 'TCons "List" [TCons "List" [TCons "Pair" [TVar "a",TVar "b"]]]'),
    Benchmark("multiAppPair", 7, '(Term "app" [Term "app" [Term "Pair" [], Term "app" [Term "app" [Term "Data.Tuple.fst" [], Term "tp" []], Term "x" []]], Term "app" [Term "app" [Term "Data.Tuple.snd" [], Term "tp" []], Term "x" []]])',
              '[("tp",TCons "Pair" [TFun (TVar "a") (TVar "b"),TFun (TVar "a") (TVar "c")]),("x",TVar "a")]', 'TCons "Pair" [TVar "b",TVar "c"]'),
    Benchmark("map", 3, '(Term "app" [Term "app" [Term "GHC.List.map" [], Term "f" []], Term "xs" []])',
              '[("f",TFun (TVar "a") (TVar "b")),("xs",TCons "List" [TVar "a"])]', 'TCons "List" [TVar "b"]'),
    Benchmark("replFuncs", 3, '(Term "app" [Term "app" [Term "GHC.List.replicate" [], Term "n" []], Term "f" []])',
              '[("f",TFun (TVar "a") (TVar "b")),("n",TCons "Int" [])]', 'TCons "List" [TFun (TVar "a") (TVar "b")]'),
    Benchmark("mbAppFirst", 5, '(Term "app" [Term "app" [Term "app" [Term "Data.Maybe.maybe" [], Term "x" []], Term "f" []], Term "app" [Term "Data.Maybe.listToMaybe" [], Term "xs" []]])',
              '[("x",TVar "b"),("f",TFun (TVar "a") (TVar "b")),("xs",TCons "List" [TVar "a"])]', 'TVar "b"'),
    Benchmark("mapTwice", 5, '(Term "app" [Term "app" [Term "GHC.List.map" [], Term "g" []], Term "app" [Term "app" [Term "GHC.List.map" [], Term "f" []], Term "xs" []]])',
              '[("f",TFun (TVar "a") (TVar "b")),("g",TFun (TVar "b") (TVar "c")),("xs",TCons "List" [TVar "a"])]', 'TCons "List" [TVar "c"]'),
    Benchmark("resolveEither", 4, '(Term "app" [Term "app" [Term "app" [Term "Data.Either.either" [], Term "f" []], Term "id" []], Term "e" []])',
              '[("e",TCons "Either" [TVar "a",TVar "b"]),("f",TFun (TVar "a") (TVar "b"))]', 'TVar "b"'),
    Benchmark("firstJust", 5, '(Term "app" [Term "app" [Term "Data.Maybe.fromMaybe" [], Term "x" []], Term "app" [Term "Data.Maybe.listToMaybe" [], Term "app" [Term "Data.Maybe.catMaybes" [], Term "xs" []]]])',
              '[("x",TVar "a"),("xs",TCons "List" [TCons "Maybe" [TVar "a"]])]', 'TVar "a"'),
    Benchmark("appendN", 4, '(Term "app" [Term "GHC.List.concat" [], Term "app" [Term "app" [Term "GHC.List.replicate" [], Term "n" []], Term "xs" []]])',
              '[("n",TCons "Int" []),("xs",TCons "List" [TVar "a"])]', 'TCons "List" [TVar "a"]'),
    Benchmark("applyNtimes", 6, '(Term "app" [Term "app" [Term "app" [Term "GHC.List.foldr" [], Term "$" []], Term "x" []], Term "app" [Term "app" [Term "GHC.List.replicate" [], Term "n" []], Term "f" []]])',
              '[("f",TFun (TVar "a") (TVar "a")),("x",TVar "a"),("n",TCons "Int" [])]', 'TVar "a"'),
    Benchmark("dedupe", 5, '(Term "app" [Term "app" [Term "GHC.List.map" [], Term "GHC.List.head" []], Term "app" [Term "app" [Term "Data.List.group" [], Term "tcarg0" []], Term "xs" []]])',
              '[("tcarg0",TCons "@@hplusTC@@Eq" [TVar "a"]),("xs",TCons "List" [TVar "a"])]', 'TCons "List" [TVar "a"]'),
    Benchmark("inverseMap", 5, '(Term "app" [Term "app" [Term "app" [Term "GHC.List.zipWith" [], Term "$" []], Term "fs" []], Term "app" [Term "GHC.List.repeat" [], Term "x" []]])',
              '[("fs",TCons "List" [TFun (TVar "a") (TVar "b")]),("x",TVar "a")]', 'TCons "List" [TVar "b"]'),
    Benchmark("app2", 4, '(Term "app" [Term "app" [Term "f" [], Term "x" []], Term "app" [Term "g" [], Term "x" []]])',
              '[("f",TFun (TVar "a") (TFun (TVar "b") (TVar "c"))),("g",TFun (TVar "a") (TVar "b")),("x",TVar "a")]', 'TVar "c"'),
    Benchmark("singletonList", 3,
              '(Term "app" [Term "app" [Term "Cons" [], Term "x" []], Term "Nil" []])', '[("x",TVar "a")]', 'TCons "List" [TVar "a"]'),
    Benchmark("headLast", 5, '(Term "app" [Term "app" [Term "Pair" [], Term "app" [Term "GHC.List.head" [], Term "xs" []]], Term "app" [Term "GHC.List.last" [], Term "xs" []]])',
              '[("xs",TCons "List" [TVar "a"])]', 'TCons "Pair" [TVar "a",TVar "a"]'),
    Benchmark("headRest", 3, '(Term "app" [Term "Data.Maybe.fromJust" [], Term "app" [Term "GHC.List.uncons" [], Term "xs" []]])',
              '[("xs",TCons "List" [TVar "a"])]', 'TCons "Pair" [TVar "a",TCons "List" [TVar "a"]]'),
    Benchmark("coundPredMatch", 4, '(Term "app" [Term "GHC.List.length" [], Term "app" [Term "app" [Term "GHC.List.filter" [], Term "p" []], Term "xs" []]])',
              '[("xs",TCons "List" [TVar "a"]),("p",TFun (TVar "a") (TCons "Bool" []))]', 'TCons "Int" []'),
    Benchmark("splitStr", 7, '(Term "what is the solutions?" [])',
              '[("str",TCons "List" [TCons "Char" []]),("c",TCons "Char" [])]', 'TCons "List" [TCons "List" [TCons "Char" []]]'),
    Benchmark("splitAtFirst", 5, '(Term "app" [Term "app" [Term "GHC.List.break" [], Term "app" [Term "app" [Term "(Data.Eq.==)" [], Term "tcarg0" []], Term "x" []]], Term "xs" []])',
              '[("tcarg0",TCons "@@hplusTC@@Eq" [TVar "a"]),("x",TVar "a"),("xs",TCons "List" [TVar "a"])]', 'TCons "Pair" [TCons "List" [TVar "a"],TCons "List" [TVar "a"]]'),
    Benchmark("hoogle01", 3, '(Term "app" [Term "f" [], Term "app" [Term "GHC.List.head" [], Term "xs" []]])',
              '[("f",TFun (TVar "a") (TVar "b")),("xs",TCons "List" [TVar "a"])]', 'TVar "b"'),
    Benchmark("firstMatch", 4, '(Term "app" [Term "GHC.List.head" [], Term "app" [Term "app" [Term "GHC.List.filter" [], Term "p" []], Term "xs" []]])',
              '[("xs",TCons "List" [TVar "a"]),("p",TFun (TVar "a") (TCons "Bool" []))]', 'TVar "a"'),
    Benchmark("firstMaybe", 3, '(Term "app" [Term "GHC.List.head" [], Term "app" [Term "Data.Maybe.catMaybes" [], Term "mbs" []]])',
              '[("mbs",TCons "List" [TCons "Maybe" [TVar "a"]])]', 'TVar "a"'),
    Benchmark("rights", 3, '(Term "app" [Term "Data.Either.Right" [], Term "app" [Term "Data.Either.rights" [], Term "es" []]])',
              '[("es",TCons "List" [TCons "Either" [TVar "a",TVar "b"]])]', 'TCons "Either" [TVar "a",TCons "List" [TVar "b"]]'),
    Benchmark("firstKey", 3, '(Term "app" [Term "Data.Tuple.fst" [], Term "app" [Term "GHC.List.head" [], Term "xs" []]])',
              '[("xs",TCons "List" [TCons "Pair" [TVar "a",TVar "b"]])]', 'TVar "a"'),
    Benchmark("firstRight", 4, '(Term "app" [Term "Data.Either.Right" [], Term "app" [Term "GHC.List.head" [], Term "app" [Term "Data.Either.rights" [], Term "es" []]]])',
              '[("es",TCons "List" [TCons "Either" [TVar "a",TVar "b"]])]', 'TCons "Either" [TVar "a",TVar "b"]'),
    Benchmark("maybe", 4, '(Term "app" [Term "Data.Maybe.Just" [], Term "app" [Term "app" [Term "Data.Maybe.fromMaybe" [], Term "x" []], Term "mb" []]])',
              '[("mb",TCons "Maybe" [TVar "a"]),("x",TVar "a")]', 'TCons "Maybe" [TVar "a"]'),
    Benchmark("app3", 4, '(Term "app" [Term "app" [Term "app" [Term "f" [], Term "x" []], Term "z" []], Term "y" []])',
              '[("f",TFun (TVar "a") (TFun (TVar "b") (TFun (TVar "c") (TVar "d")))),("x",TVar "a"),("y",TVar "c"),("z",TVar "b")]', 'TVar "d"'),
    Benchmark("zipWithResult", 5, '(Term "app" [Term "app" [Term "GHC.List.zip" [], Term "xs" []], Term "app" [Term "app" [Term "GHC.List.map" [], Term "f" []], Term "xs" []]])',
              '[("f",TFun (TVar "a") (TVar "b")),("xs",TCons "List" [TVar "a"])]', 'TCons "List" [TCons "Pair" [TVar "a",TVar "b"]]'),
    Benchmark("eitherTriple", 5, '(Term "app" [Term "app" [Term "app" [Term "Data.Bool.bool" [], Term "e2" []], Term "e1" []], Term "app" [Term "Data.Either.isLeft" [], Term "e1" []]])',
              '[("e1",TCons "Either" [TVar "a",TVar "b"]),("e2",TCons "Either" [TVar "a",TVar "b"])]', 'TCons "Either" [TVar "a",TVar "b"]'),
    Benchmark("pipe", 4, '(Term "app" [Term "app" [Term "app" [Term "GHC.List.foldr" [], Term "$" []], Term "x" []], Term "fs" []])',
              '[("fs",TCons "List" [TFun (TVar "a") (TVar "a")]),("x",TVar "a")]', 'TVar "a"'),
    Benchmark("lookup", 5, '(Term "app" [Term "Data.Maybe.fromJust" [], Term "app" [Term "app" [Term "app" [Term "GHC.List.lookup" [], Term "tcarg0" []], Term "k" []], Term "xs" []]])',
              '[("tcarg0",TCons "@@hplusTC@@Eq" [TVar "a"]),("xs",TCons "List" [TCons "Pair" [TVar "a",TVar "b"]]),("k",TVar "a")]', 'TVar "b"'),
    Benchmark("mbElem", 6, '(Term "app" [Term "Data.Maybe.listToMaybe" [], Term "app" [Term "app" [Term "GHC.List.filter" [], Term "app" [Term "app" [Term "(Data.Eq.==)" [], Term "tcarg0" []], Term "x" []]], Term "xs" []]])',
              '[("tcarg0",TCons "@@hplusTC@@Eq" [TVar "a"]),("x",TVar "a"),("xs",TCons "List" [TVar "a"])]', 'TCons "Maybe" [TVar "a"]'),
    Benchmark("areEq", 7, '(Term "app" [Term "Data.Maybe.listToMaybe" [], Term "app" [Term "app" [Term "GHC.List.filter" [], Term "app" [Term "app" [Term "(Data.Eq.==)" [], Term "tcarg0" []], Term "x" []]], Term "app" [Term "GHC.List.repeat" [], Term "y" []]]])',
              '[("tcarg0",TCons "@@hplusTC@@Eq" [TVar "a"]),("x",TVar "a"),("y",TVar "a")]', 'TCons "Maybe" [TVar "a"]'),
    Benchmark("applyPair", 4, '(Term "app" [Term "app" [Term "Data.Tuple.fst" [], Term "p" []], Term "app" [Term "Data.Tuple.snd" [], Term "p" []]])',
              '[("p",TCons "Pair" [TFun (TVar "a") (TVar "b"),TVar "a"])]', 'TVar "b"'),
    Benchmark("flatten", 3, '(Term "app" [Term "GHC.List.concat" [], Term "app" [Term "GHC.List.concat" [], Term "xss" []]])',
              '[("xss",TCons "List" [TCons "List" [TCons "List" [TVar "a"]]])]', 'TCons "List" [TVar "a"]'),
    Benchmark("takeNdropM", 7, '(Term "app" [Term "app" [Term "Pair" [], Term "app" [Term "app" [Term "GHC.List.take" [], Term "n" []], Term "xs" []]], Term "app" [Term "app" [Term "GHC.List.drop" [], Term "m" []], Term "xs" []]])',
              '[("n",TCons "Int" []),("m",TCons "Int" []),("xs",TCons "List" [TVar "a"])]', 'TCons "Pair" [TCons "List" [TVar "a"], TCons "List" [TVar "a"]]'),
    Benchmark("indexesOf", 6, '(Term "app" [Term "app" [Term "GHC.List.map" [], Term "Data.Tuple.snd" []], Term "app" [Term "f" [], Term "app" [Term "app" [Term "GHC.List.zip" [], Term "xs" []], Term "ys" []]]])',
              '[("f",TFun (TCons "List" [TCons "Pair" [TVar "a",TCons "Int" []]]) (TCons "List" [TCons "Pair" [TVar "a",TCons "Int" []]])),("xs",TCons "List" [TVar "a"]),("ys",TCons "List" [TCons "Int" []])]', 'TCons "List" [TCons "Int" []]'),
    Benchmark("containsEdge", 9, '(Term "app" [Term "app" [Term "(Data.Bool.&&)" [], Term "app" [Term "app" [Term "GHC.List.elem" [], Term "app" [Term "Data.Tuple.fst" [], Term "edge" []]], Term "vs" []]], Term "app" [Term "app" [Term "GHC.List.elem" [], Term "app" [Term "Data.Tuple.snd" [], Term "edge" []]], Term "vs" []]])',
              '[("vs",TCons "List" [TCons "Int" []]),("edge",TCons "Pair" [TCons "Int" [],TCons "Int" []])]', 'TCons "Bool" []'),
]

stackoverflow_benchmarks = [
    Benchmark("extractEitherValues", 5, '(Term "app" [Term "app" [Term "GHC.List.map" [], Term "app" [Term "app" [Term "Data.Either.either" [], Term "id" []], Term "id" []]], Term "es" []])',
              '[("es",TCons "List" [TCons "Either" [TVar "b",TVar "b"]])]', 'TCons "List" [TVar "b"]'),
    Benchmark("filterMultiple", 6, '(Term "app" [Term "app" [Term "GHC.List.filter" [], Term "app" [Term "app" [Term "Data.Function.flip" [], Term "app" [Term "GHC.List.elem" [], Term "tcarg0" []]], Term "xs" []]], Term "ys" []])',
              '[("tcarg0",TCons "@@hplusTC@@Eq" [TVar "a"]),("xs",TCons "List" [TVar "a"]),("ys",TCons "List" [TVar "a"])]', 'TCons "List" [TVar "a"]'),
    Benchmark("filterNot", 5, '(Term "app" [Term "app" [Term "GHC.List.filter" [], Term "app" [Term "app" [Term "(Data.Function..)" [], Term "Data.Bool.not" []], Term "p" []]], Term "xs" []])',
              '[("p",TFun (TVar "a") (TCons "Bool" [])),("xs",TCons "List" [TVar "a"])]', 'TCons "List" [TVar "a"]'),
    Benchmark("isSortedBy", 6, '(Term "app" [Term "GHC.List.and" [], Term "app" [Term "app" [Term "app" [Term "GHC.List.zipWith" [], Term "cmp" []], Term "xs" []], Term "app" [Term "GHC.List.tail" [], Term "xs" []]]])',
              '[("cmp", TFun (TVar "a") (TFun (TVar "a") (TCons "Bool" []))),("xs",TCons "List" [TVar "a"])]', 'TCons "Bool" []'),
    Benchmark("multipleNth", 4, '(Term "app" [Term "app" [Term "GHC.List.map" [], Term "app" [Term "(GHC.List.!!)" [], Term "xs" []]], Term "indices" []])',
              '[("xs",TCons "List" [TVar "a"]),("indices",TCons "List" [TCons "Int" []])]', 'TCons "List" [TVar "a"]'),
    Benchmark("doubleMap", 4, '(Term "app" [Term "app" [Term "GHC.List.map" [], Term "app" [Term "GHC.List.map" [], Term "f" []]], Term "xss" []])',
              '[("f",TFun (TVar "a") (TVar "b")),("xss",TCons "List" [TCons "List" [TVar "a"]])]', 'TCons "List" [TCons "List" [TVar "b"]]'),
    Benchmark("lengthOfSnds", 5, '(Term "app" [Term "GHC.List.length" [], Term "app" [Term "GHC.List.concat" [], Term "app" [Term "app" [Term "GHC.List.map" [], Term "Data.Tuple.snd" []], Term "ps" []]] ])',
              '[("ps",TCons "List" [TCons "Pair" [TVar "a",TCons "List" [TVar "b"]]])]', 'TCons "Int" []'),
    Benchmark("lessThanNTimes", 8, '(Term "app" [Term "GHC.List.null" [], Term "app" [Term "app" [Term "GHC.List.drop" [], Term "n" []], Term "app" [Term "app" [Term "GHC.List.filter" [], Term "app" [Term "app" [Term "(Data.Eq.==)" [], Term "tcarg0" []], Term "x" []]], Term "xs" []]]])',
              '[("tcarg0",TCons "@@hplusTC@@Eq" [TVar "a"]),("n",TCons "Int" []),("x",TVar "a"),("xs",TCons "List" [TVar "a"])]', 'TCons "Bool" []'),
    Benchmark("groupByFirst", 6, '(Term "app" [Term "app" [Term "Data.List.groupBy" [], Term "app" [Term "app" [Term "Data.Function.on" [], Term "app" [Term "(Data.Eq.==)" [], Term "tcarg0" []]], Term "Data.Tuple.fst" []]], Term "xs" []])',
              '[("tcarg0",TCons "@@hplusTC@@Eq" [TVar "a"]),("xs",TCons "List" [TCons "Pair" [TVar "a",TVar "b"]])]', 'TCons "List" [TCons "List" [TCons "Pair" [TVar "a",TVar "b"]]]'),
    Benchmark("mySortBy", 5, '(Term "app" [Term "app" [Term "Data.List.sortBy" [], Term "app" [Term "app" [Term "Data.Function.on" [], Term "cmp" []], Term "Data.Tuple.fst" []]], Term "xs" []])',
              '[("cmp",TFun (TVar "a") (TFun (TVar "a") (TCons "Ordering" []))),("xs",TCons "List" [TCons "Pair" [TVar "a",TVar "b"]])]', 'TCons "List" [TCons "Pair" [TVar "a",TVar "b"]]'),
    Benchmark("transpose", 6, '(Term "app" [Term "app" [Term "app" [Term "GHC.List.foldr" [], Term "app" [Term "GHC.List.zipWith" [], Term "Cons" []]], Term "app" [Term "GHC.List.repeat" [], Term "Nil" []]], Term "xs" []])',
              '[("xs",TCons "List" [TCons "List" [TVar "a"]])]', 'TCons "List" [TCons "List" [TVar "a"]]'),
    Benchmark("partition", 9, '(Term "app" [Term "app" [Term "Pair" [], Term "app" [Term "app" [Term "GHC.List.filter" [], Term "p" []], Term "xs" []]], Term "app" [Term "app" [Term "GHC.List.filter" [], Term "app" [Term "app" [Term "(Data.Function..)" [], Term "Data.Bool.not" []], Term "p" []]], Term "xs" []]])',
              '[("p",TFun (TVar "a") (TCons "Bool" [])),("xs",TCons "List" [TVar "a"])]', 'TCons "Pair" [TCons "List" [TVar "a"],TCons "List" [TVar "a"]]'),
    Benchmark("matchedKeys", 7, '(Term "app" [Term "app" [Term "GHC.List.map" [], Term "Data.Tuple.fst" []], Term "app" [Term "app" [Term "GHC.List.filter" [], Term "app" [Term "app" [Term "(Data.Function..)" [], Term "p" []], Term "Data.Tuple.snd" []]], Term "xs" []]])',
              '[("p",TFun (TVar "b") (TCons "Bool" [])),("xs",TCons "List" [TCons "Pair" [TVar "a",TVar "b"]])]', 'TCons "List" [TVar "a"]'),
    Benchmark("filterPairs", 6, '(Term "app" [Term "app" [Term "GHC.List.filter" [], Term "app" [Term "Data.Tuple.uncurry" [], Term "f" []]], Term "app" [Term "app" [Term "GHC.List.zip" [], Term "xs" []], Term "ys" []]])',
              '[("f",TFun (TVar "a") (TFun (TVar "b") (TCons "Bool" []))),("xs",TCons "List" [TVar "a"]),("ys",TCons "List" [TVar "b"])]', 'TCons "List" [TCons "Pair" [TVar "a",TVar "b"]]'),
    Benchmark("applyNthNTimes", 7, '(Term "app" [Term "app" [Term "app" [Term "GHC.List.zipWith" [], Term "$" []], Term "app" [Term "app" [Term "GHC.List.iterate" [], Term "app" [Term "(Data.Function..)" [], Term "f" []]], Term "f" []]], Term "xs" []])',
              '[("f",TFun (TVar "a") (TVar "a")),("xs",TCons "List" [TVar "a"])]', 'TCons "List" [TVar "a"]'),
    Benchmark("removeMax", 7, '(Term "app" [Term "app" [Term "GHC.List.filter" [], Term "app" [Term "app" [Term "(Data.Eq./=)_Ord" [], Term "tcarg0" []], Term "app" [Term "app" [Term "GHC.List.maximum" [], Term "tcarg0" []], Term "xs" []]]], Term "xs" []])',
              '[("tcarg0",TCons "@@hplusTC@@Ord" [TVar "a"]),("xs",TCons "List" [TVar "a"])]', 'TCons "List" [TVar "a"]'),
    Benchmark("allSameBy", 7, '(Term "app" [Term "app" [Term "GHC.List.all" [], Term "app" [Term "app" [Term "app" [Term "Data.Function.on" [], Term "app" [Term "(Data.Eq.==)" [], Term "tcarg0" []]], Term "cmp" []], Term "xs" []]], Term "xss" []])',
              '[("tcarg0", TCons "@@hplusTC@@Eq" [TVar "b"]), ("cmp", TFun (TVar "a") (TVar "b")), ("xs", TVar "a"),("xss",TCons "List" [TVar "a"])]', 'TCons "Bool" []'),
    Benchmark("mostFrequent", 7, '(Term "app" [Term "app" [Term "Data.List.maximumBy" [], Term "app" [Term "app" [Term "Data.Function.on" [], Term "app" [Term "Data.Ord.compare" [], Term "tcarg0" []]], Term "Data.Tuple.snd" []]], Term "app" [Term "occur" [], Term "xs" []]])',
              '[("tcarg0", TCons "@@hplusTC@@Ord" [TVar "b"]), ("xs",TCons "List" [TVar "a"]), ("occur", TFun (TCons "List" [TVar "a"]) (TCons "List" [TCons "Pair" [TVar "a", TVar "b"]]))]', 'TCons "Pair" [TVar "a", TVar "b"]'),
    Benchmark("splitOn", 7, '(Term "app" [Term "app" [Term "Data.List.groupBy" [], Term "app" [Term "app" [Term "Data.Function.on" [], Term "(Data.Bool.&&)" []], Term "app" [Term "app" [Term "(Data.Eq./=)" [], Term "tcarg0" []], Term "x" []]]], Term "xs" []])',
              '[("tcarg0", TCons "@@hplusTC@@Eq" [TVar "a"]), ("x", TVar "a"), ("xs", TCons "List" [TVar "a"])]', 'TCons "List" [TCons "List" [TVar "a"]]'),
]


class BenchmarkResult:
    def __init__(self, name, sol, time):
        self.name = name
        self.sol = sol
        self.time = time


def get_time(s):
    match_result = re.search(r"Time: (\d+\.\d+)s", s)
    return match_result.group(1)


def to_time(s):
    if s:
        return float(s)
    else:
        return None


def run_benchmark(bench, ablation, limit):
    p = Popen(RUN_CMD + [str(bench), "--ablation", ablation, "--timeoutlimit", str(limit)], stdin=PIPE, stdout=PIPE)
    prev_line = None
    syn_prog = None
    syn_time = None
    for line in iter(p.stdout.readline, b''):
        print(line.decode(), end='')
        syn_prog = prev_line
        if "Time:" in line.decode():
            syn_time = get_time(line.decode())
            print("Success", syn_time)
        else:
            prev_line = line.decode()

    if syn_prog is None:
        print("Fail")

    sys.stdout.flush()
    syn_results.append(BenchmarkResult(
        bench.name, syn_prog, to_time(syn_time)))


def build_argparser():
    argparser = argparse.ArgumentParser(description="Run benchmarks")
    argparser.add_argument(
        '--suites', choices=['hplus', 'stackoverflow', 'all'], default='all', help="which suites to run")
    argparser.add_argument('--benchmarks', nargs='+',
                           help="which benchmarks to run")
    argparser.add_argument('--ablation', choices=['default', 'noReduction', 'noEnumeration', 'noOptimize'], default='default', help="which ablation to run")
    argparser.add_argument('--timeout', type=int, default=300, help="timeout for each benchmark")
    return argparser


if __name__ == "__main__":
    args = build_argparser().parse_args()

    benchmarks = {}
    if 'hplus' in args.suites or 'all' in args.suites:
        if args.benchmarks:
            benchmarks['hplus'] = [bench for bench in hplus_benchmarks
                                   if bench.name in args.benchmarks]
        else:
            benchmarks['hplus'] = hplus_benchmarks

    if 'stackoverflow' in args.suites or 'all' in args.suites:
        if args.benchmarks:
            benchmarks['stackoverflow'] = [bench for bench in stackoverflow_benchmarks
                                           if bench.name in args.benchmarks]
        else:
            benchmarks['stackoverflow'] = stackoverflow_benchmarks

    for i in range(3):
        syn_results = []
        for suite, suite_benches in benchmarks.items():
            print("===============================================================")
            print("Running suite", suite)
            for bench in suite_benches:
                print("---------------------------------------------------------------")
                print("Running benchmark: " + bench.name)
                run_benchmark(bench, args.ablation, args.timeout)

        # write results to csv
        csv_file = "results" + "_" + args.suites + "_" + args.ablation + "_" + str(i) + ".csv"
        with open(csv_file, "w") as f:
            f.write("name,sol,time\n")
            for result in syn_results:
                f.write("{}\t{}\t{}\n".format(
                    result.name, result.sol, result.time))
