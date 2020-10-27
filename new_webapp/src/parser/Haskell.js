import Parsimmon from "parsimmon";
import { parseResultToStr } from "../utilities/args";

const P = Parsimmon;


// Parser combinator for a Haskell-with-named-arguments type description language.
export const typeParser = Parsimmon.createLanguage({
    TypeDecl: (r) => {
        const typeBody = P.string("=>")
            .trim(P.optWhitespace)
            .then(r.Type);
        const withTyclass = P.seqObj(
            ["typeClasses", r.TypeCtx],
            ["typeBody", typeBody]
        );
        return P.alt(
            withTyclass,
            r.Type
        )
    },
    TypeCtx: (r) => {
        return P.alt(
                r.Type,
                r.CtxTuple
            )
            .trim(P.optWhitespace)
    },
    CtxTuple: (r) => {
        return P.string("(")
            .trim(Parsimmon.optWhitespace)
            .then(P.seqMap(
                r.Type,
                P.string(",")
                .trim(Parsimmon.optWhitespace)
                .then(r.Type)
                .many(),
                (first, rest) => [first].concat(rest)))
            .skip(P.string(")"))
    },
    Type: (r) => {
        const argName = r.ArgName.trim(Parsimmon.optWhitespace)
        const argTy = r.AType.trim(Parsimmon.optWhitespace)
        const res = P.string("->")
            .trim(Parsimmon.optWhitespace)
            .then(r.Type)
            .trim(Parsimmon.optWhitespace)
            .many();
        const func = P.seqObj(
            ["argName", argName],
            ["argTy", argTy],
            ["result", res]
        );
        const constant = r.AType.map(x => {return {argTy: x}});
        return P.alt(func, constant);
    },
    ArgName: (r) => {
        return P.regex(/[a-zA-Z0-9_]+/)
            .trim(Parsimmon.optWhitespace)
            .skip(P.string(":"))
            .times(0, 1);
    },
    AType: (r) => {
        return P.alt(
            r.TyCon,
            r.BType
        );
    },
    BType: (r) => {
        return P.alt(
            r.VarId,
            r.Tuple,
            r.ListTy,
            r.Unit,
            P.string("(")
                .trim(Parsimmon.optWhitespace)
                .then(r.Type)
                .trim(Parsimmon.optWhitespace)
                .skip(P.string(")"))
        )
    },
    TyCon: (r) => {
        return P.seqMap(
            r.VarId,
            P.seqMap(P.whitespace, r.BType, (sp, t) => t).many(),
            (dt, rest) => {return {
                datatype: dt,
                arguments: rest
            }});
    },
    Tuple: (r) => {
        return P.string("(")
            .trim(Parsimmon.optWhitespace)
            .then(P.seqMap(
                r.Type,
                P.string(",")
                .trim(Parsimmon.optWhitespace)
                .then(r.Type)
                .atLeast(1),
                (first, rest) => [first].concat(rest)
            ))
            .skip(P.string(")"))
            .map((xs) => {
                return {
                    datatype: "Pair",
                    arguments: xs
                };
            })
    },
    VarId: () => {return P.regex(/[a-zA-Z0-9]+/)},
    Unit: () => P.string("()"),
    ListTy: (r) => {
        return P.string("[")
            .trim(Parsimmon.optWhitespace)
            .then(r.Type)
            .skip(P.string("]"))
            .map((t) => {
                return {
                    datatype: "List",
                    arguments: [t]
                };
            });
    },
    _: () => {return Parsimmon.optWhitespace;},
});