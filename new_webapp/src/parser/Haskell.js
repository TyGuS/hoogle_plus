import Parsimmon from "parsimmon";

const P = Parsimmon;


// Parser combinator for a Haskell-with-named-arguments type description language.
export const typeParser = Parsimmon.createLanguage({
    Type: (r) => {
        const argName = r.ArgName.trim(Parsimmon.optWhitespace)
        const argTy = r.BType.trim(Parsimmon.optWhitespace)
        const res = P.string("->")
            .trim(Parsimmon.optWhitespace)
            .then(r.Type)
            .trim(Parsimmon.optWhitespace)
            .many();
        return P.seqObj(
            ["argName", argName],
            ["argTy", argTy],
            ["result", res]
        );
    },
    ArgName: (r) => {
        return P.regex(/[a-zA-Z0-9_]+/)
            .trim(Parsimmon.optWhitespace)
            .skip(P.string(":"))
            .times(0, 1);
    },
    BType: (r) => {
        return P.alt(
            r.TyCon,
            r.VarId,
            r.Tuple,
            r.ListTy,
            P.string("(")
                .trim(Parsimmon.optWhitespace)
                .then(r.Type)
                .trim(Parsimmon.optWhitespace)
                .skip(P.string(")"))
        )
    },
    TyCon: (r) => {
        return r.VarId
            .skip(P.whitespace)
            .then(r.BType);
    },
    Tuple: (r) => {
        return P.string("(")
            .trim(Parsimmon.optWhitespace)
            .then(r.Type)
            .then(
                P.string(",")
                .trim(Parsimmon.optWhitespace)
                .then(r.Type)
                .many()
            )
            .skip(P.string(")"))
    },
    VarId: () => {return P.regex(/[a-zA-Z0-9]+/)},
    ListTy: (r) => {
        return P.string("[")
            .trim(Parsimmon.optWhitespace)
            .then(r.Type)
            .skip(P.string("]"));
    },
    _: () => {return Parsimmon.optWhitespace;},
});