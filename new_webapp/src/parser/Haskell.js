import Parsimmon from "parsimmon";

const P = Parsimmon;


// Parser combinator for a Haskell-with-named-arguments type description language.
export const typeParser = Parsimmon.createLanguage({
    TypeDecl: (r) => {
        return r.TypeCtx
            .skip(P.string("=>"))
            .times(0,1)
            .trim(P.optWhitespace)
            .then(r.Type)
    },
    TypeCtx: (r) => {
        return P.alt(
                r.TyCon,
                r.VarId,
                r.CtxTuple
            )
            .trim(P.optWhitespace)
    },
    CtxTuple: (r) => {
        return P.string("(")
            .trim(Parsimmon.optWhitespace)
            .then(P.alt(r.TyCon, r.VarId))
            .then(
                P.string(",")
                .trim(Parsimmon.optWhitespace)
                .then(P.alt(r.TyCon, r.VarId))
                .many()
            )
            .skip(P.string(")"))
    },
    Type: (r) => {
        const argName = r.ArgName.trim(Parsimmon.optWhitespace)
        const argTy = r.BType.trim(Parsimmon.optWhitespace)
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
        const constant = r.BType.map(x => {return {argTy: x}});
        return P.alt(func, constant);
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
            r.Unit,
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
    Unit: () => P.string("()"),
    ListTy: (r) => {
        return P.string("[")
            .trim(Parsimmon.optWhitespace)
            .then(r.Type)
            .skip(P.string("]"));
    },
    _: () => {return Parsimmon.optWhitespace;},
});