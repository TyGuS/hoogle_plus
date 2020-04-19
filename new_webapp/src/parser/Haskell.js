import Parsimmon from "parsimmon";

const P = Parsimmon;

export const typeParser =  Parsimmon.createLanguage({
    Type: (r) => {
        return r.BType
            .trim(Parsimmon.optWhitespace)
            .then(
                P.string("->")
                .trim(Parsimmon.optWhitespace)
                .then(r.Type)
                .trim(Parsimmon.optWhitespace)
                .many()
            )
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
                .skip(P.string(")"))
        )
    },
    TyCon: (r) => {
        return r.VarId
            .then(P.whitespace)
            .then(r.Type);
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