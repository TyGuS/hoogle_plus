import _, { mapObject } from "underscore";
import { typeParser } from "../parser/Haskell";

export const getArgNames = (numArgs) => {
    let argNames = [];
    const initArgs = ["x", "y", "z"];
    const initArgNum = initArgs.length;
    for (let index = 0; index < numArgs; index++) {
        if (index < initArgNum) {
            argNames = argNames.concat(initArgs[index]);
        } else {
            argNames = argNames.concat("x" + (index - initArgNum + 1));
        }
    }
    return argNames;
}

// [{arg0..argN, output}] -> [{inputs:[str], output:str}]
export const namedArgsToExample = (listList, numArgs) => {
  return listList.map(element => {
    const argIndices = [...Array(numArgs).keys()];
    const inputs = argIndices.map(idx => element[idx]);
    return {
      inputs,
      output: element['output'],
      id: element['id'],
    };
  });
};

// [{inputs: [str], output: str, id}] -> [{id, arg0, ... output}]
export const exampleToNamedArgs = (examples) => {
  if (examples && (examples.length > 0) && (! examples[0].inputs)) {
    debugger;
  }
  return examples.map(example => {
    let namedArgs = example.inputs.reduce((obj, str, idx) => {
      obj[idx] = str;
      return obj;
    }, {});
    namedArgs["output"] = example.output;
    namedArgs["isLoading"] = example.isLoading;
    namedArgs["id"] = example.id;
    if (example.error) {
      namedArgs["error"] = example.error;
    }
    return namedArgs;
  });
};

export const inputsToId = (inputs) => {
  return inputs.reduce((prev, curr) => prev + "-" + curr, "unknown");
}

export const getArgInfo = (queryStr) => {
  try {
    const result = typeParser.TypeDecl.tryParse(queryStr);
    return {
      argNum: depth(result),
      argNames: argNamesOf(result),
      parsedType: result,
    };
  } catch (error) {
    console.log("parse error", error);
    return null;
  }
}

const depth = (xs) => {
  if(!xs || !xs.result || xs.result.length === 0) {
    return 0;
  }
  return depth(xs.result[0]) + 1;
}

export const argNamesOf = (xs) => {
  if(!xs.argName || !xs.result || xs.result.length === 0) {
    return [];
  }
  return xs.argName.concat(argNamesOf(xs.result[0]));
}

export const parseResultToStr = (obj) => {
  if(!obj.result || obj.result.length === 0) {
    // pretty print the datatypes
    if(obj.datatype) {
      const argStrs = obj.arguments.map(parseResultToStr);
      const addParens = (argStr) => {
        if(argStr.includes(" ") || argStr.includes("->")) {
          return "(" + argStr + ")";
        }
        return argStr;
      };
      switch(obj.datatype) {
        case "List":
          return "[" + argStrs[0] + "]";
        case "Pair":
          return "(" + argStrs.join(", ") + ")";
        default:
          const parensArgStrs = argStrs.map(addParens);
          return [obj.datatype].concat(parensArgStrs).join(" ");
      }
    }

    // pretty print the argument types
    if(obj.argTy) {
      const argTyp = obj.argTy;
      return parseResultToStr(argTyp);
    }

    return obj;
  }

  if(obj.argName.length !== 0) {
    return obj.argName[0] + ": " + parseResultToStr(obj.argTy) + " -> " + parseResultToStr(obj.result[0]);
  }

  return parseResultToStr(obj.argTy) + " -> " + parseResultToStr(obj.result[0]);
}

export const replaceArgName = (obj, oldArgName, newArgName) => {
  if(!obj.result || obj.result.length === 0) {
    return obj;
  }

  if(obj.argName[0] === oldArgName) {
    return {
      ...obj,
      argName: [newArgName]
    };
  }

  const child = replaceArgName(obj.result[0], oldArgName, newArgName);
  return {
    ...obj,
    result: [child],
  };
}