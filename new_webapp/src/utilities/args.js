import _ from "underscore";
import { typeParser } from "../parser/Haskell";

export const getArgNames = (numArgs) => {
    let argNames = [];
    for (let index = 0; index < numArgs; index++) {
      argNames = argNames.concat("arg" + index);
    }
    return argNames;
}

// [{arg0..argN, output}] -> [{inputs:[str], output:str}]
export const namedArgsToExample = (listList, numArgs) => {
  return listList.map(element => {
    const argNames = getArgNames(numArgs);
    const inputs = argNames.map(argName => element[argName]);
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
      const argName = "arg" + idx;
      obj[argName] = str;
      return obj;
    }, {});
    namedArgs["output"] = example.output;
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

export const getArgCount = (queryStr) => {
  try {
    const result = typeParser.TypeDecl.tryParse(queryStr);
    console.log("parse result", result);
    return depth(result);
  } catch (error) {
    console.log("parse error", error);
    return null;
  }
}

const depth = (xs) => {
  if(!xs || !xs.result || xs.result.length == 0) {
    return 0;
  }
  return depth(xs.result[0]) + 1;
}
