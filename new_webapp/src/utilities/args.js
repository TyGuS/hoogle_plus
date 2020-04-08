export const getArgNames = (numArgs) => {
    let argNames = [];
    for (let index = 0; index < numArgs; index++) {
      argNames = argNames.concat("arg" + index);
    }
    return argNames;
}

// [{arg0..argN, output}] -> [{usage: [0,..N,output], id}]
export const namedArgsToUsage = (listList, numArgs) => {
  return listList.map(element => {
      const argNames = getArgNames(numArgs);
      const usage = argNames.map(argName => element[argName]).concat(element.output);
      return {
          usage: usage,
          id: usageToId(usage),
      }
  });
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

// [{usage: [str], id}] -> [{id, arg0, ... output}]
export const usageToNamedArgs = (facts) => {
  const rows = facts.map((element) => {
    let row = [];
    for (let index = 0; index < element.usage.length - 1; index++) {
      let argName = "arg" + index;
      row[argName] = element.usage[index];
    }
    row["output"] = element.usage[element.usage.length - 1];
    row["id"] = element.id;
    return row;
  });
  return rows;
};

// [str] -> {inputs: [str], output: str}
export const usageToExample = (usage) => {
  const lastIndex = usage.length - 1;
  const inputs = usage.slice(0, lastIndex);
  const output = usage[lastIndex];
  return {inputs,output};
};

export const usageToId = (usage) => {
  const args = usage.slice(0, usage.length - 1);
  return args.reduce((prev, curr) => prev + "-" + curr, "unknown");
}

export const inputsToId = (inputs) => {
  return inputs.reduce((prev, curr) => prev + "-" + curr, "unknown");
}

export const getArgCount = (queryStr) => {
  let arrows = 0;
  let countUntil = queryStr.length;
  if (queryStr.trim().endsWith(')')){
    let parens = 0;
    let trimTo = 0;
    for (let i = queryStr.length - 1; i >= 0; --i){
      if(queryStr[i] === ')') {
        if (parens === 0) { trimTo = i-1; }
        parens += 1;
      }
      else if (queryStr[i] === '(') {
        parens -= 1;
        if (parens === 0) {
          arrows += getArgCount(queryStr.substr(i+1,trimTo-i))
          countUntil = i;
          break;
        }
      }
    }
  }
  let parens = 0;
  for (let i = 0; i < countUntil; ++i){
    if (queryStr[i] === '-') {
      if (queryStr[i+1] === '>' && parens === 0){
        arrows += 1;
      }
    }
    else if (queryStr[i] === '(') {
      parens += 1;
    }
    else if (queryStr[i] === ')') {
      parens -= 1;
    }
  }
  return arrows;
}
