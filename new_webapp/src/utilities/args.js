export const getArgNames = (numArgs) => {
    let argNames = [];
    for (let index = 0; index < numArgs; index++) {
      argNames = argNames.concat("arg" + index);
    }
    return argNames;
}

// [{arg0..argN, result}] -> [{usage: [0,..N,result], id}]
export const namedArgsToUsage = (listList, numArgs) => {
  return listList.map(element => {
      const argNames = getArgNames(numArgs);
      const usage = argNames.map(argName => element[argName]).concat(element.result);
      return {
          usage: usage,
          id: usageToId(usage),
      }
  });
}

// [{usage: [], id}] -> [{id, arg0, ... result}]
export const usageToNamedArgs = (facts) => {
  const rows = facts.map((element) => {
    let row = [];
    for (let index = 0; index < element.usage.length - 1; index++) {
      let argName = "arg" + index;
      row[argName] = element.usage[index];
    }
    row["result"] = element.usage[element.usage.length - 1];
    row["id"] = element.id;
    return row;
  });
  return rows;
};

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
  let parens = 0;
  for (let i = 0; i < queryStr.length; ++i){
    if (queryStr[i] == '-') {
      if (queryStr[i+1] == '>' && parens == 0){
        arrows += 1;
      }
    }
    else if (queryStr[i] == '(') {
      parens += 1;
    }
    else if (queryStr[i] == ')') {
      parens -= 1;
    }
  }
  return arrows;
}