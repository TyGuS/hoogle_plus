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

export const usageToId = (usage) => {
  const args = usage.slice(0, usage.length - 1);
  return args.reduce((prev, curr) => prev + "-" + curr);
}

export const inputsToId = (inputs) => {
  return inputs.reduce((prev, curr) => prev + "-" + curr);
}


export const getArgCount = (queryStr) => {
  return (queryStr.match(/->/g) || []).length;
}