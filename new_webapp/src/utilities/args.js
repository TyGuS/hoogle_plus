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
          id: element.id,
      }
  });
}