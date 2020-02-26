export const getArgNames = (numArgs) => {
    let argNames = [];
    for (let index = 0; index < numArgs; index++) {
      argNames = argNames.concat("arg" + index);
    }
    return argNames;
}