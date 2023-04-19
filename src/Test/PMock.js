export const store = function(v) {
  const argsList = [];
  const push = function(args) {
    argsList.push(args);
  }
  return {
    argsList,
    store: push
  }
}

export const _runRuntimeThrowableFunction = function(runtimeThrowableFunction) {
  let result;
  try {
    result = runtimeThrowableFunction();
  } catch (error) {
    return {
      hasError: true,
      error
    };
  }
  return {
    hasError: false,
    result
  }
}
