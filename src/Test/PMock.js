export const store = function(v) {
  const calledParamsList = [];
  const storeCalledParams = function(params) {
    calledParamsList.push(params);
  }
  return {
    calledParamsList,
    store: storeCalledParams
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
