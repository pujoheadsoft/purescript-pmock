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
