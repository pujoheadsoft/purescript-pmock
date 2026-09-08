export const store = function() {
  const calledParamsList = [];
  const storeCalledParams = function(params) {
    return function() {
      calledParamsList.push(params);
    }
  }
  return {
    calledParamsList,
    store: storeCalledParams
  }
}
