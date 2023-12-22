export const store = function(_) {
  return function() {
    const calledParamsList = [];
    const storeCalledParams = function(params) {
      calledParamsList.push(params);
    }
    return {
      calledParamsList,
      store: storeCalledParams
    }
  }
}
