export const store = function() {
  const calledParamsList = [];
  const storeCalledParams = function(params) {
    calledParamsList.push(params);
  }
  return {
    calledParamsList,
    store: storeCalledParams
  }
}
