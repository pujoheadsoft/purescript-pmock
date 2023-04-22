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
