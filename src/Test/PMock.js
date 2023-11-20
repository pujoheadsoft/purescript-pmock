export const store = function(_) {
  const calledParamsList = [];
  const storeCalledParams = function(params) {
    calledParamsList.push(params);
  }
  return {
    calledParamsList,
    store: storeCalledParams
  }
}
