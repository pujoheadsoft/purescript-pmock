const scopeStack = [];

export const openScope = () => {
  const scope = [];
  scopeStack.push(scope);
  return scope;
};

export const closeScope = scope => () => {
  if (scopeStack[scopeStack.length - 1] !== scope) {
    throw new Error("PMock scopes must be closed in nesting order.");
  }
  scopeStack.pop();
  return scope;
};

export const currentScopeImpl = just => nothing => () => {
  const scope = scopeStack[scopeStack.length - 1];
  return scope === undefined ? nothing : just(scope);
};

export const addVerification = scope => verification => () => {
  scope.push(verification);
};

export const deferVerification = create => () => create()();
