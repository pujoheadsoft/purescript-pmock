const recorders = new WeakMap();

export const registerRecorderImpl = fn => recorder => () => {
  recorders.set(fn, recorder);
};

export const hasRecorderImpl = fn => () => recorders.has(fn);

export const lookupRecorderImpl = fn => () => recorders.get(fn);

export const unregisterRecorderImpl = fn => () => {
  recorders.delete(fn);
};
