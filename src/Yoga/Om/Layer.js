let counter = 0;
export const nextId = () => ++counter;

export const tryGetScope = (ctx) => ctx.scope || null;
