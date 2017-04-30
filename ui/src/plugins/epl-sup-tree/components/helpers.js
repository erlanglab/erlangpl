// @flow

export function trackTransforms(ctx) {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  let xform = svg.createSVGMatrix();
  ctx.getTransform = () => xform;

  const savedTransforms = [];
  const save = ctx.save;
  ctx.save = () => {
    savedTransforms.push(xform.translate(0, 0));
    return save.call(ctx);
  };

  const restore = ctx.restore;
  ctx.restore = () => {
    xform = savedTransforms.pop();
    return restore.call(ctx);
  };

  const scale = ctx.scale;
  ctx.scale = (sx, sy) => {
    xform = xform.scaleNonUniform(sx, sy);
    return scale.call(ctx, sx, sy);
  };

  const rotate = ctx.rotate;
  ctx.rotate = radians => {
    xform = xform.rotate(radians * 180 / Math.PI);
    return rotate.call(ctx, radians);
  };

  const translate = ctx.translate;
  ctx.translate = (dx, dy) => {
    xform = xform.translate(dx, dy);
    return translate.call(ctx, dx, dy);
  };

  const transform = ctx.transform;
  ctx.transform = (a, b, c, d, e, f) => {
    let m2 = svg.createSVGMatrix();
    m2.a = a;
    m2.b = b;
    m2.c = c;
    m2.d = d;
    m2.e = e;
    m2.f = f;
    xform = xform.multiply(m2);
    return transform.call(ctx, a, b, c, d, e, f);
  };

  const setTransform = ctx.setTransform;
  ctx.setTransform = (a, b, c, d, e, f) => {
    xform.a = a;
    xform.b = b;
    xform.c = c;
    xform.d = d;
    xform.e = e;
    xform.f = f;
    return setTransform.call(ctx, a, b, c, d, e, f);
  };

  const pt = svg.createSVGPoint();
  ctx.transformedPoint = (x, y) => {
    pt.x = x;
    pt.y = y;
    return pt.matrixTransform(xform.inverse());
  };

  return ctx;
}

export function addListeners(element, events) {
  Object.keys(events).forEach(key => {
    element.addEventListener(key, events[key], false);
  });
}

export function removeListeners(element, events) {
  Object.keys(events).forEach(key => {
    element.removeEventListener(key, events[key], false);
  });
}
