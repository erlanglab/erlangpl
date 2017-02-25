export const syncGraphViewWithHistory = location => {
  if (location && location.pathname.match(/^\/traffic/)) {
    const view = location.pathname
      .replace('/traffic', '')
      .split('/')
      .filter(a => a !== '');
    return view;
  }

  return [];
};
