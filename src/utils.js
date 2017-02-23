export const syncGraphViewWithHistory = location => {
  if (location && location.pathname.match(/^\/graph/)) {
    const view = location.pathname
      .replace('/graph', '')
      .split('/')
      .filter(a => a !== '');
    return view;
  }

  return [];
};
