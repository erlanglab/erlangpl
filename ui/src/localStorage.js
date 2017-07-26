const LOCAL_STORAGE_KEY = 'erlang-performance-lab';

export function saveState(state) {
  try {
    const serialized = JSON.stringify(state);
    localStorage.setItem(LOCAL_STORAGE_KEY, serialized);
  } catch (err) {
    console.warn('Could not save state to localStorage: ', state);
  }
}

export function loadState() {
  try {
    const serialized = localStorage.getItem(LOCAL_STORAGE_KEY);
    if (serialized === null) {
      return undefined;
    }
    return JSON.parse(serialized);
  } catch (err) {
    return undefined;
  }
}

export function clearState() {
  try {
    localStorage.removeItem(LOCAL_STORAGE_KEY);
  } catch (err) {
    console.warn('Could not clear state in localStorage: ', err);
  }
}
