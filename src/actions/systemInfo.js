// @flow

export const UPDATE_SYSTEM_INFO = 'UPDATE_SYSTEM_INFO';

export const updateSystemInfo = (info: any) => ({
  type: UPDATE_SYSTEM_INFO,
  info,
});
