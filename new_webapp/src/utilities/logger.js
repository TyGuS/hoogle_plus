import createLog from 'localstorage-logger';

export const log = createLog({
  logName: 'hoogle-plus-study',
  maxLogSizeInBytes: 5 * 1024 * 1024 // 5MB
});