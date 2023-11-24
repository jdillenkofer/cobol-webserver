#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <assert.h>

int is_directory(int fd){
  struct stat fileInfo;
  int ret = fstat(fd, &fileInfo);
  return S_ISDIR(fileInfo.st_mode) != 0;
}

static int has_prefix(const char *str, const char* pre) {
  size_t lenstr = strlen(str);
  size_t lenpre = strlen(pre);
  return lenstr < lenpre ? 0 : memcmp(pre, str, lenpre) == 0;
}

int is_filepath_subpath_of_cwd(char* filepath) {
  char* cwd = getcwd(NULL, 0);
  if (cwd == NULL) {
    return 0;
  }
  const char* resolved_path = realpath(filepath, NULL);
  if (resolved_path == NULL) {
    free((void*) cwd);
    return 0;
  }
  int cwd_is_prefix_of_filepath = has_prefix(resolved_path, cwd);
  free((void*) resolved_path);
  free((void*) cwd);
  return cwd_is_prefix_of_filepath;
}

int is_macos() {
  #ifdef __APPLE__
  return 1;
  #else
  return 0;
  #endif
}

#ifdef __APPLE__
ssize_t sendfile64(int out_fd, int in_fd, off_t* offset, size_t count) {
  // We always use NULL for offset
  // No need to support the other variant
  assert(offset != NULL);

  #define MIN(x, y) (((x) < (y)) ? (x) : (y))

  #define BUFFERSIZE 4096
  char buffer[BUFFERSIZE];
  ssize_t totalBytesWritten = 0;
  while (totalBytesWritten < count) {
    ssize_t bytesRead = read(in_fd, buffer, MIN(BUFFERSIZE - totalBytesWritten, BUFFERSIZE));
  #undef BUFFERSIZE
  #undef MIN
    if (bytesRead == 0) {
      return totalBytesWritten;
    }
    if (bytesRead == -1) {
      return bytesRead;
    }

    ssize_t totalLoopBytesWritten = 0;
    do {
      ssize_t bytesWritten = write(out_fd, buffer + totalLoopBytesWritten, bytesRead - totalLoopBytesWritten);
      if (bytesWritten == -1) {
        return bytesWritten;
      }
      totalLoopBytesWritten += bytesWritten;
    }
    while (bytesRead != totalLoopBytesWritten);
    
    totalBytesWritten += totalLoopBytesWritten;
  }
  return totalBytesWritten;
}
#endif