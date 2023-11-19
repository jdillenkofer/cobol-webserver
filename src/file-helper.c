#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>

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
