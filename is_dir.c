#include <sys/stat.h>

int is_directory(int fd){
  struct stat fileInfo;
  int ret = fstat(fd, &fileInfo);
  return S_ISDIR(fileInfo.st_mode) != 0;
}
