// Test that system is able to allocate at least 2560 processes and exit
// gracefully.

#include "user/user.h"

#define N 4096
#define NMIN 2560

void forktest(void) {
  int n, pid;

  int fd[2];
  char buf[1];
  if (pipe(fd) != 0) {
    printf("pipe() failed\n");
    exit(1);
  }

  printf("forktest\n");

  for (n = 0; n < N; n++) {
    pid = fork();
    if (pid < 0) break;
    if (pid == 0) {
      close(fd[1]);
      read(fd[0], buf, 1);
      exit(0);
    }
  }

  close(fd[0]);
  close(fd[1]);

  if (n == N) {
    // 4096 processes take more space than available RAM
    printf("fork claimed to work %d times!\n", N);
    exit(1);
  }

  printf("forked %d processes", n);
  if (n < NMIN) {
    printf(", not enough\n");
    exit(1);
  }
  printf("\n");

  for (; n > 0; n--) {
    if (wait(0) < 0) {
      printf("wait stopped early\n");
      exit(1);
    }
  }

  if (wait(0) != -1) {
    printf("wait got too many\n");
    exit(1);
  }

  printf("forktest: OK\n");
}

int main(void) {
  forktest();
  exit(0);
}
