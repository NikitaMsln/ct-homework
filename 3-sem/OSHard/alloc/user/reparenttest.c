// A slightly simplified forktest edition, which tries to spawn many
// childrens and abandons all of them. Children should be reparented
// to initproc and gracefully waited.

#include "user/user.h"

void allocate(int cnt) {
  int n, pid;

  int fd[2];
  char buf[1];
  if (pipe(fd) != 0) {
    printf("pipe() failed\n");
    exit(1);
  }

  for (n = 0; n < cnt; n++) {
    pid = fork();
    if (pid < 0) break;
    if (pid == 0) {
      close(fd[1]);
      read(fd[0], buf, 1);
      exit(0);
    }
  }

  if (n < cnt) {
    exit(1);
  }

  exit(0);
}

void small(const char* s) {
  for (int test = 0; test < 20; test++) {
    printf("%s: small test %d\n", s, test + 1);

    // Small test: 10 parallel masters allocate 32 processes
    // and then quit.

    int i;
    int failed = 0;
    int uncompleted = 0;
    for (i = 0; i < 10; i++) {
      int pid = fork();

      if (pid < 0) {
        printf("%s: could not fork\n", s);
        failed = 1;
        break;
      }

      if (pid == 0) {
        allocate(32);
      }
    }

    for (int j = 0; j < i; j++) {
      int status;
      int c = wait(&status);
      if (c < 0) {
        printf("%s: lost children\n", s);
        failed = 1;
      }
      if (status != 0 && status != 1) {
        printf("%s: unexpected exit status %d\n", s, status);
        failed = 1;
      }
      uncompleted += status;
    }

    if (failed) {
      exit(1);
    }

    if (uncompleted > 0) {
      printf("%s: %d masters failed to allocate processes\n", s, uncompleted);
    }
  }
}

void big(const char* s) {
  for (int test = 0; test < 3; test++) {
    printf("%s: large test %d\n", s, test + 1);

    // Large test: single master allocates as much processes
    // as it can, then quits.

    int pid = fork();
    if (pid < 0) {
      printf("%s: could not fork\n", s);
      exit(1);
    }

    if (pid == 0) {
      allocate(5555);
    }

    int status;
    int c = wait(&status);
    if (c < 0) {
      printf("%s: lost children\n", s);
      exit(1);
    }
    if (status == 0) {
      printf("%s: allocated all processes for big test\n", s);
      exit(1);
    }
    sleep(30);  // sleep a bit to unload some processes
  }
}

int main(int argc, const char** argv) {
  if (argc != 2) {
    printf("usage: %s (small | big)\n", argv[0]);
    exit(1);
  }

  if (strcmp(argv[1], "small") == 0) {
    small(argv[0]);
  } else if (strcmp(argv[1], "big") == 0) {
    big(argv[0]);
  } else {
    printf("usage: %s (small | big)\n", argv[0]);
    exit(1);
  }

  printf("%s: OK\n", argv[0]);
  return 0;
}
