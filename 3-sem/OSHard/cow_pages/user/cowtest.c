//
// tests for copy-on-write fork() assignment.
//

#include "kernel/memlayout.h"
#include "kernel/types.h"
#include "user/user.h"

// allocate more than half of physical memory,
// then fork. this will fail in the default
// kernel, which does not support copy-on-write.
void simpletest() {
  uint64 phys_size = PHYSTOP - KERNBASE;
  int sz = (phys_size / 3) * 2;

  printf("simple: ");

  char *p = sbrk(sz);
  if (p == (char *)0xffffffffffffffffL) {
    printf("sbrk(%d) failed\n", sz);
    exit(-1);
  }

  for (char *q = p; q < p + sz; q += 4096) {
    *(int *)q = getpid();
  }

  int pid = fork();
  if (pid < 0) {
    printf("fork() failed\n");
    exit(-1);
  }

  if (pid == 0) exit(0);

  wait(0);

  if (sbrk(-sz) == (char *)0xffffffffffffffffL) {
    printf("sbrk(-%d) failed\n", sz);
    exit(-1);
  }

  printf("ok\n");
}

// three processes all write COW memory.
// this causes more than half of physical memory
// to be allocated, so it also checks whether
// copied pages are freed.
void threetest() {
  uint64 phys_size = PHYSTOP - KERNBASE;
  int sz = phys_size / 4;
  int pid1, pid2;

  printf("three: ");

  char *p = sbrk(sz);
  if (p == (char *)0xffffffffffffffffL) {
    printf("sbrk(%d) failed\n", sz);
    exit(-1);
  }

  pid1 = fork();
  if (pid1 < 0) {
    printf("fork failed\n");
    exit(-1);
  }
  if (pid1 == 0) {
    pid2 = fork();
    if (pid2 < 0) {
      printf("fork failed");
      exit(-1);
    }
    if (pid2 == 0) {
      for (char *q = p; q < p + (sz / 5) * 4; q += 4096) {
        *(int *)q = getpid();
      }
      for (char *q = p; q < p + (sz / 5) * 4; q += 4096) {
        if (*(int *)q != getpid()) {
          printf("wrong content\n");
          exit(-1);
        }
      }
      exit(-1);
    }
    for (char *q = p; q < p + (sz / 2); q += 4096) {
      *(int *)q = 9999;
    }
    exit(0);
  }

  for (char *q = p; q < p + sz; q += 4096) {
    *(int *)q = getpid();
  }

  wait(0);

  sleep(1);

  for (char *q = p; q < p + sz; q += 4096) {
    if (*(int *)q != getpid()) {
      printf("wrong content\n");
      exit(-1);
    }
  }

  if (sbrk(-sz) == (char *)0xffffffffffffffffL) {
    printf("sbrk(-%d) failed\n", sz);
    exit(-1);
  }

  printf("ok\n");
}

char junk1[4096];
int fds[2];
char junk2[4096];
char buf[4096];
char junk3[4096];

// test whether copyout() simulates COW faults.
void filetest() {
  printf("file: ");

  buf[0] = 99;

  for (int i = 0; i < 4; i++) {
    if (pipe(fds) != 0) {
      printf("pipe() failed\n");
      exit(-1);
    }
    int pid = fork();
    if (pid < 0) {
      printf("fork failed\n");
      exit(-1);
    }
    if (pid == 0) {
      sleep(1);
      if (read(fds[0], buf, sizeof(i)) != sizeof(i)) {
        printf("error: read failed\n");
        exit(1);
      }
      sleep(1);
      int j = *(int *)buf;
      if (j != i) {
        printf("error: read the wrong value\n");
        exit(1);
      }
      exit(0);
    }
    if (write(fds[1], &i, sizeof(i)) != sizeof(i)) {
      printf("error: write failed\n");
      exit(-1);
    }
  }

  int xstatus = 0;
  for (int i = 0; i < 4; i++) {
    wait(&xstatus);
    if (xstatus != 0) {
      exit(1);
    }
  }

  if (buf[0] != 99) {
    printf("error: child overwrote parent\n");
    exit(1);
  }

  printf("ok\n");
}

const char HACK[] = {
    0x13, 0x05, 0xb0, 0x07,  // li a0, 123
    0x89, 0x48,              // li a7, 2
    0x73, 0x00, 0x00, 0x00   // ecall
};

void rotest_victim() { sleep(5); }

typedef void (*func)();

// Checks that PTE access flags are not overwritten with incorrect values.
void rotest() {
  printf("ro: ");

  int pid1 = fork();

  if (pid1 > 0) {
    int xstatus;
    if (wait(&xstatus) != pid1) {
      printf("error: first child not found\n");
      exit(1);
    }

    if (xstatus == 123) {
      printf("error: parent memory corrupted\n");
      exit(1);
    } else if (xstatus == 1) {
      printf("failed\n");
      exit(1);
    } else if (xstatus != 0) {
      printf("error: unexpected first child exit code %d\n");
      exit(1);
    }

    printf("ok\n");
    return;
  }

  int pid2 = fork();

  if (pid2 > 0) {
    int xstatus;

    if (wait(&xstatus) != pid2) {
      printf("error: second child not found\n");
      exit(1);
    }

    rotest_victim();

    if (xstatus == 123) {
      printf("error: self memory corrupted\n");
      exit(1);
    } else if (xstatus != -1) {
      printf("error: unexpected second child exit code %d\n", xstatus);
      exit(1);
    }

    exit(0);
  }

  memmove(rotest_victim, HACK, sizeof(HACK));
  rotest_victim();

  exit(0);
}

int main(int argc, char *argv[]) {
  simpletest();

  // check that the first simpletest() freed the physical memory.
  simpletest();

  threetest();
  threetest();
  threetest();

  filetest();

  rotest();

  printf("ALL COW TESTS PASSED\n");

  exit(0);
}
