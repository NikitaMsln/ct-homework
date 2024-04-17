#ifndef KERNEL_LIST_H
#define KERNEL_LIST_H

struct list {
  struct list* next;
  struct list* prev;
};

#endif
