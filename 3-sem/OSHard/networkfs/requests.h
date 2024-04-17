#ifndef OS_2023_NETWORKFS_367382_REQUESTS_H
#define OS_2023_NETWORKFS_367382_REQUESTS_H
#include <linux/fs.h>
#include <linux/types.h>

struct entry {
  unsigned char entry_type;  // DT_DIR (4) or DT_REG (8)
  ino_t ino;
  char name[256];
};

struct entries {
  size_t entries_count;
  struct entry entries[16];
};

struct content {
  u64 content_length;
  char content[4096];
};

struct entry_info {
  unsigned char entry_type;  // DT_DIR (4) or DT_REG (8)
  ino_t ino;
};

int64_t networkfs_list_request(struct inode* node, struct entries** result);
int64_t networkfs_create_request(struct inode* parent, const char* name,
                                 struct entry_info* result);
int64_t networkfs_read_request(struct inode* node, struct content** result);
int64_t networkfs_write_request(struct inode* node, const char* data);
int64_t networkfs_link_request(struct inode* parent, struct inode* source,
                               const char* name);
int64_t networkfs_unlink_request(struct inode* parent, const char* name);
int64_t networkfs_rmdir_request(struct inode* parent, const char* name);
int64_t networkfs_lookup_request(struct inode* parent, const char* name,
                                 struct entry_info* result);

#endif  // OS_2023_NETWORKFS_367382_REQUESTS_H
