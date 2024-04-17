#include "requests.h"

#include <linux/kernel.h>

#include "http.h"

int64_t networkfs_list_request(struct inode* node, struct entries** result) {
  char buf[40];
  sprintf(buf, "%ld", node->i_ino);
  *result = (struct entries*)kmalloc(sizeof(struct entries), GFP_USER);
  return networkfs_http_call((char*)node->i_sb->s_fs_info, "list",
                             (void*)*result, sizeof(struct entries), 1, "inode",
                             buf);
}

int64_t networkfs_create_request(struct inode* parent, const char* name,
                                 struct entry_info* result) {
  const char* dir = "directory";
  const char* file = "file";
  char buf[40];
  sprintf(buf, "%ld", parent->i_ino);
  return networkfs_http_call((char*)parent->i_sb->s_fs_info, "create",
                             (void*)&result->ino, sizeof(ino_t), 3, "parent",
                             buf, "name", name, "type",
                             (result->entry_type == DT_DIR) ? dir : file);
}

int64_t networkfs_read_request(struct inode* node, struct content** result) {
  char buf[40];
  sprintf(buf, "%ld", node->i_ino);
  *result = (struct content*)kmalloc(sizeof(struct content), GFP_USER);
  return networkfs_http_call((char*)node->i_sb->s_fs_info, "read",
                             (void*)*result, sizeof(struct content), 1, "inode",
                             buf);
}

int64_t networkfs_write_request(struct inode* node, const char* data) {
  char buf[40];
  sprintf(buf, "%ld", node->i_ino);
  return networkfs_http_call((char*)node->i_sb->s_fs_info, "write", (void*)0, 0,
                             2, "inode", buf, "content", data);
}

int64_t networkfs_link_request(struct inode* parent, struct inode* source,
                               const char* name) {
  char parent_buf[40];
  sprintf(parent_buf, "%ld", parent->i_ino);
  char source_buf[40];
  sprintf(source_buf, "%ld", source->i_ino);
  return networkfs_http_call((char*)parent->i_sb->s_fs_info, "link", (void*)0,
                             0, 3, "parent", parent_buf, "source", source_buf,
                             "name", name);
}

int64_t networkfs_unlink_request(struct inode* parent, const char* name) {
  char buf[40];
  sprintf(buf, "%ld", parent->i_ino);
  return networkfs_http_call((char*)parent->i_sb->s_fs_info, "unlink", (void*)0,
                             0, 2, "parent", buf, "name", name);
}

int64_t networkfs_rmdir_request(struct inode* parent, const char* name) {
  char buf[40];
  sprintf(buf, "%ld", parent->i_ino);
  return networkfs_http_call((char*)parent->i_sb->s_fs_info, "rmdir", (void*)0,
                             0, 2, "parent", buf, "name", name);
}

int64_t networkfs_lookup_request(struct inode* parent, const char* name,
                                 struct entry_info* result) {
  char buf[40];
  sprintf(buf, "%ld", parent->i_ino);
  return networkfs_http_call((char*)parent->i_sb->s_fs_info, "lookup",
                             (void*)result, sizeof(struct entry_info), 2,
                             "parent", buf, "name", name);
}
