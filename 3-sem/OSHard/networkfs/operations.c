#include "operations.h"

#include "entrypoint.h"
#include "requests.h"

struct inode_operations networkfs_inode_ops = {.lookup = networkfs_lookup,
                                               .create = networkfs_create,
                                               .unlink = networkfs_unlink,
                                               .mkdir = networkfs_mkdir,
                                               .rmdir = networkfs_rmdir};

struct file_operations networkfs_file_ops = {.iterate = networkfs_iterate};

struct dentry *networkfs_lookup(struct inode *parent, struct dentry *child,
                                unsigned int flag) {
  const char *name = child->d_name.name;
  struct entry_info result;
  int64_t error;
  if ((error = networkfs_lookup_request(parent, name, &result)) == 0) {
    struct inode *inode = networkfs_get_inode(
        parent->i_sb, parent, (result.entry_type == DT_REG) ? S_IFREG : S_IFDIR,
        result.ino);
    d_add(child, inode);
  } else {
    printk(KERN_ERR "networkfs: unable to lookup: error code %lld", error);
  }

  return NULL;
}

int networkfs_iterate(struct file *filp, struct dir_context *ctx) {
  struct dentry *dentry = filp->f_path.dentry;
  struct inode *inode = dentry->d_inode;

  loff_t record_counter = 0;

  while (true) {
    switch (ctx->pos) {
      case 0:
        dir_emit(ctx, ".", 1, inode->i_ino, DT_DIR);
        break;

      case 1:
        struct inode *parent_inode = dentry->d_parent->d_inode;
        dir_emit(ctx, "..", 2, parent_inode->i_ino, DT_DIR);
        break;

      case 2:
        struct entries *content = NULL;
        int64_t error;
        if ((error = networkfs_list_request(inode, &content)) == 0) {
          for (int i = 0; i < content->entries_count; i++, ctx->pos++) {
            char name[257];
            strncpy(name, content->entries[i].name, 256);
            name[256] = '\0';
            dir_emit(ctx, name, strlen(name), content->entries[i].ino,
                     content->entries[i].entry_type);
          }
          record_counter += content->entries_count;
        } else {
          printk(KERN_ERR "networkfs: unable to iterate: error code %lld",
                 error);
        }
        kfree(content);
        return record_counter;

      default:
        return record_counter;
    }

    ++record_counter;
    ++ctx->pos;
  }
}

int networkfs_create(struct user_namespace *user_ns, struct inode *parent,
                     struct dentry *child, umode_t mode, bool b) {
  const char *name = child->d_name.name;
  struct entry_info new_inode_info;
  new_inode_info.entry_type = (mode & S_IFDIR) ? DT_DIR : DT_REG;
  int64_t error;
  if ((error = networkfs_create_request(parent, name, &new_inode_info)) == 0) {
    struct inode *inode =
        networkfs_get_inode(parent->i_sb, parent, mode, new_inode_info.ino);
    d_add(child, inode);
  } else {
    printk(KERN_ERR "networkfs: unable to create: error code %lld", error);
    if (error != 9) return error;
  }

  return 0;
}

int networkfs_unlink(struct inode *parent, struct dentry *child) {
  const char *name = child->d_name.name;

  int64_t error;
  if ((error = networkfs_unlink_request(parent, name)) != 0) {
    printk(KERN_ERR "networkfs: unable to unlink: error code %lld", error);
    return error;
  }

  return 0;
}

int networkfs_mkdir(struct user_namespace *user_ns, struct inode *parent,
                    struct dentry *child, umode_t mode) {
  return networkfs_create(user_ns, parent, child, (mode & ~S_IFREG) | S_IFDIR,
                          1);
}

int networkfs_rmdir(struct inode *parent, struct dentry *child) {
  const char *name = child->d_name.name;

  int64_t error;
  if ((error = networkfs_rmdir_request(parent, name)) != 0) {
    printk(KERN_ERR "networkfs: unable to unlink: error code %lld", error);
    return error;
  }

  return 0;
}
