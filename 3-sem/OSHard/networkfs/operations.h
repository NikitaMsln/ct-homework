#ifndef OS_2023_NETWORKFS_367382_OPERATIONS_H
#define OS_2023_NETWORKFS_367382_OPERATIONS_H

#include <linux/fs.h>
#include <linux/fs_context.h>
#include <linux/kernel.h>

struct dentry *networkfs_lookup(struct inode *parent, struct dentry *child,
                                unsigned int flag);
int networkfs_iterate(struct file *filp, struct dir_context *ctx);
int networkfs_create(struct user_namespace *user_ns, struct inode *parent,
                     struct dentry *child, umode_t mode, bool b);
int networkfs_unlink(struct inode *parent, struct dentry *child);
int networkfs_mkdir(struct user_namespace *user_ns, struct inode *parent,
                    struct dentry *child, umode_t mode);
int networkfs_rmdir(struct inode *parent, struct dentry *child);

#endif  // OS_2023_NETWORKFS_367382_OPERATIONS_H
