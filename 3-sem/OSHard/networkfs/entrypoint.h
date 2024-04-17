#ifndef OS_2023_NETWORKFS_367382_ENTRYPOINT_H
#define OS_2023_NETWORKFS_367382_ENTRYPOINT_H
#include <linux/fs.h>
#include <linux/fs_context.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>

int networkfs_init_fs_context(struct fs_context *fc);
struct inode *networkfs_get_inode(struct super_block *sb,
                                  const struct inode *parent, umode_t mode,
                                  int i_ino);
int networkfs_fill_super(struct super_block *sb, struct fs_context *fc);
int networkfs_get_tree(struct fs_context *fc);
void networkfs_kill_sb(struct super_block *sb);

#endif  // OS_2023_NETWORKFS_367382_ENTRYPOINT_H
