#include "entrypoint.h"

extern struct inode_operations networkfs_inode_ops;

extern struct file_operations networkfs_file_ops;

struct file_system_type networkfs_fs_type = {
    .name = "networkfs",
    .init_fs_context = networkfs_init_fs_context,
    .kill_sb = networkfs_kill_sb};

struct fs_context_operations networkfs_context_ops = {.get_tree =
                                                          networkfs_get_tree};

int networkfs_init_fs_context(struct fs_context *fc) {
  fc->ops = &networkfs_context_ops;
  return 0;
}

struct inode *networkfs_get_inode(struct super_block *sb,
                                  const struct inode *parent, umode_t mode,
                                  int i_ino) {
  struct inode *inode;
  inode = new_inode(sb);

  if (inode != NULL) {
    inode->i_ino = i_ino;
    inode->i_op = &networkfs_inode_ops;
    inode->i_fop = &networkfs_file_ops;
    inode_init_owner(&init_user_ns, inode, parent, mode | S_IRWXUGO);
  }

  return inode;
}

int networkfs_fill_super(struct super_block *sb, struct fs_context *fc) {
  // Создаём корневую inode
  struct inode *inode = networkfs_get_inode(sb, NULL, S_IFDIR, 1000);
  // Создаём корень файловой системы
  sb->s_root = d_make_root(inode);
  sb->s_fs_info = kmalloc(strlen(fc->source) * sizeof(char), GFP_USER);
  strcpy((char *)sb->s_fs_info, fc->source);

  if (sb->s_root == NULL) {
    return -ENOMEM;
  }

  return 0;
}

int networkfs_get_tree(struct fs_context *fc) {
  int ret = get_tree_nodev(fc, networkfs_fill_super);

  if (ret != 0) {
    printk(KERN_ERR "networkfs: unable to mount: error code %d", ret);
  }

  return ret;
}

void networkfs_kill_sb(struct super_block *sb) {
  kfree(sb->s_fs_info);
  printk(KERN_INFO "networkfs: superblock is destroyed");
}

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Nikita Maslennikov");
MODULE_VERSION("0.01");

int networkfs_init(void) {
  if (register_filesystem(&networkfs_fs_type) == 0) {
    printk(KERN_INFO "Register complete!\n");
  } else {
    printk(KERN_INFO "Register failed\n");
  }
  return 0;
}

void networkfs_exit(void) {
  if (unregister_filesystem(&networkfs_fs_type) == 0) {
    printk(KERN_INFO "Unregister complete!\n");
  } else {
    printk(KERN_INFO "Unregister failed\n");
  }
}

module_init(networkfs_init);
module_exit(networkfs_exit);
