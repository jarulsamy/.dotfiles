# Gentoo

Specific information related to my personal Gentoo configuration.

This is mostly here just for my own reference and most of the information can be
found from the [Gentoo Wiki](https://wiki.gentoo.org/) in a better and more
comprehensive way. This information is meant as a quick refernece only.

## Kernel Configuration

This included scripts in this folder can be helpful
when configuring a kernel for a brand new system.

- `find_all_modules.sh` - Get all the currently loaded kernel module file names.
- `get_kernel_modules.py` - Convert the name filenames of kernel modules into
  kernel configuration parameters.

Sample usage (run as **root**):

```bash

$ ./find_all_modules.sh | ./get_kernel_modules.py

```

> This will likely report many unfound modules, this is fine. Many devices do not have kernel drivers.

## Xorg / Nvidia

Linus is right to hate Nvidia :D.
