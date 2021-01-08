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

    $ ./find_all_modules.sh | ./get_kernel_modules.py

> This will likely report many unfound modules, this is fine. Many devices do not have kernel drivers.

### Nvidia

Compile the kernel with the following settings:

    [*] Enable loadable module support --->

    [*] MTRR (Memory Type Range Register) support

    Device Drivers --->
        Graphics support --->
            [*] VGA Arbitration

    Device Drivers --->
        Character devices --->
            [*] IPMI top-level message handler

    Device Drivers --->
        Graphics support --->
            Frame buffer Devices --->
                <*> Support for frame buffer devices --->
                < > nVidia Framebuffer Support
                < > nVidia Riva support

    Device Drivers --->
        Graphics support --->
            < > Nouveau (nVidia) cards

## Nvidia Drivers

Linus is right to hate Nvidia :D, configuring drivers is a pain!

Ensure `VIDEO_CARDS="nvidia"` is in `/etc/portage/make.conf` and `/usr/src/linux` points to the current kernel sources:

    root # eselect kernel list
    Available kernel symlink targets:
      [1]   linux-3.7.10-gentoo *
      [2]   linux-3.7.9-gentoo

Set with:

    root # eselect kernel set 1

Install the drivers:

    root # emerge --ask x11-drivers/nvidia-drivers

> The drivers can be installed with the tools USE flag. This will install nvidia-settings,
> a handy graphical tool for monitoring and configuring several
> aspects of the NVIDIA card.

> Every time a kernel is built, it is necessary to reinstall the NVIDIA kernel modules.
> An easy way to rebuild the modules installed by ebuilds (such as x11-drivers/nvidia-drivers)
> is to run `emerge @module-rebuild`.

More info [here](https://wiki.gentoo.org/wiki/NVIDIA/nvidia-drivers).
