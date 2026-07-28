# Euler setup

Tried setting this up on Euler using spack; for some reason spack install r@4.5 doesn't work.

```bash
/cluster/software/stacks/2026-06/setup-env.sh
```

## TODO

For now, tried to `rv sync` from p3m x86_64 ubuntu jammy (<https://p3m.dev/cran/__linux__/jammy/latest>) using the following:

```bash
module load stack/2024-06 gcc gdal proj r/4.5 zsh cmake
cd ~/github-repos/evoland-experiments
rv sync
```

This complains that terra cannot be installed; however, the `stack/2024-06` does not contain sqlite, libcurl is not found, libtiff unclear.
**NEXT STEP** try to figure out deps within 2024-06 stack OR try anew creating my own minimal R spack env.

```log
Failed to install dependencies.
    Failed to install terra:
        * installing *source* package ‘terra’ ...
        ** this is package ‘terra’ version ‘1.9-34’
        ** package ‘terra’ successfully unpacked and MD5 sums checked
        ** using staged installation
        configure: CC: /cluster/software/stacks/2024-06/spack/opt/spack/linux-ubuntu22.04-x86_64_v3/gcc-11.4.0/gcc-12.2.0-bj2twcnwcownogkldo6ndfylxx5sqpbn/bin/gcc
        configure: CXX: /cluster/software/stacks/2024-06/spack/opt/spack/linux-ubuntu22.04-x86_64_v3/gcc-11.4.0/gcc-12.2.0-bj2twcnwcownogkldo6ndfylxx5sqpbn/bin/g++ -std=gnu++17
        checking for gdal-config... /cluster/software/stacks/2024-06/spack/opt/spack/linux-ubuntu22.04-x86_64_v3/gcc-12.2.0/gdal-3.7.3-rmhlatqryf2c4izusktcv63oj3abmime/bin/gdal-config
        checking gdal-config usability... yes
        configure: GDAL: 3.7.3
        checking GDAL version >= 2.0.1... yes
        checking for gcc... /cluster/software/stacks/2024-06/spack/opt/spack/linux-ubuntu22.04-x86_64_v3/gcc-11.4.0/gcc-12.2.0-bj2twcnwcownogkldo6ndfylxx5sqpbn/bin/gcc
        checking whether the C compiler works... yes
        checking for C compiler default output file name... a.out
        checking for suffix of executables...
        checking whether we are cross compiling... no
        checking for suffix of object files... o
        checking whether the compiler supports GNU C... yes
        checking whether /cluster/software/stacks/2024-06/spack/opt/spack/linux-ubuntu22.04-x86_64_v3/gcc-11.4.0/gcc-12.2.0-bj2twcnwcownogkldo6ndfylxx5sqpbn/bin/gcc accepts -g... yes
        checking for /cluster/software/stacks/2024-06/spack/opt/spack/linux-ubuntu22.04-x86_64_v3/gcc-11.4.0/gcc-12.2.0-bj2twcnwcownogkldo6ndfylxx5sqpbn/bin/gcc option to enable C11 features... none needed
        checking for stdio.h... yes
        checking for stdlib.h... yes
        checking for string.h... yes
        checking for inttypes.h... yes
        checking for stdint.h... yes
        checking for strings.h... yes
        checking for sys/stat.h... yes
        checking for sys/types.h... yes
        checking for unistd.h... yes
        checking for gdal.h... yes
        checking GDAL: linking with --libs only... yes
        checking GDAL: /cluster/software/stacks/2024-06/spack/opt/spack/linux-ubuntu22.04-x86_64_v3/gcc-12.2.0/gdal-3.7.3-rmhlatqryf2c4izusktcv63oj3abmime/share/gdal/pcs.csv readable... no
        checking GDAL: checking whether PROJ is available for linking:... yes
        checking GDAL: checking whether PROJ is available for running:... yes
        configure: GDAL: 3.7.3
        configure: pkg-config proj exists, will use it
        Package sqlite3 was not found in the pkg-config search path.
        Perhaps you should add the directory containing `sqlite3.pc'
        to the PKG_CONFIG_PATH environment variable
        Package 'sqlite3', required by 'proj', not found
        Package 'libtiff-4', required by 'proj', not found
        Package 'libcurl', required by 'proj', not found
        configure: using proj.h.
        configure: PROJ: 9.2.1
        checking PROJ: checking whether PROJ and sqlite3 are available for linking:... no
        configure: error: libproj or sqlite3 not found in standard or given locations.
        *** Installing this package from source requires the prior
        *** installation of external software, see for details
        *** https://r-spatial.github.io/sf/#installing
        ERROR: configuration failed for package ‘terra’
        * removing ‘/tmp/.tmpLeUvtD/terra’
```
