#!/bin/bash
# 
# Get LAPACKE and build it. This needs SCons installed,
# you can get it from http://www.scons.org
# or on Debian-based systems with 
#  sudo apt-get install scons
#
wget http://www.netlib.org/lapack/lapacke.tgz
tar xzf lapacke.tgz
cd lapacke
scons -j 4
cd ..
echo
echo
echo "Don't forget to source set_env.bash to set LD_LIBRARY_PATH and LIBRARY_PATH!"
echo