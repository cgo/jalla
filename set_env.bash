#
# If lapacke is not found on your system,
# you can (but don't have to) use this file
# to set paths to the one which can be built with get_lapacke.sh.
# Source this in bash; use analog commands in other shells.
#
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:`pwd`/lib
export LIBRARY_PATH=`pwd`/lib
export PKG_CONFIG_PATH=`pwd`/pkgconfig
