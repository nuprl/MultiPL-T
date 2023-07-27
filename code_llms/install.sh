#!/bin/bash
set -x
set -e

# Check that the script is being run from the directory in which this file is
# located.
if [ ! -f "install.sh" ]; then
    echo "Please run install.sh from the directory in which it is located."
    exit 1
fi

# Check that we are in a Python virtual environment
if [ -z "$VIRTUAL_ENV" ]; then
    echo "Please run install.sh from within a Python virtual environment."
    exit 1
fi

SITE_PACKAGES=`python3 -c 'import sysconfig; print(sysconfig.get_paths()["purelib"])'`
ln -s `pwd` $SITE_PACKAGES/code_llms
