#!!!!You need to source me with "source env.sh" from the _RIGHT_ directory!!!!
if [ ! -r main.ml ]
    then echo "There is no main.ml here. 
Are you sure you run this script from the source directory ?
";
fi

# To compile the source for uiuc
echo setting PATH
export PATH=/home/pad/packages/bin:/home/pad/packages/sbin:$PATH
echo setting LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/home/pad/packages/lib:$LD_LIBRARY_PATH

# To run. To find the config/ files.
echo setting SYNCWEB_HOME
export SYNCWEB_HOME=`pwd`

