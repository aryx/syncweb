#!!!!You need to source me with "source env.sh" from the _RIGHT_ directory!!!!
if [ ! -r main.ml ]
    then echo "There is no main.ml here. 
Are you sure you run this script from the source directory ?
";
fi

# To run. To find the config/ files.
echo setting SYNCWEB_HOME
export SYNCWEB_HOME=`pwd`

