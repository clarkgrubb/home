src_dir=`dirname $0`
cd $src_dir
home_dir=$1
if [[ $# < 1 ]]; then echo "USAGE: install.sh DIR"; exit -1; fi
if [ ! -d $home_dir ]; then echo "NOT A DIRECTORY: " $home_dir; exit -1; fi
rm -rf $home_dir/.emacs.d
cp -R .emacs.d $home_dir
cp .bash_profile .zprofile .zshrc .zshenv $home_dir