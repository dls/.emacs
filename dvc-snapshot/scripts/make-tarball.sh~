#! /bin/sh

tempdir=/tmp/dvc-tarball.$$
workdir=$(pwd)
tarballname=$1

if [[ $tarballname = "" ]]; then
    tarballname=$(basename $workdir).tar.gz
fi

rm -fr $tempdir
mkdir $tempdir

(cd lisp; make dvc-version.el)
(cd texinfo; make dvc-version.texinfo)

bzr branch . $tempdir/$tarballname/
for file in lisp/dvc-version.el texinfo/dvc-version.texinfo
do
    cp $file $tempdir/$tarballname/$file
done

cd $tempdir ; tar -cvzf "${workdir}/$tarballname" $tarballname
rm -fr $tempdir
