make -f Makefile.daomake macosx

mkdir Dao-$VERSION
cd Dao-$VERSION
fossil open --nested ../Dao.fossil
fossil close
cp ../manifest.uuid .

mkdir -p doc
cp -r ../doc/html doc/

cd modules
fossil open --nested ../../modules/DaoModules.fossil
fossil close

cd ../tools
fossil open --nested ../../tools/DaoTools.fossil
fossil close

cd ../../ && tar -zcf Dao-$VERSION.tgz Dao-$VERSION
