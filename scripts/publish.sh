#!bash

PROJECT=agda2train
CABAL=$PROJECT.cabal
curVersion=$(grep "^version" "$CABAL" | cut -d: -f2 | tr -d "[:blank:]")
prevVersion=$(git show HEAD^:"$CABAL" | grep "^version" | cut -d: -f2 | tr -d "[:blank:]")

[ $curVersion == $prevVersion ] \
  && echo "Version has not changed, not publishing to Hackage." && exit 1

read -p "Username: " HACKAGE_USERNAME
read -s -p "Password: " HACKAGE_PASSWORD

echo "Version has changed, publishing to Hackage."
cabal sdist
cabal upload -u $HACKAGE_USERNAME -p $HACKAGE_PASSWORD \
  --publish dist-newstyle/sdist/$PROJECT-$curVersion.tar.gz
cabal haddock --haddock-for-hackage --haddock-hyperlink-source --haddock-executables
DOCS=$PROJECT-$curVersion-docs
untar dist-newstyle/$DOCS.tar.gz
mv $DOCS/$PROJECT/* $DOCS/ && rm -r $DOCS/$PROJECT/
tar cv --format=ustar -f $DOCS.tar $DOCS
cabal upload -u $HACKAGE_USERNAME -p $HACKAGE_PASSWORD \
  --publish -d $DOCS.tar
