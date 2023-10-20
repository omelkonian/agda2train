#!bash
set -e

PROJECT=agda2train
CABAL=$PROJECT.cabal
curVersion=$(grep "^version" "$CABAL" | cut -d: -f2 | tr -d "[:blank:]")
prevVersion=$(git show HEAD^:"$CABAL" | grep "^version" | cut -d: -f2 | tr -d "[:blank:]")

[ $curVersion == $prevVersion ] \
  && echo "Version has not changed, not publishing to Hackage." && exit 1 \
  || echo "Version has changed, publishing to Hackage."

dir=$(mktemp -d dist.XXXXXX)
trap 'rm -r "$dir"' EXIT

read -p "Username: " HACKAGE_USERNAME
read -s -p "Password: " HACKAGE_PASSWORD

if [ "$@" != "--only-docs" ]; then
  echo "Uploading package: $PROJECT-$curVersion"
  cabal sdist --builddir="$dir"
  cabal upload -u $HACKAGE_USERNAME -p $HACKAGE_PASSWORD \
    --publish $dir/sdist/$PROJECT-$curVersion.tar.gz \
    || echo "** Version $curVersion already uploaded."
fi

echo "Uploading package documentation: $PROJECT-$curVersion"
cabal haddock --builddir="$dir" --haddock-for-hackage --haddock-hyperlink-source
cabal upload -u $HACKAGE_USERNAME -p $HACKAGE_PASSWORD \
  --publish -d $dir/$PROJECT-$curVersion-docs.tar.gz \
  || echo "** Documentation for version $curVersion already uploaded."
