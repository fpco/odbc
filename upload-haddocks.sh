#!/usr/bin/env bash
if [ "$1" == "" ]; then
echo "Need a package name."
exit
fi

set -xe
STACKVER=$(cat $1.cabal|grep '^version:'|head -1|awk '{print $2}')
STACKDOCDIR=$1-$STACKVER-docs
stack haddock --haddock-arguments "--theme doc/" --force-dirty
sh doc/patch.sh
rm -rf _release/$STACKDOCDIR
mkdir -p _release

for i in $(ls $(stack path --local-doc-root)/$1-$STACKVER/*.html); do sed -i.bak 's/<head>/<head><script src="highlight.pack.js"><\/script><script src="https:\/\/code.jquery.com\/jquery-3.3.1.slim.min.js" integrity="sha256-3edrmyuQ0w65f8gfBsqowzjJe2iM6n0nKciPUp8y+7E=" crossorigin="anonymous"><\/script><script src="init.js"><\/script>/' $i; done


cp -r $(stack path --local-doc-root)/$1-$STACKVER _release/$STACKDOCDIR
sed -i 's/href="\.\.\/\([^/]*\)\//href="..\/..\/\1\/docs\//g' _release/$STACKDOCDIR/*.html
(cd _release && tar cvz --format=ustar -f $STACKDOCDIR.tar.gz $STACKDOCDIR)

curl -X PUT \
     -H 'Content-Type: application/x-tar' \
     -H 'Content-Encoding: gzip' \
     -u ChrisDone \
     --data-binary "@_release/$STACKDOCDIR.tar.gz" \
     "https://hackage.haskell.org/package/$1-$STACKVER/docs"
