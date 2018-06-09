#!/bin/sh

if [[ -z $1 ]]; then
  echo "Usage:"
  echo "  ./make-with-reader.sh <module>"
  echo "    Deletes elm-stuff and ~/.elm and builds src/<module>.elm to src/<module>-reader.html"
  exit 1
fi

echo "Removing elm-stuff and ~/.elm"
rm -rf elm-stuff
rm -rf ~/.elm

elm make --reader "src/$1.elm" --output="target/$1-reader.html"
