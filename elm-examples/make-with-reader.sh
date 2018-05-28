#!/bin/sh

elm make --reader "src/$1.elm" --output="target/$1-reader.html"
