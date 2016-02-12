#!/usr/bin/env bash

if [ ! -x "$(which sbt)" ] ; then
  brew install sbt
fi

case "$1" in
  test)
    sbt test
    ;;
  build)
    sbt compile
    ;;
  run)
    shift
    sbt "run $*"
    ;;
  *)
    echo "Usage"
    echo "./go.sh test"
    echo "./go.sh build"
    echo "./go.sh run +|- num [num]"
    ;;
esac
