#! /usr/bin/sh
BASENAME=$(dirname "$0")

cd $BASENAME

rm -fr test-data
mkdir test-data

for i in {0..360}; do
  NEXT_DATE=$(date +%Y-%m-%d -d "$DATE - $i day");
  echo "test-data/x_$NEXT_DATE.sql";
  touch "test-data/x_$NEXT_DATE.sql";
done
