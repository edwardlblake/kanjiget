export PATH=$PATH:/Developer/Racket\ v5.3.1/bin/

mkdir out

raco make --vv ../src/kanjiget.rkt
raco exe -o Kanjiget.app --gui --3m --vv -- ../src/kanjiget.rkt
# TODO: --ico kanjiget.icns
# etc.

raco distribute -v -- out Kanjiget.app
cp -R ../data out/Kanjiget.app/Contents/MacOS/data

rm -r Kanjiget.app/

