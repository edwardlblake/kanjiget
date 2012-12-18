@echo off
SET PATH=%PATH%;C:\Program Files (x86)\Racket;C:\Program Files\Racket

mkdir out

raco make --vv ..\src\kanjiget.rkt
raco exe -o Kanjiget.exe --gui --3m --vv -- ..\src\kanjiget.rkt
raco distribute -v -- out\ Kanjiget.exe
del Kanjiget.exe

