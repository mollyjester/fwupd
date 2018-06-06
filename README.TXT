When setting up a path params for utility (in .csv also) keep in mind
not to use slashes before a path.

wrong:
\updater\path\file.exe
\firmware\path\file.bin
\db\mbver.csv
\dumpfile.txt

right:
updater\path\file.exe
firmware\path\file.bin
db\mbver.csv
dumpfile.txt

Look for examples in fwupd.bat