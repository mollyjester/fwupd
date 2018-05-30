fwupd utility made for flashing motherboards.
this soft is AS IS, do not blame me if there is something went wrong afterall.

it scans a text dmi dump for motherboard version provided by .csv file and launches an updater with given firmware.

BASE STRUCTURE of .csv file:

mbversion;updater;param_fmt;firmware

where:

mbversion - motherboard version
updater - path to updater .exe file
param_fmt - params with %s placeholder to be replaced with firmware path
firmware - path to firmware file

START PARAMETERS

-b <path>: Path to folder with .csv file
-d <path>: Text dump file with DMI data
-h: Shows help
-i: Create clear well structured .csv file. Can be combined with -b
-s: Silent mode

if -i param is set the program will stop after creating a .csv file.
if there is no -d param set fwupd will stop with exit code -1. 
also if any error occured during execution the exitcode will be -1.

made with Lazarus. thanks!
also thanks to Vladimir Zhirov for CSVDocument unit.
