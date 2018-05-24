fwupd utility made for flashing motherboards.
this soft is AS IS, do not blame me if there is something went wrong afterall.

it launches a dmi gathering utility, searches through its output for motherboard version provided by .csv file and launches an updater with given firmware.

words about .csv file. if there is no .csv file utility will try to create it in \db subfolder.
the structure of .csv file:
the first row is a column names whatever you like. 
the other rows structure is:
motherboard version;path\to\updater.exe;updater params with %s placeholder for firmware path;path\to\fimware.bin
