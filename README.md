# MVT for APL system Version 2.00

OS/360-MVT 21.8F customized for use with APL\360 Version 1 Modification Level 1

by Juergen Winkelmann, winkelmann@id.ethz.ch, March 29, 2013

Please see folder doc of this distribution for full documentation and copyright information.

# Installing

Installation instructions are in the User Manual found in the doc/ folder. However, you might need to install a 64bit PuTTy: 
```
sudo apt-get install putty
```
The included PuTTy executables were 32bit. 

All you need to do is

* Install the APL font, predefined connection parameters and very few minor settings on your Windows or Linux system (and that's the only manual work you need to do!)
* Unzip the source you downloaded from the museum
* Start the system by executing a single script.
* Sit back, relax and wait for two APL\360 windows to pop up

There are no interactions with Hercules or OS/360 necessary. The need for knowhow in these areas has been completely eliminated, making MVT for APL a true ready to use APL\360 system while still not containing a single byte of APL\360 licensed code.


# Features

* An OS/360 MVT 21.8F system designed for use with APL\360.
* An automated procedure to build APL\360 and the MVT supervisor calls it relies on from source.
* An automated MVT operator (APLPILOT).
* IBM 1052-7 and IBM 2741 terminal support through an adapted version of the Hercules System/370, ESA/390, and z/Architecture Emulator.
* An OPFNS workspace supporting APL\360 operations.
* A comprehensive public workspace library.
* Scripts and related software combining all components.
* The MVT for APL Version 2.00 User’s Manual.


# Acknowledgements

* Juergen Winkelmann, [the author of this release](https://hercules-390.yahoogroups.narkive.com/e7ZVzKKL/running-apl-360-on-os-360-mvt-21-8f-mvt-for-apl-version-2-00-available)
* Len Shustek, Chairman of the Board of Trustees, Computer History Museum, Mountain View, went through 10 years of persistent negotiations with IBM to make the impossible happen: Obtain a license to make the APL\360 source code available to the public for non-commercial use.
* Brian and Barry Silverman shared many insights they gained during their 1998 APL\360 resurrection project, especially on how to handcraft a library structure on DASD. This greatly helped overcome the chicken-egg problem of how to create an initial library setup without having the original distribution tape available.
* Catherine Lathwell spread the word when the APL\360 source became available and provided the connection to Len Shustek.
* Max H. Parke provided the IBM 2741 terminal support: Having created the Hercules device driver for the 2741 and other asynchronous terminals attached to an IBM 2703 TCU, Max is most knowledgeable when it comes to implementing emulations of complex communications equipment and topologies. With the availability of APL\360 running on OS/360-MVT under Hercules he added the functionality and features required by APL\360 to the 2741 device support to provide a true generic implementation of the most commonly used APL terminal back in the APL\360 era.
* Tony Harminc made information about APL internal structures (workspaces, libraries, control blocks) available that was very helpful in creating the initial library setup.
* Jay Maynard's cookbook style MVT system generation instructions enabled getting up to speed with OS/360-MVT quickly.
* Kevin Leonard provides a lot of tools (namely PDS, COMPARE and REVIEW) and usability enhancements for OS/360-MVT on his website that helped transforming the MVT system initially meant as a runtime only environment into a veritable development system.
* Phil Roberts extracted the public library workspaces from APL\MTS. This was a major effort including installation of MTS and APL\MTS, exploring how to execute APL\MTS and how to print output, unlocking hidden functions, putting together some 4,000 APL statements to extract and finally print the full contents of the workspaces.
* and last but not least the Hercules developers who made this effort possible by providing this great piece of software.
