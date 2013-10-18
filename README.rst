----------------
HOME DIRECTORIES
----------------

Dot files for Darwin, Linux, Cygwin, and MinGW.

Installation
------------

::

    $ make

Location
--------

Early versions of Unix put user home directories in ``/usr``. This directory became co-opted for common files which were not essential to run the system. In Linux and BSD the user home directories are in ``/home``.

Each user's home directory is specified in ``/etc/passwd``. The system sets the environment variable ``HOME`` with this value. The C shell introduced tilde expansion as shortcut for the home directory in paths.

Mac OS X puts the home directories in ``/User``. It sets the HOME environment variable, but does not store user information in ``/etc/passwd``. The information is stored in the Directory Service which can be queried with the dscl command:

::

    dscl . -read /Users/$USER NFSHomeDirectory

DOS did not have a home directory, and Windows 95 had a single ``C:\My Documents directory``.

In Windows terminology, the home directory is called the user profile folder. Windows NT was the first multiuser version of Windows. It put user profile folders in ``C:\WINNT\Profiles``. Windows XP put them in ``C:\Documents and Settings`` and since Windows Vista they have been in ``C:\Users``.

Windows sets the environment variable ``%UserProfile%`` to the path of the current user profile folder.

Subdirectories
--------------

==============  =================================================================================
subdirectory    description
==============  =================================================================================
Articles        PDFs; link to ~/Dropbox/Articles
Bin             put it first in PATH; a place to install executables without admin privilege
<Company>       work for <Company>
Desktop         files here appear as icons on the desktop
Documents       some applications keep their files here; including ~/Documents/Eclipse;
                editor files go in ~/Dropbox/Documents
Downloads       browsers should be configured to download files here; it is generally
Dropbox         Articles, Documents, Elements, Pictures
Eclipse         move to ~/Documents/Eclipse?
Env             virtualenv, rbenv environments
Etc             ~/Etc/UnicodeData.txt
Lang            subdirectories by programming language; if the code is put under version
                control it is in Src
Library         On Mac, scripts run by Fastscripts must be in ~/Library/Scripts
Man             put it first in MANPATH; a place to install man pages w/o admin privilege
Movies          Mac place for movies; keep this empty
Music           used by iTunes and Amazon Cloud Player
Notebooks       for iPython notebooks; start the server in this directory; move into Documents
Pictures        link to ~/Dropbox/Pictures; also Omnigraffle and image editor files;
                PDFs created by R
Public          If part of the home directory is made available on the local network,
                make it this directory. E.g on Mac
                ``System Preferences | Sharing | File Sharing``
Shared          a directory which is shared with guest VMs
Src             tarballs, git repos, isos for VM
Trash           On Mac, symlink to .Trash
Videos          Windows/Ubuntu place for movies; keep this empty
==============  =================================================================================
