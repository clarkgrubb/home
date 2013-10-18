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

Username
--------

Subdirectories
--------------

==============  =========  =================================================================================
subdirectory    os         description
==============  =========  =================================================================================
Articles        all        PDFs; link to ~/Dropbox/Articles
Bin             all        put it first in PATH; a place to install executables without admin privilege
<Company>       all        work for <Company>
Desktop         all        files here appear as icons on the desktop
Documents       all        some applications keep their files here; including ~/Documents/Eclipse
                           and ~/Documents/Notebooks for iPython; editor files go in ~/Dropbox/Documents
Downloads       all        browsers should be configured to download files here; it is generally
Dropbox         host       Articles, Documents, Elements, Pictures
Env             all        virtualenv, rbenv environments
Etc             all        ~/Etc/UnicodeData.txt
Lang            all        subdirectories by programming language; if the code is put under version
                           control it is in Src
Library         mac        scripts run by Fastscripts must be in ~/Library/Scripts
Man             all        put it first in MANPATH; a place to install man pages w/o admin privilege
Movies          all        keep this empty
Music           all        iTunes and Amazon Cloud Player
Pictures        all        link to ~/Dropbox/Pictures; also Omnigraffle and image editor files;
                           PDFs created by R
Public                     If part of the home directory is made available on the local network,
                           make it this directory. E.g on Mac
                           ``System Preferences | Sharing | File Sharing``
Shared          all        a directory which is shared with guest VMs
Src             all        tarballs, git repos, isos for VM
Trash           all        On Mac, symlink to .Trash
Videos          win/linux  Windows/Ubuntu place for movies; keep this empty
==============  =========  =================================================================================
