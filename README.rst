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

Early versions of Unix put user home directories in ``/usr``.  In Linux and BSD the user home directories are in ``/home``.

A Unix user's home directory is specified in ``/etc/passwd``. This is the working directory when the user logs in, and the system sets the environment variable ``HOME`` to this value. The C shell introduced tilde expansion as shortcut for the home directory in paths.

Mac OS X puts the home directories in ``/User``. It sets the HOME environment variable, but does not store user information in ``/etc/passwd``. The information is stored in the Directory Service which can be queried with the dscl command:

::

    dscl . -read /Users/$USER NFSHomeDirectory

DOS did not have a home directory, and Windows 95 had a single ``C:\My Documents directory``.

In Windows terminology, the home directory is called the user profile folder. Windows NT was the first multiuser version of Windows. It put user profile folders in ``C:\WINNT\Profiles``. Windows XP put them in ``C:\Documents and Settings`` and since Windows Vista they have been in ``C:\Users``.

Windows sets the environment variable ``%UserProfile%`` to the path of the current user profile folder.

Username
--------

When an Ubuntu instance is created, the setup procedure prompts for the user's full name, login name, and password.  The ``adduser`` command can be used to create more users.

On ``Mac OS X`` the setup procedure prompts for the user's full name, login name, and password.  User accounts are managed at ``System Preferences | Users & Groups``.

*Windows...*

*Avatars*

Subdirectories
--------------

I like a home directory which only contains subdirectories and hidden files.  Desktop operating systems create some subdirectories for you, and since they capitalize those subdirectory names, it is best to follow that convention.

The Mac filesystem (HFS+), incidentally, is not case sensitive, but it remembers the case that was used when a file is created and uses it for display.

For the benefit of the command line, we discourage file names which contain spaces.  For the benefit of tab completion, try to choose names that are uniquely specified by their first two letters.  But note that desktops create two directories ``Documents`` and ``Downloads`` which require three letters to uniquely specify.

Windows some home directory subdirectories which break the no-space rule and the unique-two-letter-prefix rule, e.g. ``My Documents``, ``My Pictures``.  One can rename these directories, but to avoid problems does one have to fiddle with the registry?

::

    HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders


=================  =========  =================================================================================
subdirectory       os         description
=================  =========  =================================================================================
AppData            win
Application Data   win
Articles           all        PDFs; link to ~/Dropbox/Articles
Bin                all        put it first in PATH; a place to install executables without admin privilege
<Company>          all        work for <Company>
Desktop            all        files here appear as icons on the desktop
Documents          all        some applications keep their files here; including ~/Documents/Eclipse
                              and ~/Documents/Notebooks for iPython; editor files go in ~/Dropbox/Documents
Downloads          all        browsers should be configured to download files here; it is generally
Dropbox            host       Articles, Documents, Elements, Pictures
Env                all        virtualenv, rbenv environments
Etc                all        ~/Etc/UnicodeData.txt
Favorits           win
Lang               all        subdirectories by programming language; if the code is put under version
                              control it is in Src
Library            mac        scripts run by Fastscripts must be in ~/Library/Scripts
Links              win
Local Settings     win
Man                all        put it first in MANPATH; a place to install man pages w/o admin privilege
Movies             all        keep this empty
Music              all        iTunes and Amazon Cloud Player
NetHood            win
Pictures           all        link to ~/Dropbox/Pictures; also Omnigraffle and image editor files;
                              PDFs created by R
Public             all        If part of the home directory is made available on the local network,
                              make it this directory. E.g on Mac
                              ``System Preferences | Sharing | File Sharing``
PrintHood          win
Recent             win
Saved Games        win
Searches           win
Shared             all        a directory which is shared with guest VMs
Src                all        tarballs, git repos, isos for VM
Start Menu         win
Templates          win/linux
Trash              all        On Mac, symlink to .Trash
Videos             win/linux  Windows/Ubuntu place for movies; keep this empty
=================  =========  =================================================================================


Hidden Files
------------

Unix hidden files have names which start with a period.

On Mac, the Finder does not display these files.  Other files can be hidden from the Finder:

    SetFile -a V foo.txt

To expose a file to the Finder:

    SetFile -a v foo.txt

*Windows...*

NTUSER.DAT* files

Temporary Files
---------------

POSIX systems are supposed to set the ``$TMPDIR`` environment variable.  Mac OS X does.

Linux, in my experience does not.  The Filesystem Hierarchy Standard guarantees that ``/tmp`` will exist, howver.

http://www.pathname.com/fhs/pub/fhs-2.3.html

Windows sets the ``%TEMP%`` environment variable to the location of the temporary file directory.

Projects
--------

* README
* Makefile
* .git
