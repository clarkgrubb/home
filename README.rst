----
HOME
----

installation_ | `home directories <#home-directories>`_ | users_ | subdirectories_ | `finder, explorer, and files <#finder-explorer-files>`_ | `hidden files <#hidden-files>`_ | `temporary files <#temporary-files>`_ | trash_ | public_ | shared_ | dropbox_

Summary
-------

Dot files for Darwin, Linux, Cygwin, and MinGW.

Notes on home directory organization.


Installation
------------

To install the dot files:

::

    $ make

Home Directories
----------------

Early versions of Unix put user home directories in ``/usr``.  In Linux and BSD the user home directories are in ``/home``.

A Unix user's home directory is specified in ``/etc/passwd``. This is the working directory when the user logs in, and the system sets the environment variable ``HOME`` to this value. The C shell introduced tilde expansion as shortcut for the home directory in paths.

Mac OS X puts the home directories in ``/User``. It sets the HOME environment variable, but does not store user information in ``/etc/passwd``. The information is stored in the Directory Service which can be queried with the dscl command:

::

    dscl . -read /Users/$USER NFSHomeDirectory

In Windows terminology, the home directory is called the *user profile folder*.

DOS did not have a home directory, and Windows 95 had a single ``C:\My Documents`` directory.

Windows NT was the first multiuser version of Windows. It put user profile folders in ``C:\WINNT\Profiles``. Windows XP put them in ``C:\Documents and Settings``.  Since Windows Vista they have been in ``C:\Users``.

Windows sets the environment variable ``%USERPROFILE%`` to the path of the current user profile folder.

Users
-----

When an Ubuntu instance is created, the setup procedure prompts for the user's full name, login name, and password.  The ``adduser`` command can be used to create more users.

VMware Fusion has an Easy Install feature which collects the account information and provides it to the Ubuntu instance.

On ``Mac OS X`` the setup procedure prompts for the user's full name, login name, and password.  User accounts are managed at ``System Preferences | Users & Groups``.

*Windows...*

*Avatars*

Subdirectories
--------------

I like a home directory which only contains subdirectories and hidden files.  Desktop operating systems create some subdirectories for me, and since they capitalize those subdirectory names, it is best to follow that convention.

The Mac filesystem (HFS+), incidentally, is not case sensitive, but it remembers the case that was used when a file is created and uses it for display.

To make it easier to work at the command line, I avoid file names which contain spaces.

Since I use tab completion, I choose names that are uniquely specified by their first two letters.  But note that desktops create two directories ``Documents`` and ``Downloads`` which require three letters to uniquely specify.

Windows some home directory subdirectories which break the no-space rule and the unique-two-letter-prefix rule, e.g. ``My Documents``, ``My Pictures``.  One can rename these directories, but since they are `Special Folders <http://en.wikipedia.org/wiki/Special_folder>`_, one should also update the registry for the benefit of applications using the Special Folder API to get the paths.  

::

    HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders

Also, the Explorer may keep separate names for these files, so one may need to change the file names at both the command line and in the Explorer.

**standard home subdirectories:**

=================  =========  =================================================================================
subdirectory       os         description
=================  =========  =================================================================================
AppData            win
Application Data   win
Desktop            all        files here appear as icons on the desktop
Documents          all        some applications keep their files here; including ~/Documents/Eclipse
                              and ~/Documents/Notebooks for iPython; editor files go in ~/Dropbox/Documents
Downloads          all        browsers should be configured to download files here; it is generally
Dropbox            host       Articles, Documents, Elements, Pictures
Favorites          win        browser bookmarks; probably not used by Firefox or Chrome
Library            mac        scripts run by Fastscripts must be in ~/Library/Scripts
Links              win        used by the Explorer sidebar?
Local Settings     win        hidden; 
Movies             mac        keep this empty
Music              all        iTunes and Amazon Cloud Player
NetHood            win        hidden; 
Pictures           all        delete and link to ~/Dropbox/Pictures; also Omnigraffle and image editor files;
                              PDFs created by R
Public             all        If part of the home directory is made available on the local network,
                              make it this directory. E.g on Mac
PrintHood          win
Recent             win        hidden; full of Windows Shell shortcuts (.lnk suffix).
Saved Games        win
Searches           win
Start Menu         win
Templates          win/linux
Videos             win/linux  Windows/Ubuntu place for movies; keep this empty
=================  =========  =================================================================================

**personal home subdirectories:**

===============================================================================================================
subdirectory       os         description
=================  =========  =================================================================================
Articles           all        PDFs; link to ~/Dropbox/Articles
Bin                all        put it first in PATH; a place to install executables without admin privilege
<Company>          all        work for <Company>
Env                all        virtualenv, rbenv environments
Etc                all        ~/Etc/UnicodeData.txt
Lang               all        subdirectories by programming language; if the code is put under version
                              control it is in Src
Man                all        put it first in MANPATH; a place to install man pages w/o admin privilege
Shared             all        shared with guest VMs
Src                all        tarballs, git repos, isos for VM
Trash              all        symlink to Trash or Recyle Bin
=================  =========  =================================================================================

To set up the home directory:

::

    mkdir ~/Bin ~/Env ~/Etc ~/Lang ~/Man ~/Src

Make these links into ~/Dropbox: Articles, Pictures

*links on Windows*

Finder, Explorer, Files
-----------------------

*notes on the Finder sidebar*

The GUI file system browser in Ubuntu used to be called Nautilus, but as of Ubuntu 13 it is just called *Files*?


Hidden Files
------------

Unix hidden files have names which start with a period.

On Mac, the Finder does not display these files.  Other files can be hidden from the Finder:

    SetFile -a V foo.txt

To expose a file to the Finder:

    SetFile -a v foo.txt

On Windows, files can be hidden from the Explorer by checking ``Properties | General | Hidden`` or at the command line using ``ATTRIB +H``.  To expose a hidden file to Explorer at the command line use ``ATTRIB -H``.

To make hidden files visible in the Explorer:

::

    Organize | Folder and search options | View | Hidden files and folders | Show hidden files, folders, and drives

Files are always visible at the command line.

NTUSER.DAT* files

Temporary Files
---------------

POSIX systems are supposed to set the ``$TMPDIR`` environment variable.  Mac OS X sets ``$TMPDIR`` to a randomly generated path in ``/var/folders``.  *When does Mac OS X clean out /tmp and $TMPDIR?*

Linux does not set ``$TMPDIR`` in my experience.  The `Filesystem Hierarchy Standard <http://www.pathname.com/fhs/pub/fhs-2.3.html>`_ guarantees that ``/tmp`` will exist, howver.

Ubuntu Linux is usually configured to empty ``/tmp`` on boot.  See ``/etc/init/mounted-tmp.conf``.

Windows sets the ``%TEMP%`` environment variable to the location of the temporary file directory.  Windows never cleans out this directory.

Trash
-----

The Mac OS X Trash folder is ``~/.Trash``.

The Ubuntu Trash folder is ``~/.local/share/Trash``.  It is not created until something is moved to the trash using Nautilus.

The Windows recycle bin is at ``C:\$Recycle.Bin``.

Public
------

On Mac OS X to expose a directory on the local netowrk, go to:

::

    System Preferences | Sharing | File Sharing

Shared
------

*a host directory accessible to guest operating systems*

Dropbox
-------

Some ``~/Dropbox`` subdirectories:

* Articles
* Documents
* Elements
* Pictures
