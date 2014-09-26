----
HOME
----

installation_ | `home directories <#home-directories>`_ | users_ | subdirectories_ | `finder and explorer <#finder-and-explorer>`_ | `hidden files <#hidden-files>`_ | `temporary files <#temporary-files>`_ | trash_ | public_ | local_

Summary
-------

Dot files which are used on Darwin, Linux, and MinGW.

Also notes on how the home directory is organized.

Installation
------------

To install the dot files:

::

    $ make

Home Directories
----------------

Early versions of Unix put user home directories in ``/usr``.  In Linux and BSD the user home directories are in ``/home``.

A Unix user's home directory is specified in ``/etc/passwd``. When the user logs in, the home directory in ``/etc/passwd`` is used to set the working directory and the environment variable ``HOME``. The C shell introduced tilde expansion as a shortcut for the home directory in paths.

Mac OS X puts home directories in ``/Users``. It sets the ``HOME`` environment variable, but does not store user information in ``/etc/passwd``. Instead the information is stored in a *Directory Service* which can be queried with the ``dscl`` command:

::

    dscl . -read /Users/$USER NFSHomeDirectory

Windows calls the home directory the *user profile folder*.

DOS did not have a home directory and Windows 95 had a single ``C:\My Documents`` directory at the file system root.  Windows NT was the first multiuser version of Windows. It put user profile folders in ``C:\WINNT\Profiles``. Windows XP put them in ``C:\Documents and Settings``.  Since Windows Vista they have been in ``C:\Users``.

Windows sets the environment variable ``%USERPROFILE%`` to the path of the current user profile folder.

Users
-----

The name of home directory is the login name of the user.  A user (login name, password, maybe a full name) must be specified when the operating system is installed.

To change the name, it is probably best to create a new account, copy over files, and delete the old account.  On Windows a login name can be changed, but this does not change the home directory name.

On Mac OS X user accounts are managed at ``System Preferences | Users & Groups``.  To set the Mac avatar, go to ``System Preferences | Users & Groups`` and click on the image.  To use an image that is not one of the defaults, drag it from the Finder to the System Preferences pane.

On Windows to add a new account go to ``Control Panel | User Accounts | Manage another account``.  To set the Windows avatar, go to ``Control Panel | User Accounts | Change your picture``.

On Linux the ``adduser`` command creates a user.  On Ubuntu the avatar can be set at ``System Settings | User Accounts``

Subdirectories
--------------

Subdirectories in the home directory should be capitalized and regular files in the home directory should be hidden.

The Mac filesystem (HFS+), incidentally, is not case sensitive, but it remembers the case that was used when a file is created and uses it for display.

To make it easier to work at the command line, avoid file names which contain spaces.

Tab completion works well when the contents of a directory are uniquely specified by a short prefix.  Two letter prefixes are often possible, though the desktop operating systems create directories, namely ``Documents`` and ``Downloads``, which require three letters to uniquely specify.

Windows creates home subdirectories which break both the no-space rule and the unique-two-letter-prefix rule, e.g. ``My Documents``, ``My Music``, ``My Pictures``, ``My Videos``.  One can rename these directories.  Since they are `Special Folders <http://en.wikipedia.org/wiki/Special_folder>`_, one should also update the registry for the benefit of applications using the Special Folder API to get the paths.  The registry key is:

::

    HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders

Also, the Explorer keeps separate names for these files, so one must change the file names both at the command line and in Explorer.

**windows home subdirectories to rename:**

::

    My Documents  ->  Documents
    My Music      ->  Music
    My Pictures   ->  Pictures
    My Videos     ->  Videos
    Saved Games   ->  Games

**standard home subdirectories:**

==================  =========  ==================================================================================
subdirectory        os         description
==================  =========  ==================================================================================
Desktop             all        Files here appear as icons on the desktop.
Documents           all        Some applications keep their files here; including ``~/Documents/IntelliJ``,
                               ``~/Documents/Excel``, and ``~/Documents/Notebooks`` for iPython;
                               editor files go in ``~/Dropbox/Documents``
Downloads           all        Configure browsers to download files here without asking.
Dropbox             host       Synchronization and cloud backup.
Games               win        Minesweeper and Solitaire store games in ``Saved Games\Microsoft Games``
Library             mac        Put AppleScript in ``~/Library/Scripts``.
Movies              mac        Stream video and keep this empty.
Music               all        E.g. iTunes and Amazon Cloud Player.
Pictures            all        Image editor files; Visio and Omnigraffle; PDFs created by R.
Public              linux/mac  A directory to make available on the local network.
                               Windows has a directory at ``C:\Users\Public``.
Templates           win/linux  If you create a template in Word it will be stored here.
Videos              win/linux  Stream video and keep this empty.
==================  =========  ==================================================================================

**windows specific home subdirectories**

==================  =========  ==================================================================================
subdirectory        os         description
==================  =========  ==================================================================================
*AppData*           win        Hidden; I like to unhide it; items pinned to the Start Menu are shortcuts at
                               ``AppData/Roaming/Microsoft/Internet Explorer/Quick Launch/User Pinned/StartMenu``
*Application Data*  win        Hidden; link to ``AppData\Roaming``.
Contacts            win        Used by Outlook?
*Cookies*           win        Hidden; link to ``AppData\Roaming\Microsoft\Windows\Cookies``.
Favorites           win        Browser bookmarks; probably not used by Firefox or Chrome.
Links               win        The Favorites section of the Explorer sidebar.
*Local Settings*    win        Hidden; link to ``AppData\Local``.
*NetHood*           win        Hidden; link to ``AppData\Roaming\Microsoft\Windows\Network Shortcuts``
*PrintHood*         win        Hidden; link to ``AppData\Roaming\Microsoft\Windows\Printer Shortcuts``
Recent              win        Hidden; full of Windows Shell shortcuts (``.lnk`` suffix).  Implements
                               the ``Recent Places`` folder.
*SendTo*            win        Hidden; link to ``AppData\Roaming\Microsoft\Windows\SendTo``.
                               There is is ``Send to`` item in the Explorer context menu.  More target
                               applications can be added by putting shortcuts in the folder.  The applications
                               must be launchable from the command line and accept a file path as an argument.
Searches            win        If in Windows Search you click on ``See more results``, you'll get a window
                               of results.  The window has a ``Save search`` option which will create an XML
                               file in this directory.
Start Menu          win        Hidden; link to ``AppData\Roaming\Microsoft\Windows\Start Menu``.
                               ``Start Menu\Programs`` is one way to add programs to the
                               ``All Programs`` section of the Start Menu; put a shortcut in
                               ``Start Menu\Programs\Startup``
                               to launch an application at login.
==================  =========  ==================================================================================


**personal home subdirectories:**

=================  =========  =================================================================================
subdirectory       os         description
=================  =========  =================================================================================
Articles           all        PDFs; link to ``~/Dropbox/Articles``.
<Company>          all        Work
<Company>/Doc      all        Work Spreadsheets, Slideshows
<Company>/HR       all        Documents from HR
<Company>/Pic      all        Work Pngs, Jpgs, Svg, Omnigraffle files
<Company>/Src      all        Work Source code
<Company>/Web      all        Work HTML, generated from Markdown with a Makefile
Local              all        Place to install headers and libraries
Local/bin          all        Put first in ``PATH``; a place to install executables without admin privilege.
Local/env          all        ``virtualenv`` environments.
Local/etc          all        ``~/Etc/UnicodeData.txt``
Local/man          all        Put first in ``MANPATH``; a place to install man pages w/o admin privilege
Local/src          all        Tarballs, git repos, ISOs for virtual machines, Java SDKs.
Lang               all        Subdirectories by programming language; code under version control is in ``Src``.
Shared             all        Share with guest virtual machines.
Trash              all        Symlink to Trash or Recyle Bin.
=================  =========  =================================================================================

To set up the home directory:

::

    cd
    mkdir Local Lang
    cd Local
    mkdir bin env etc man src

On Mac OS X:

::
   
    cd
    rm -rf Pictures
    ln -s Dropbox/Articles Articles
    ln -s Dropbox/Pictures Pictures/Pictures

On Windows create two links: a symlink and an Explorer shortcut.

Finder and Explorer
-------------------

Drag directories to the Finder sidebar to add them.

Drag directories to the Explorer sidebar to add them.

The ``Favorites`` section of the Explorer sidebar is implemented with shortcuts in the ``~/Links`` directory.   Because shortcuts can have a different name from their target, the shortcut to the home directory can be ``Home`` instead of the current user name.

To add directories to the Ubuntu file system browser, open the directory, click on the cog icon in the upper right, and select ``Bookmark this Location``.

Hidden Files
------------

Unix hidden files have names which start with a period.  The file globbing characters ``*`` and ``?`` do not match an initial period.  Use ``ls -a`` to see hidden files in a listing.

On Mac, the Finder does not display these files.  Other files can be hidden from the Finder:

    SetFile -a V foo.txt

To expose a file to the Finder:

    SetFile -a v foo.txt

On Windows, files can be hidden from the Explorer by checking ``Properties | General | Hidden``.

Files can be hidden from the Explorer at the command line using ``ATTRIB +H``.  To expose a hidden file to Explorer at the command line use ``ATTRIB -H``.

To make all hidden files visible in the Explorer go to:

::

    Organize | Folder and search options | View | Hidden files and folders

and select ``Show hidden files, folders, and drives``.

Windows files which are hidden by attribute are always visible at the command line.  The files in the Windows home directory of the form ``NTUSER.DAT*`` are hard to miss because of their long names.  They are caches of the registry.

If Unix tools are installed by MinGW, these observe the convention that files that start with a period are hidden.

Conversely Unix dot files are visible in Explorer, which is unaesthetic.  They can be manually hidden with this command:

::

    ATTRIB +H /S /D C:\.*


Temporary Files
---------------

POSIX systems are supposed to set the ``$TMPDIR`` environment variable.  Mac OS X sets ``$TMPDIR`` to a path in ``/var/folders``.  Mac OS X checks for and deletes files that are older than 3 days in ``/tmp`` on a daily schedule.  See ``/etc/periodic/daily/110.clean-tmps`` and ``/etc/defaults/periodic.conf``.  Older files may survive because of open file handles.  *How are files in /var/folders cleaned up?*

Linux does not set ``$TMPDIR`` in my experience.  The `Filesystem Hierarchy Standard <http://www.pathname.com/fhs/pub/fhs-2.3.html>`_ guarantees that ``/tmp`` will exist, howver.  Ubuntu is usually configured to empty ``/tmp`` on boot.  See ``/etc/init/mounted-tmp.conf``.

Windows sets the ``%TEMP%`` environment variable to the location of the temporary file directory.  Windows never cleans out this directory.

Trash
-----

The Mac OS X Trash folder is ``~/.Trash``.  Files are sent to the Mac OS X trash by selecting them in the Finder and typing ``⌘Delete``.  When the Finder is active, ``⇧⌘Delete`` empties the Trash.

The Ubuntu Trash folder is ``~/.local/share/Trash``.  It is not created until something is moved to the trash using Nautilus.

The Windows Recycle Bin is at ``C:\$Recycle.Bin``.  Actually, each NTFS file system has a Recycle Bin.  The Recycle Bin on the Desktop is a union.

Files are sent to the Recyle Bin by right clicking in the Explorer and selecting ``Delete`` or selecting the file and pressing the delete key (fn delete in Bootcamp).  Right click the Recycle Bin in Explorer to empty it.

Command line tools such as ``rm`` and ``del`` on all operating systems remove files without putting them in the Trash directory or Recycle Bin folder.

Public
------

On Mac OS X to expose a directory on the local network, go to:

::

    System Preferences | Sharing | File Sharing

This is good enough for sharing with Macs.  There is an option for enabling SMB so that Windows can access the directory, but it requires storing the Windows account password on the Mac.

The name of a Mac is generated from the initial account.  It can be changed at ``System Preferences... | Sharing``.  Macs use fully qualified DNS names of the form ``<MAC-NAME>.local``.  Such a DNS name is only visible if both computers are on the same network.  Lookups are performed by broadcasting.  Each machine is the DNS server for its own name.


Windows has a directory at ``C:\Users\Public`` which can be made public.  Navigate to it in the Explorer and select ``Share with | Advanced sharing settings | Public`` to get a list of checkboxes.  If the choices are permissive enough, a Mac will on the local network will automatically detect and be able to access the files.  I believe other parts of the file system on a Windows machine can also be made public.

Windows requests a computer name during installation.  It can be changed later at 

::

    Control Panel | System | Computer name, domain, and workgroup settings

Local
-----

The ``~/Local`` directory is an aid when building source code without root privilege.  If the source code depends on headers and libraries that aren't installed, download them, build them, and install them in ``~/Local``. 

To make ``autoconf`` aware of ``~/Local``, create the file ``${HOME}/Local/shared/config.site`` with these contents::

    CPPFLAGS=-I$HOME/Local/include
    LDFLAGS=-L$HOME/Local/lib

Then configure the source code with::

    ./configure --prefix=$HOME/Local

To use locally installed command-line tools and man pages::

    export PATH=~/Local/bin:$PATH
    export MANPATH=~/Local/man:$(MANPATH= manpath)
