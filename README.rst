================
HOME DIRECTORIES
================

Dot files for Darwin, Linux, Cygwin, and MinGW.

Installation
============

::

    $ make

Location
========

Early versions of Unix put user home directories in ``/usr``. This directory became co-opted for common files which were not essential to run the system. In Linux and BSD the user home directories are in ``/home``.

Each user's home directory is specified in ``/etc/passwd``. The system sets the environment variable ``HOME`` with this value. The C shell introduced tilde expansion as shortcut for the home directory in paths.

Mac OS X puts the home directories in ``/User``. It sets the HOME environment variable, but does not store user information in ``/etc/passwd``. The information is stored in the Directory Service which can be queried with the dscl command:

::

    dscl . -read /Users/$USER NFSHomeDirectory

DOS did not have a home directory, and Windows 95 had a single ``C:\My Documents directory``.

In Windows terminology, the home directory is called the user profile folder. Windows NT was the first multiuser version of Windows. It put user profile folders in ``C:\WINNT\Profiles``. Windows XP put them in ``C:\Documents and Settings`` and since Windows Vista they have been in ``C:\Users``.

Windows sets the environment variable ``%UserProfile%`` to the path of the current user profile folder.
