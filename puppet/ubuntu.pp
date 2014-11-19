# server

package { 'emacs24':
	ensure => installed,
}

package { 'git':
	ensure => installed,
}

package { 'mercurial':
	ensure => installed,
}

package { 'zsh':
	ensure => installed,
}

package { 'ssh':
	ensure => installed,
}

package { 'tmux':
	ensure => installed,
}

package { 'curl':
	ensure => installed,
}

package { 'silversearcher-ag':
	ensure => installed,
}

package { 'libncurses5-dev':
	ensure => installed,
}

package { 'ntp':
	ensure => installed,
}

package { 'ruby-dev':
	ensure => installed,
}

package { 'python-pip':
	ensure => installed,
}

package { 'openjdk-7-jdk':
	ensure => installed,
}

# desktop

package { 'octave':
	ensure => installed,
}

package { 'gnumeric':
	ensure => installed,
}

package { 'r-base':
	ensure => installed,
}

package { 'ipython-notebook':
	ensure => installed,
}

package { 'python-sympy':
	ensure => installed,
}

package { 'python-pandas':
	ensure => installed,
}

package { 'texlive':
	ensure => installed,
}

package { 'texworks':
	ensure => installed,
}

package { 'gnome-dictionary':
	ensure => installed,
}

package { 'inkscape':
	ensure => installed,
}

package { 'gimp':
	ensure => installed,
}

# user

$user = 'clark'
$group = 'clark'

# home directory

file { "/home/${user}/Local":
     ensure => 'directory',
     owner => $user,
     group => $group,
     mode => 755,
}

file { "/home/${user}/Local/bin":
     ensure => 'directory',
     owner => $user,
     group => $group,
     mode => 755,
}

file { "/home/${user}/Local/env":
     ensure => 'directory',
     owner => $user,
     group => $group,
     mode => 755,
}

file { "/home/${user}/Local/etc":
     ensure => 'directory',
     owner => $user,
     group => $group,
     mode => 755,
}

file { "/home/${user}/Local/lib":
     ensure => 'directory',
     owner => $user,
     group => $group,
     mode => 755,
}

file { "/home/${user}/Local/man":
     ensure => 'directory',
     owner => $user,
     group => $group,
     mode => 755,
}

file { "/home/${user}/Local/share":
     ensure => 'directory',
     owner => $user,
     group => $group,
     mode => 755,
}

file { "/home/${user}/Local/src":
     ensure => 'directory',
     owner => $user,
     group => $group,
     mode => 755,
}

