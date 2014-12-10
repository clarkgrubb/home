# server

package { 'emacs':
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

package { 'tmux':
	ensure => installed,
}

package { 'readline-devel':
	ensure => installed,
}

package { 'wireshark':
	ensure => installed,
}

package { 'gcc':
	ensure => installed,
}

package { 'gdb':
	ensure => installed,
}

package { 'ruby':
	ensure => installed,
}

package { 'openssh-server':
        ensure => installed,
}      

# desktop

package { 'octave':
	ensure => installed,
}

package { 'gnumeric':
	ensure => installed,
}

package { 'R':
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

ssh_authorized_key { 'clark.grubb@demandmedia.com':
  user => 'clark',
  type => 'ssh-rsa',
  key => 'AAAAB3NzaC1yc2EAAAABIwAAAQEAsVXZkmmn1b3PLx3EAUCPjUgZnt7eG9CEaCX6k3f+pWol42B9zHAYMEu6WSwet0S6fgheq8YlSmHNwdLcZ1Nh+zUDziz6DQ7Jfo5+1d3pZGNlEFWjhHnWLL6ee2BhATuP2Pc69UqdwtZvrHTqmHthTowbQVjtoRAH2eIiOMgKAYC2F3uJq3aQfwpdFJV/SsQZjVehna0lzQi4I4NpQPbZZNw//eDlXI6v/s30cyIr+SzcY1gAHcXErdSu+QM7ULjTnD5ZESY9Ux+1T61Ra3glScjy0qE4uXED6toDIQihy+j6jmoKyqTmdaTNY+ggnzdggvXFbGN1O0WJ/n9sPYkgYQ==',
}

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

