# Cheatpas


**cheatpas** is a lightweight, fast, and user-friendly CLI tool for managing and viewing cheatsheets. Written in Pascal, it serves as a modern addition to `man` pages, focusing on quick examples and easy access to common commands or undocumented software (e.g. proprietary software).

---

## Features

- Store and manage personal or shared cheatsheets
- Quickly list, view, search, or create new cheatsheets
- Colorized output for better readability (optional)
- Configurable editor and cheatsheet directory via a simple Conf file
- Cross-platform support
- Simple `.deb` package for easy Debian installation

---

## Installation

### From Debian Package

Download the latest `.deb` from the [Releases](https://github.com/J-F-K-C/cheatpas/releases) page and install:

```bash
sudo dpkg -i cheatpas.deb
```

This installs:

* Binary: /usr/local/bin/cheatpas
* Default config: /etc/cheatpas/config.ini

You may edit the config or copy it to your home directory for user-specific customization.

### From Source

Clone the repository and compile with Free Pascal Compiler:

git clone https://github.com/J-F-K-C/cheatpas.git
cd cheatpas/src
fpc cheatpas.pas
sudo mv cheatpas /usr/local/bin/

### Configuration

~/.config/cheatpas/config.conf

or, by default, /etc/cheatpas/config.conf for system-wide use.

Example config.conf:

[cheatpas]
cheat_dir=/home/USER/cheats
editor=nano
color=true

* cheat_dir: folder where your cheatsheets are stored

* editor: your preferred text editor for creating/editing cheatsheets (uses $EDITOR as standard-fallback)

* color: true or false to enable/disable color output

### Usage

* List all available cheatsheets
```cheatpas list```

* Show a specific cheatsheet
```cheatpas show <cheatsheet_name>```

* Create a new cheatsheet
```cheatpas new <cheatsheet_name>```

* Search inside all cheatsheets
```cheatpas search <text>```

### Cheatsheet Format

Each cheatsheet is a plain text file with .txt extension

Recommended format:

```
# Title or command
Description or examples
Usage examples
```

Store them in your configured cheat_dir (default ~/cheats)

### Example

*  Create a new cheatsheet for 'git'
cheatpas new git

* Show it
cheatpas show git

* Search all cheatsheets for 'ssh'
cheatpas search ssh

Contributing

### Contributions are welcome!

Fork the repository

Create a feature branch

Submit a pull request

Please follow Pascal coding conventions and keep the CLI simple and fast.

License

This project is licensed under the MIT License. See LICENSE for details.

### Acknowledgments

Inspired by cheat CLI and traditional Unix manpages

Built with Free Pascal Compiler and Lazarus


