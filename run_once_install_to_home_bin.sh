#!/bin/bash

# Install packages to .local/share/bin

# Bitwarden cli - remember to login
# Download
cd /tmp/
wget https://github.com/bitwarden/sdk-sm/releases/download/bws-v1.0.0/bws-x86_64-unknown-linux-gnu-1.0.0.zip

# Unpack -> now it is installed 
mkdir -p ~/.local/bin
unzip -o bws-x86_64-unknown-linux-gnu-1.0.0.zip -d ~/.local/bin 


# Quarto - install to .local/bin
# Download to tmp
cd /tmp/
wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.6.39/quarto-1.6.39-linux-amd64.tar.gz

# Unpack to Builds, where the library resides
mkdir -p ~/Builds
tar -C ~/Builds -xvzf quarto-1.6.39-linux-amd64.tar.gz

# Symlink the binary to .local/bin 
mkdir -p ~/.local/bin
ln -sf ~/Builds/quarto-1.6.39/bin/quarto ~/.local/bin/quarto


# Build a toolbx fedora container
cd /var/home/bagge/.local/share/chezmoi/containers/main_toolbx/
just build
