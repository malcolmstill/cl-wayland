#!/bin/bash

gcc -c -fPIC -pthread xdg-shell.c -o xdg-shell.o
gcc -shared -Wl,-soname,lib-xdg-shell.so.1 -o lib-xdg-shell.so.1.0.1 xdg-shell.o
