
# cl-wayland

`cl-wayland` is a Common Lisp wrapper for libwayland. It aims to allow Wayland compositors and clients to be written in Common Lisp.

## Status

`cl-wayland` is being developed primarily in support of [ulubis](https://github.com/malcolmstill/ulubis) and is therefor feature incomplete. Namely it doesn't yet generate client-side bindings and some of the core server functionality may not be in place. Pull requests adding more of the API are more than welcome.

## Requiremnts

`cl-wayland` (obiously) requires libwayland and cffi. It is likely that libwayland already exists on your Linux installation if it is recent.

## Installation

```
CL-USER> (ql:quickload :cl-wayland)
```
