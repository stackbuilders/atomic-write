[![Build Status](https://travis-ci.org/stackbuilders/atomic-write.svg?branch=master)](https://travis-ci.org/stackbuilders/atomic-write) [![Hackage version](https://img.shields.io/hackage/v/atomic-write.svg)](http://hackage.haskell.org/package/atomic-write)

# Atomic Write

Atomic Write assists with atomic modification of files using
Haskell. It is a wrapper for using the atomic mv(1) operation which
correctly sets permissions based on the original file, or on system
defaults if no file previously exists.

## How it works
On most Unix systems, mv is an atomic operation. This makes it simple to write to a file atomically just by using the mv operation. However, this will destroy the permissions on the original file. This library does the following to preserve permissions while atomically writing to a file:

If an original file exists, take those permissions and apply them to the temp file before mving the file into place.

If the original file does not exist, create a following with default permissions (based on the currently-active umask).

This way, when the file is mv'ed into place, the permissions will be the ones held by the original file.

This library is based on similar implementations found in common libraries in Ruby and Python:

- [Ruby on Rails includes a similar method called atomic_write](https://apidock.com/rails/File/atomic_write/class)

- [Chef includes atomic update functionality](https://github.com/chef/chef/blob/c4631816132fcfefaba3d123a1d0dfe8bc2866bb/lib/chef/file_content_management/deploy/mv_unix.rb#L23:L71)

- [There is a python library for atomically updating a file](https://github.com/sashka/atomicfile)

## Usage

To use `atomic-write`, import the module corresponding to the type you wish to write atomically, e.g., to write a (strict) ByteString atomically:

import System.AtomicWrite.Writer.ByteString
Then you can use the atomicWriteFile function that accepts a FilePath and a ByteString, e.g.:

atomicWriteFile myFilePath myByteString

See the
[Haddock documentation](http://hackage.haskell.org/package/atomic-write).

## License

MIT, see [the LICENSE file](LICENSE).

## Contributing

Do you want to contribute to this project? Please take a look at our [contributing guideline](/docs/CONTRIBUTING.md) to know how you can help us build it.

---
<img src="https://www.stackbuilders.com/media/images/Sb-supports.original.png" alt="Stack Builders" width="50%"></img>  
[Check out our libraries](https://github.com/stackbuilders/) | [Join our team](https://www.stackbuilders.com/join-us/)
(C) 2015-2019 [Stack Builders Inc.](http://www.stackbuilders.com/)
