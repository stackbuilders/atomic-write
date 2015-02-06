[![Build Status](https://travis-ci.org/stackbuilders/atomic-write.svg?branch=master)](https://travis-ci.org/stackbuilders/atomic-write)

# Atomic Write

Atomic Write assists with atomic modification of files using
Haskell. It is a wrapper for using the atomic mv(1) operation which
correctly sets permissions based on the original file, or on system
defaults if no file previously exists.

See the
[Haddock documentation](http://hackage.haskell.org/package/atomic-write)
for more information.

## Author

Justin Leitgeb

## License

MIT

## Copyright

(C) 2015 [Stack Builders Inc.](http://www.stackbuilders.com/)
