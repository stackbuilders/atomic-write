[![Build](https://github.com/stackbuilders/atomic-write/actions/workflows/build.yml/badge.svg?branch=master)](https://github.com/stackbuilders/atomic-write/actions/workflows/build.yml)
[![Hackage version](https://img.shields.io/hackage/v/atomic-write.svg)](http://hackage.haskell.org/package/atomic-write)
<!-- ALL-CONTRIBUTORS-BADGE:START - Do not remove or modify this section -->
[![All Contributors](https://img.shields.io/badge/all_contributors-6-orange.svg?style=flat-square)](#contributors-)
<!-- ALL-CONTRIBUTORS-BADGE:END -->

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

```import System.AtomicWrite.Writer.ByteString```

Then you can use the atomicWriteFile function that accepts a FilePath and a ByteString, e.g.:

```atomicWriteFile myFilePath myByteString```

See the
[Haddock documentation](http://hackage.haskell.org/package/atomic-write).

## Contributors ✨

Thanks goes to these wonderful people ([emoji key](https://allcontributors.org/docs/en/emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tbody>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://www.stackbuilders.com/news/author/justin-leitgeb"><img src="https://avatars.githubusercontent.com/u/9977?v=4?s=100" width="100px;" alt="Justin S. Leitgeb"/><br /><sub><b>Justin S. Leitgeb</b></sub></a><br /><a href="https://github.com/stackbuilders/atomic-write/commits?author=jsl" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/cptrodolfox"><img src="https://avatars.githubusercontent.com/u/20303685?v=4?s=100" width="100px;" alt="William R. Arellano"/><br /><sub><b>William R. Arellano</b></sub></a><br /><a href="https://github.com/stackbuilders/atomic-write/commits?author=cptrodolfox" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/Alex0jk"><img src="https://avatars.githubusercontent.com/u/22301755?v=4?s=100" width="100px;" alt="Alexander Mejía"/><br /><sub><b>Alexander Mejía</b></sub></a><br /><a href="https://github.com/stackbuilders/atomic-write/commits?author=Alex0jk" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/acamino"><img src="https://avatars.githubusercontent.com/u/957202?v=4?s=100" width="100px;" alt="Agustin Camino"/><br /><sub><b>Agustin Camino</b></sub></a><br /><a href="https://github.com/stackbuilders/atomic-write/commits?author=acamino" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://juancarlos.io/"><img src="https://avatars.githubusercontent.com/u/2164411?v=4?s=100" width="100px;" alt="Juan Paucar"/><br /><sub><b>Juan Paucar</b></sub></a><br /><a href="https://github.com/stackbuilders/atomic-write/commits?author=juanpaucar" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://www.barbaramorantes.com/"><img src="https://avatars.githubusercontent.com/u/67979158?v=4?s=100" width="100px;" alt="Barbara Morantes"/><br /><sub><b>Barbara Morantes</b></sub></a><br /><a href="#example-BarbDMC" title="Examples">💡</a></td>
    </tr>
  </tbody>
  <tfoot>
    <tr>
      <td align="center" size="13px" colspan="7">
        <img src="https://raw.githubusercontent.com/all-contributors/all-contributors-cli/1b8533af435da9854653492b1327a23a4dbd0a10/assets/logo-small.svg">
          <a href="https://all-contributors.js.org/docs/en/bot/usage">Add your contributions</a>
        </img>
      </td>
    </tr>
  </tfoot>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

This project follows the [all-contributors](https://github.com/all-contributors/all-contributors) specification. Contributions of any kind welcome!

## License

MIT, see [the LICENSE file](LICENSE).

## Contributing

Do you want to contribute to this project? Please take a look at our [contributing guideline](/docs/CONTRIBUTING.md) to know how you can help us build it.

---
<img src="https://cdn.stackbuilders.com/media/images/Sb-supports.original.png" alt="Stack Builders" width="50%"></img>
[Check out our libraries](https://github.com/stackbuilders/) | [Join our team](https://www.stackbuilders.com/join-us/)
