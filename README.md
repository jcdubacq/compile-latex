# NAME

compile-latex - compile TeX files as many times as needed

# SYNOPSIS

compile-latex \[_options_\] file\[.tex\]

compile-latex __\--depends__|__\--outputs__|__\--internals__ file\[.tex\]

compile-latex __\--help__|__\--man__|__\--nroff__|__\--usage__

# OPTIONS

## GENERIC OPTIONS

__\--debug__ _n_ Change the verbosity level to _n_ (see ["DEBUG"](#debug)).

__\--forget__ Forget persistent options stored for the file (e.g. index
options).

__\--manual__ Removes automatic mode. This option is persistant.

__\--auto__ (default) Reinstates automatic mode. Use it to remove
__\--manual__ effect. Currently has the effect of __\--index__, __\--bibtex__
if no other index option (resp. bibliography) option was selected, and
starts the automatic discovery mode for `bibtex`.

## BIBTEX OPTIONS

__\--bibtex-file__ _somefile_ Marks some file to be an input for
`bibtex`. Can be given multiple times. Automatic mode will probably
spot all bibtex-able files anyway, so mostly useful in manual mode.

__\--bibtex__ Equivalent to __\--bibtex-file__ _file.aux_.

## INDEX OPTIONS

__\--index-file__ _somefile_ Marks some file to be an input for
`makeindex`. Can be given multiple times.

__\--index-file-suffix__ _extension_ The same as above, but for the file
_file_._extension_.

Each of the following option applies to the preceding `index-file`:

__\--index-style__ _somefile_ Corresponds to option __\-t__ of makeindex.

__\--index-output__ _somefile_ Corresponds to option __\-t__ of makeindex.

__\--index-output-suffix__ _extension_ The same as above, but for the file
_file_._extension_.

__\--index-options__ _string_ Corresponds to other options of makeindex,
separated by spaces, see ["QUOTING"](#quoting).

## DEPENDENCY OPTIONS

__\--depends__ List local files read by compilation and not overwritten
(`.tex`, `.cls`,...)

__\--internals__ List local files both output and read by compilation
(`.aux`...)

__\--outputs__ List local files output by compilation, but not read
(`.log`...).

__\--separator__ _string_ Use this quoted string as separator between
names instead of one space (see ["QUOTING"](#quoting)).

## HELP OPTIONS

__\--help__ Generate this help (long version).

__\--usage__ Generate this help (short version).

__\--man__ Generate man page.

__\--nroff__ Generate man page in NROFF format.

# DESCRIPTION

compile-latex is a program that compiles a TeX file until it reaches a
fixed point. The program assumes that if all the inputs of a program are
the same, then not repeating this program is acceptable; and if any
input of a program changes, then the program is rerun.

compile-latex also manages makeindex. It supports the use of
the `multind` and `index` packages for multiple indexes; however, the
automatic mode only uses the `.idx` -> `.ind` standard index.

compile-latex also manages bibtex. It currently supports only simple
bibliography or multibib package (with hints); bibtopic, biblatex are
not supported).

compile-latex remembers from one compilation to another the index options
it was given. It is not necessary to repeat them (not harmful either).

compile-latex uses strace to determine the files accessed by a program.

# GIVING OPTIONS

Options may come from:

- the command line
- previous invocations (unless __\--forget__ is used)
- the file itself in specially formatted comments
- automatic mode

Some options coming from the command line and previous invocations will
be stored in the `file.md5` file and will be reused at later
invocations. They are the index options, bibtex options and the
__\--manual__ option (since __\--auto__ is on by default, it does not need
to be stored).

Comment lines in the file formatted as follows: one or more percent
char, zero or more spaces, the string `compile-latex`, one or more
spaces, the string `"option"`, zero or more spaces, the string `":"`
and one option (without any excess spaces), will be considered as an
option. These options will not be stored for later invocations, since
they are in the file anyway.

# QUOTING

Each option expects a single argument. Some may require spaces to
separate various parts (e.g. __\--index-options__). If one part needs
spaces (a filename, for example), use `%20` for the space inside the
part. Use `%25` for a litteral `%` and `%3B` for a `;`. Quoting
should almost never be necessary.

# DEBUG

Debug level varies from 0 (silent, except for results of __\--depends__ or
like options) to 7 (very verbose).

The default level is 1 and will print all time consuming operations.

Level 2 will print changed files before launching time consuming operations.

Level 3 will print all operations even skipped ones.

Level 5 will print changed checksums.

# EXAMPLES

    compile-latex file.tex

will compile `file.tex` as many times as necessary, processing index
and bibtex files as many times as necessary too (sometimes, bibtex index
have to be processed several times because of cross-references).

    compile-latex file.tex --manual --index-file-suffix adx \
    --index-output-suffix and \
    --index-options '-g -t /tmp/log%20indexation' \
    --index-file-suffix odx --index-output-suffix ond --bibtex

will compile `file.tex` and do some special treatment for auxiliary
programs. First, normal index `file.idx` will not be processed, as well
as support for multibib (only a normal `bibtex file` will be
issued). However, two index files ending in `.adx` and `.odx` (coming
from the `multind` package for example) will be processed to
`file.and` and the second one to `file.ond`, the first being indexed
with the `makeindex` options `-g` and `-t /tmp/log indexation`
(remark the space in the filename).

# BUGS

Generation of pictures by other programs is not supposed to be done by
`compile-latex`, unless the document uses `write18` to generate the
images (in which case, compile-latex will happily rerun the generation).

# AUTHOR

Copyright Jean-Christophe Dubacq 2012

# COPYING

This work is licensed under the BSD licence. It is explicitly stated
here that the license does not extend to the data managed by the
  program, in case anyone had doubts.
