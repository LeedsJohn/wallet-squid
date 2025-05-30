# Wallet Squid 💼🦑

Wallet Squid is a command line application to organize notes.

## Installation

You'll have to clone this project and install the dependencies (likely using opam). I
will write better instructions once I go through an install on a new computer or opens
an issue.  Because `core_unix` is a dependency, I doubt this will work on Windows.

⚠️ **Important:** Once you have built `wallet-squid`, I recommend creating an alias in
your shell config to make running the executable easier. For example, if this directory
was in my home directory, I would add the line
`alias ws='~/wallet-squid/_build/default/bin/main.exe'` to my `.zshrc`. After this, I
can run `wallet-squid` using the command `ws` instead of typing the path to the
executable.

### Dependencies

The `fzf` command relies on, you guessed it, [`fzf`](https://github.com/junegunn/fzf).

## Usage

Create a directory to hold your notes.  For example, my directory is in `~/notes`.  This
is your base path.

### Environment Variables:

Every environment variable can be specified with a flag, but I'd recommend [setting the
following environment variables](https://www.freecodecamp.org/news/how-to-set-an-environment-variable-in-linux/):

* `WALLET_SQUID_BASE_PATH` - the path that your notes are held in.
* `WALLET_SQUID_MARKDOWN_EDITOR` - name of the program you want to open markdown files
in (when opened with the `fzf` command).

### Creating Notes

To create a note, simply make a [markdown file](https://www.markdownguide.org/).

⚠️ **Important:** The file name must end in `.md`.

This will be the name of the note. You
can create subdirectories inside of your notes directory - so, if I have a file
`~/notes/subdirectory/note.md`, that note would have the name `subdirectory/note`.

The first line of each file contains the tags for that note.  Separate each tag with a
comma.  Tags must only contain lowercase letters, numbers, dashes ("-"), and underscores
("_").  In addition, each individual tag cannot be longer than 64 characters. Here's
what a note might look like:

```
ocaml, class

# OCaml Class

This is a note about an OCaml class.
```

This note will be tagged with `ocaml` and `class`.

### Tags

Tags can be connected to each other.  For example, I might connect the `ocaml` tag to a
`programming` tag.  (To do this, I would run the command
`ws tag add_connection -from ocaml -to programming`).  Now, every note tagged with `ocaml` will
automatically be tagged with `programming` too. So, when I search for notes tagged with
`programming`, the previous example note will pull up.

These connections are modeled as a graph - more specifically, a [directed acyclic
graph](https://www.ibm.com/think/topics/directed-acyclic-graph).  If any connection
would introduce a cycle, then it will be rejected.  For example, if I add the
connections `a -> b` and `b -> c` and then tried to add a connection from `c -> a`, this
would be rejected because you could follow the path from `a` to `b` to `c` and then back
to `a`.

## Future plans

Check the [issues tab](https://github.com/LeedsJohn/wallet-squid/issues).

## Contributing

I'm not going to lie I'll be surprised if anyone ever wants to contribute to this but it
will be very appreciated if anyone does.  Here's some ways you can contribute:

* Open an issue with feedback or suggestions

* Open an issue and share that you would like to contribute code - at that point, I will
  do my best to help you get set up and discuss what you would like to work on.

## Similar projects

I was inspired after watching a video about [Obsidian](https://obsidian.md/).  This is a
much more featureful tool.

## What does the name mean?

nothing, i just think squids are cool and I kind of imagine a squid swimming around a
wallet or something.

## License

[MIT](https://github.com/LeedsJohn/wallet-squid/blob/main/LICENSE)

