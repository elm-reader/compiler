# The Elm Reader

The Elm Reader is a new tool designed to provide a beautiful interactive
environment for **reading** and **exploring** code written in the [Elm
programming language](http://elm-lang.org/).  We hope you'll use the Elm Reader
to **debug** your own projects, to **familiarize** yourself with code written by
others, and to share rich **visual demonstrations** of your algorithms with the
world.

Reading code is a very different experience from writing it, and we've optimized
the design of the Elm Reader to make understanding an unfamiliar codebase as
comfortable and efficient as possible.  In addition to static code navigation
features like type inspection and jump-to-definition, the Elm Reader is based on
a powerful **runtime tracing** system which reveals the values flowing through
every expression in your program.  By leveraging Elm's purely functional design,
we can provide a **nonlinear debugger** without the concept of time, allowing
you to freely explore your program's execution either forwards or backwards.

The Elm Reader is designed to complement, not compete with, the Elm Reactor;
while the Reactor shows the high-level changes to your program's state over
time, the Elm Reader will allow you to navigate deep into your program and
inspect the behavior of individual expressions.  If the Elm Reactor shows *what*
is happening, then the Elm Reader helps illuminate *why* and *how* your program
behaves the way it does.

The Elm Reader is being developed as an unofficial extension to the [Elm
compiler](https://github.com/elm/compiler), and is currently in an extremely
early stage of development.  Questions and contributions are welcome!  Please
contact [William Brandon](https://github.com/selectricsimian/) or [Wilson
Berkow](https://github.com/orgs/elm-reader/people/WilsonBerkow) to learn more.
