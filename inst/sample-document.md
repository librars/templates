# Title of the document
This is a test.

## This is a heading (h2)
This is more test.

This _text is italic_, and *so is this*. While __this text is bold__, the same **as this one**.

## Cognitive load
The idea is to reduce the amount of syntactic constructs to memorize:

- Headings and sections: Use the `#` token.
- Text formatting (bold, italic): Use the `*` and `_` tokens.
- Links, references, images: Use the `[]`, `!` and `()` tokens.
- Code blocks: Use the backtick token.
    - Verbatim
    - Executable code
    - Math
- Conditional blocks: Use the `{}` tokens.

## Links and images
This is [a link](http://some.net).

![An image here](path/to/img.jpg "optional title")

![Another image][my-image]

[my-image]: path/to/img.jpg "optional title"

## Code
Verbatim code can be added like this:

```
This is a code
```

```js
// This is code with a specified language
(function(){ console.log })();
```

The inline version is `code inline`. To add code in R to run:

```run
![code-1]
# Run this r code and display the result
5 + 4
```

Which can be referenced as code !(code-1).

```run.silent
# Run this r code silently
a <- paste("hello", "world")
```

It is possible to run inline as well by using: `run 1+2`.

## Math
To add math, it is possible to do inline using `math \int f`. And block using:

```math
\int_a^b f(x) dx
```

And with a label:

```math
![eq-1]
\int_a^b f(x) dx
```

To be referenced like: equation !(eq-1)

## Labels
A label can be applied to a block by using an anchor in text ![anchor-1].

To reference it, use !(anchor-1).

- Environments will automatically assign a number as counter.
- Text anchors will use the assigned title if present.

## Environments
A generic environment is declared with the `#!<env-name> <title>`:

#Theorem: Helmholtz's theorem
![theo-helm]
A _field_ is uniquely defined by its divergence and curl.

#proof:
Let `math s` be `math f : \mathbb{R} \mapsto \mathbb{R}` in set `math \Omega`.

#theorem: Helmholtz's theorem II
![theo-helm]
A _field_ is uniquely defined by its divergence and curl.


The one before is theorem !(theo-helm) A triple LF indicates the end of the environment.

#! Proposition
A title-less proposition:

```math
a^2 + b^2 = c^2
```


And more.
An environment begins with the `#<token>:` line and end in either of these cases:

- A double line feed is encountered.
- A new environment or heading is started.

It is not supported to nest environments.

## Conditionals
A piece can be hidden or displayed using conditional blocks:

{{if b1}}

### Dynamical content
Stuff in here might be shown or not depending on whether `b1` is defined or not.

{{end}}
