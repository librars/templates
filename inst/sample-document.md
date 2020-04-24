# Title of the document
This is a test.

## This is a heading (h2)
This is more test.

This _text is italic_, and *so is this*. While __this text is bold__, the same **as this one**.

## Cognitive load
The idea is to reduce the amount of syntactic constructs to memorize:

- Headings and sections
- Text formatting (bold, italic)
- Links, references, images
- Code blocks
    - Verbatim
    - Executable code
    - Math
- Conditional blocks

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

```!
# Run this r code and display the result
5 + 4
```

```!silent
# Run this r code silently
a <- paste("hello", "world")
```

It is possible to run inline as well by using: `! 1+2`.

## Math
To add math, it is possible to do inline using `m \int f`. And block using:

```m
\int_a^b f(x) dx
```

And with a label:

```m
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

#! theorem Helmholtz's theorem
![theo-helm]
A _field_ is uniquely defined by its divergence and curl.

#!proof
Let $s$ be.

#! theorem Helmholtz's theorem II
![theo-helm]
A _field_ is uniquely defined by its divergence and curl.


The one before is theorem !(theo-helm) A triple LF indicates the end of the environment.

#! proposition
A title-less proposition:

$$
a^2 + b^2 = c^2
$$


And more.
An environment begins with the `#!` line and end in either of these cases:

- A double line feed is encountered.
- A new environment is started.

It is not supported to nest environments.

## Latex
To render and embed the result of a latex expression:

```{latex}
\begin{tikz}
...
\end{tikz}
```

## Conditionals
A piece can be hidden or displayed using conditional blocks:

{{if b1}}

### Dynamical content
Stuff in here might be shown or not depending on whether `b1` is defined or not.

{{end}}
