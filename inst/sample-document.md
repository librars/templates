# Title of the document
This is a test.

## This is a heading (h2)
This is more test.

This _text is italic_, and *so is this*. While __this text is bold__, the same **as this one**.

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

To added code in R to run:

```!r
# Run this r code
paste("hello", "world")
```

Code can be added inline as: `inline verbatin` (no syntax supported for specifying specific languages to highlight). It is possible to run inline as well by using: `!r 1+2`.

## Math
To add math, it is possible to do inline using $\int f$. And block using:

$$
\int_a^b f(x) dx
$$

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
