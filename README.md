# Fault-Tolerant Parsing

The goal of this article is to explain how one can implement a fault-tolerant parser for a simple language, which we will call **L1**.  Here are a few sentences in L1:

```
This [i is [b not]] a test.
```

It would be rendered as "This _is_ _**not**_ a test." The language consists
of ordinary text and _elements_, which have the form 

```
[name body]
```

where the body is itself an element, e.g., plain text, an element, or
a list of elements.  The syntax is thus somewhat like Lisp, where
an elements like `[i foo]` means "render 'foo' in italic.  

A conventional parser reads its input and attempts to convert it to an abstract syntax tree — a data structure that expresses an understanding of the underlying grammar. When such a parser encounters an error, it typically
bails out with a more or less comprehensible error message.  Such behavior
is not suited for interactive language editors, where the text will pass 
in and out of an error state as the user types away.  A _fault-tolerant_ parser should in all cases return an abstract syntax tree that makes sense to the greatest degree possible.



[Error recovery with parser combinators](https://eyalkalderon.com/blog/nom-error-recovery/)

