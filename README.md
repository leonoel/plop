# plop
> There still is a role for that, and there will always be a role for that. And one way to talk about when that's appropriate is this notion of birthing process. A point in time where you're starting to create a new value, \[...\] and in setting up that value you need to manipulate memory for instance, you need to manipulate places. That's completely OK. I would never advocate languages that didn't let you, for instance, manipulate the contents of an array, because during this process, you need to be able to do that in order to write efficient programs. But this birthing process is a window that ends, and it ends whenever the thing that you've made is going to become visible to any other part of your program. At that point, it's become a fact, it's become perceptible, and then you have to stop doing place oriented programming, because \[...\] it's not a fit for the models we're trying to build. So, this use of place to create values, or this use of place to represent values under the hood is an implementation detail. Of course we have to use places, computers have memory and they have disks. But what's important is that a program is not about places, right ? It's Information Technology, it's not Technology Technology, right ? We've taken abstractions of the technology and raised them up to being what the program is about, and that's an error. There was a reason why we had to do it but we don't anymore. - Rich Hickey, [The value of values](https://www.infoq.com/presentations/Value-Values)

## Maturity
Alpha. Users should expect breaking changes in future versions.

## Installation
Artifacts are released to Clojars.

Leiningen coordinates :
```clj
[plop "a.1"]
```

## Documentation
The API stands in the single namespace `plop.core`.
```clj
(require '(plop.core :refer [place]))
```

The `place` macro defines a set of lexically scoped variables. Variables are bound to optionally type-hinted symbols, hold a reference to a value, are implicitly dereferenced each time they are accessed, and are assignable with `set!`.

The state of variables is not synchronized. This is a dangerous, expert-only feature, and if the implications of unsynchronized mutable state publication are not obvious to you, you should not use this macro.

Syntax :
```clj
(place [symbol1 init-expression1
        symbol2 init-expression2
        ,,,]
  body-expression1
  body-expression2
  ,,,)
```

A location is first created to store the current state of each variable. Then initialization expressions are evaluated sequentially, each result being assigned to its variable. Finally the body is evaluated and the result of last expression is returned.

Binding scope differs from `let` in that every variable defined in the block is scoped to the whole block. That is, a initialization expression may refer to its own variable or to another variable defined later in the block.

## License
Licensed under the [Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html) (the same as Clojure)
