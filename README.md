# plop

> There still is a role for that, there has always been a role for that. And one way to talk about when this is appropriate is that notion of birthing process. A point in time where you're starting to create a new value, and we'll talk more precisely about values in a minute, and in setting up that value you need to manipulate memory for instance, you need to manipulate places. That's completely OK. I would never advocate languages that didn't let you, for instance, manipulate the contents of an array, because during this process, you need to be able to do that in order to write efficient programs. But this birthing process is a window that ends, and it ends whenever the thing that you've made is going to become visible to any other part of your program. At that point, it's become a fact, it's become perceptible, and then you have to stop doing place oriented programming, because as we'll see, it's not a fit for the models we're trying to build. So, this use of places to create values, or this use of places to represent values under the hood is an implementation detail. Of course we have to use places, computers have memory and they have disks. But what's important is that a program is not about places, right ? It's Information Technology, it's not Technology Technology, right ? We've taken abstractions of the technology and raised them up to be what the program is about, and that's an error. There was a reason we had to do it and there's not anymore.
> Rich Hickey, [The value of values](https://www.infoq.com/presentations/Value-Values)

## Documentation

```clj
(require '(plop.core :refer [place]))
```

The `place` macro defines a set of lexically scoped variables.

Syntax :
```clj
(place [symbol1 init-expression1
        symbol2 init-expression2
        ,,,]
  body-expression1
  body-expression2
  ,,,)
```
