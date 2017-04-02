# purescript-signal-time-travel

A time travel debugger for [purescript-signal](https://github.com/bodil/purescript-signal)


# usage

Provide your `channel` and the `update` function that you want to wrap to the `initialize` function:

```purescript
timeTravel <- TimeTravel.initialize channel update
```

And use the wrapped `update` function provided by the `timeTravel` object instead of your original one:

```purescript
foldp timeTravel.update initialState (Channel.subscribe channel)
```

The wrapped `update` function captures the action and state transitions to create the time travel debugger 
breadcrumb. It will update the signal like your original `update` function would.

The `prev` and `next` functions move back and forth along the breadcrumb. When these functions are used, the 
app transitions to that given action and state, the pointer is updated to reflect the current position on the 
breadcrumb, and it _will not_ push values onto the breadcrumb. In contrast, actions that do _not_ come from the 
`prev` and `next` functions, such as normal actions sent from your app, _will_ be pushed onto the breadcrumb, and the 
pointer will be moved to the end of the breadcrumb.

See the [purescript-starter-template](https://github.com/Risto-Stevcev/purescript-starter-template) for a simple 
example use case.


# license

Licensed under the MIT license
