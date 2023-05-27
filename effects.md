# Interpreting Algebraic Effects

## Introduction

**NOTE:** This is an early draft and very much a work in progress.

This is an informal set of notes after implementing algebraic effect in an interpreter.
No formal descriptions are used, though papers with formal semantics are linked as references.
They focus on just interpretation with not type checking nor compilation pass.

No code examples are included, but an implementation of some parts can be found at:
[here](https://github.com/Gwenio/abyss-lisp/blob/main/src/context.lisp).

## Naive Deep Handlers

We will start with a basic model that roughly follows: [An Introduction to Algebraic Effects and Handlers][0]

A **context**, short for 'evaluation context', structure will be referred to,
throughout this document.
For this first section, it will have to members: a `stack` and the `next` **context**.
The stack is whatever the interpreter would use to approximate a normal program
stack.
The `next` member is used to have a **context stack**.

The details of the stack should not matter for the content of this document.
But as a basic assumption, it can be considered a singly linked list of
closures that take one argument.
The stack used for the interpreter and the stack used for the code being
interpreted are assumed separate.

The **context stack** will be assumed to be stored in a global variable for
simplicity.
It could be a thread local variable or passed as a parameter through the
interpreter's internals.

The top of the **context stack** is the **current context**.
The stack of the **current context** is the stack the interpreter will be using
as it evaluates code.

### Installing a Handler

Whenever some code is to be evaluated with a handler, the interpreter will:

- Push a **handler frame** on top of the stack of the **current context**.
- Push a new **context** on the **context stack**.
- Proceed to evaluate the code on the new **current context**.

### Naive Effect Handling

Effect handling is triggered when an **effect** is **perform**ed (the term used varies,
**perform** is used as it does not seem to be used outside of effects,
unlike 'raise' for example).
A language may have special syntax for **perform**ing an effect,
but it can just look like a normal function call.

When handling is triggered, the top of the **context stack** is popped off.
Then an object containing the effect to handle, the argument(s) provided,
and the previously popped **context** are passed to the **handler frame** of the new **current context**.

The **handler frame** will construct a **continuation** as described in [the next section](#basic-continuations).

The handler will then select what to do based on the received effect.
The selection can be done with an associative container such as a hash table.
The actions associated with effects are basically closures that expect to receive a **continuation**
and any arguments provided when the effect was performed.

When the selected action is called, normal evaluation proceeds until another effect
is **performed** or the **continuation** is **resumed**.

### Unhandled Effects

If the handler does not handle the specific effect, then it will:

- push a frame onto the **current context**'s stack which will resume the **continuation**
  with the value passed to it
- **perform** the effect it does not handle

Thus responsibility for handling the effect is passed up the **context stack**.
The **continuation** the handler has will be resumed with the result of **performing** the effect.
And if the effect is handled and the associated **continuation** **resumed**,
the result will be forwarded down to the original source of the effect.

### Basic Continuations

Note: Outside of the [multi-shot continuation](#multi-shot-continuations) section,
all continuations are one-shot.
Meaning they can only be **resume**d at most once.

The basic structure for **continuation** objects will contain a `context` and
a `handler`.
The `context` is the one popped off the **context stack** when activating a
**handler frame**.
The `handler` is the one that the **handler frame** represents.

For the simple model, you could save the stack of the context instead.
However, in later sections the structure of a context will get more complex,
making saving the context more practical.
The exception would be for [multi-shot continuations](#multi-shot-continuations).

To resume a continuation:

- `handler` is used to create a **handler frame** on the **current context**
- `context` is pushed on top of the **context stack**, becoming the new **current context**

The defining feature of 'deep' handlers is that the handler a **continuation**
was created for is re-installed when the **continuation** is **resume**d.
This means all effects that would be handled before one is **perform**ed
will still be handled by the same handler they would have been forehand.
However, effects that would not have been handled previously may now have a handler.

### Normal Returns

When the code a handler was installed for is completed, the interpreter needs
to return to the previous context.
So when the stack of the **current context** is 'empty', we would want to pop
the top of the **context stack** and pass the current return value to a
frame on the stack of the new **current context**.

The simplest way to automate this, and avoid overhead of checking if the stack
is 'empty' every time we pop a frame, is to place a special frame at the bottom
of the stack for each new **context**.
The special frame would take care of switching context.

With a **handler frame** occupying the top of the stack, there is a choice to be
made about how to return to a context.

For our simple model, it is enough to pop the top frame before switching.
After all, the top frame will always be the **handler frame**.

To more closely follow [An Introduction to Algebraic Effects and Handlers][0],
a builtin **return effect** would exist.
This will be covered in the [Normal Return Handler](#normal-return-handler) section.

### Initial Context

In a compiled language with type checked effects, failure to handle an effect
would result in a compile time error.

In a purely interpreted implementation, that luxury is unavailable.
So the interpreter will have to handle any effects that the user does not.

A clean way to do so is to have an **initial context** at the bottom of the
context stack.
This context will have a handler that performs the default action for any builtin effects
and notify of unhandled user defined effects.

The stack frame for exiting interpretation can either live on this **initial context**
or its first child.
Having the **exit frame** on the first child is likely better, but putting it on the
**initial context** may be simpler and have little impact on performance.

## Better Deep Handlers

The biggest issue with the handlers as described previously is when an effect is not handled.
Passing the effect up the **context stack** and the result back down to the source works.
But it does a good bit of extra work to get there.

A quick summary of changes to data structures for this section:

- **context** objects now have a `handler` member.
- **continuation** objects now have a list of **context** objects instead of single **context**.

Then new handler member of **context** objects replaces the **handler frame**.
Any time a **handler frame** would have been pushed on top of a stack belonging to a **context**,
set the `handler` member instead.

When an effect is **perform**ed, search down the **context stack** for a handler that handles the effect.
The **current context** is not included in the search.
While searching, keep a reference to the previous **context**, which is initially the **current context**.

Once a handler is found that handles the effect, a **continuation** is constructed.
Set the **next context** member of the previous **context** from the search to the **current context**.
The **context** who's handler is being used is now set as the top of the **context stack**.
The previous **context** is used as the **context member** of the new **continuation**.
Meanwhile the **handler member** of the **continuation** is the handler from the new **current context**.

It is fine if the **context** of the **continuation** points to itself,
which will happen if it is the previous **current context**.

To **resume** the new version of **continuations**:

- Set the **current context**'s `handler` member to the **continuation**'s `handler` member.
- Store the **current context** in a temporary location referred to as `x`.
- Swap `x` with the `next` member of the **continuation**'s `context`.
- Set the top of the **context stack** with `x`.

### Null Handler

For this improved model, it is recommended to set the `handler` member of **contexts** to a *null handler*
at any point the top of its `stack` would not be a **handler frame** under the naive model.
That is, a handler that does not handle any effects.

At least for release build.
In debug and testing builds, it can have a hard error to find issues.

This will cause less of an issue that leaving a real handler in place
or making **context**s not have a handler.

## Normal Return Handler

One aspect of [An Introduction to Algebraic Effects and Handlers][0]
that was initially skipped is being able to handle *normal returns*.

Before getting into the how, it is important to know why such a feature would be desired.
The purpose is to enable emulating mutable state in the absence of a better alternative.

This is important because just being able to handle normal returns is not quite enough for that goal.
However, it is a requirement.

For just handling *normal returns*, all that is needed is a builtin effect that cannot be manually **perform**ed.
However, this builtin effect can be handled.
Then when a **context** is done, it would **perform** the builtin effect to return to its result.

Unlike other effects, no **continuation** is provided when handling a *normal return*.

Every handler would need to handle the *normal return* effect, even if it just passes the value on.

If the implementation does not have [shallow handlers](#shallow-handlers), the process can be simplified.
The *normal return* handler can can be appended to the code the handler is associated with;
thus, removing the need to **perform** an effect.
In that case, the builtin effect is only use used to identify
which part of a handler definition is for *normal returns*.

### Emulating Mutable State

As previously mentioned, to emulate mutable state with just handlers more than *normal returns* is needed.

First, any handler intended to emulate mutable state needs to handle effects in a 'lazy' manner.
To do so, for each effect it would return a closure to handle the effect.
This includes the *normal return* handler.
And every one of the closures would expect the same number of parameters.

As closure are returned, any point a **continuation** is **resume**d
would need to call the returned closure.
Thus the parameter(s) passed to the 'lazy handlers' act as the 'mutable state'.

If the final state is desired in addition to the result after completion,
then the *normal return* handler would need to return both.
Or more accurately, the closure it returns would need to return both.

Finally, a means of setting the initial state is needed.
There are two options for doing so.

- Have the construct for attaching a handler to code accept an initial state.
- Or have a dedicated builtin effect for that purpose.

In the latter case, if present in a handler it would be implicitly **perform**ed
prior to evaluating the code the handler is attached to.
It would immediately **resume** its **continuation** after setting
things up such that the initial state will be passed to the closure returned next time an effect is handled.

If better means of mutable state are available, then this added complexity is likely not worthwhile.

## Shallow Handlers

This section will cover how to adept the previously described model to support [Shallow Effect Handlers][1].

Adding support for 'shallow' handlers is fairly trivial.
All it takes is a means of **resuming** a **continuation**.
Where 'deep' would re-install the handler in the **continuation**,
'shallow' would install the alternative provided.

If effects were statically checked, adding this feature would be more involved.

If only shallow handlers are desired, then just remove the ability to
**resume** a **continuation** without specifying a handler.
The **continuation**s would not need to store a `handler`.

The big question about shallow handlers is if they should be available.

They make reasoning about code harder that if only deep handlers are available.
And without static checks, this can quickly cause code to become an incomprehensible mess.

## Bidirectional Effects

[Handling Bidirectional Control Flow][2]

## Tunneling

[Abstraction-safe effect handlers via tunneling][3]

## Deterministic Cleanup

[Algebraic Effect Handlers with Resources and Deep Finalization][4]

## Multi-shot Continuations

.

## Miscellaneous

### Effects and Threads

.

### Foreign Functions

.

## References

- [An Introduction to Algebraic Effects and Handlers][0]
- [Shallow Effect Handlers][1]
- [Handling Bidirectional Control Flow][2]
- [Abstraction-safe effect handlers via tunneling][3]
- [Algebraic Effect Handlers with Resources and Deep Finalization][4]
- [Representing Control in the Presence of One-Shot Continuations][5]

[0]: https://www.eff-lang.org/handlers-tutorial.pdf
[1]: https://homepages.inf.ed.ac.uk/slindley/papers/shallow-extended.pdf
[2]: https://www.cs.cornell.edu/andru/papers/ufo/bidirectional-effects.pdf
[3]: https://dl.acm.org/doi/10.1145/3290318
[4]: https://www.microsoft.com/en-us/research/uploads/prod/2018/04/resource-v1.pdf
[5]: https://legacy.cs.indiana.edu/~dyb/pubs/call1cc.pdf
