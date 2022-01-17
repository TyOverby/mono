# 06 - Testing

Traditional approaches for testing web applications can be
infuriating.  With tools like selenium or puppeteer, there’s an entire
headless browser running in the background, and not only do you need
to find a way to reconfigure the app or library for testing, slow test
execution and race condition-related bugs are a constant companion.

Fortunately, Bonsai is built with functional purity in mind:

- **Virtual_dom:** A type that you’re likely familiar with, `Vdom.Node.t` is a
  pure representation of the impure DOM.

- **Incremental:** While Incremental is primarily an implementation detail, the
  library itself is used to accelerate pure functions.

Bonsai itself is all about building a DAG of components in a declarative
manner, with statically tracked inputs to the graph, as well as a state machine
for user interactivity.  State machines and pure functions are fantastic for
structuring programs, but they also lend themselves particularly well to
testing.

Instead of running an entire browser, the `Bonsai_web_test` library makes the
assumption that the `Virtual_dom` is the source of truth, and this lets us run
all of our tests in just OCaml, permitting us to use other utilities like
`ppx_expect_test`.

## Basics of testing: printing the VDOM for a component

Let's see what a test for the simplest Bonsai component would look like.

First the component:

```ocaml
let hello_world : Vdom.Node.t Computation.t =
  Bonsai.const (Vdom.Node.span [] [ Vdom.Node.text "hello world" ])
;;
```

And now the test:

```ocaml
module Handle = Bonsai_web_test.Handle
module Result_spec = Bonsai_web_test.Result_spec

let%expect_test "it shows hello world" =
  let handle = Handle.create (Result_spec.vdom Fn.id) hello_world in
  Handle.show handle;
  [%expect {| <span> hello world </span> |}]
;;
```

With this very basic test, we can see two important aspects of testing using
Bonsai: creating a `Handle.t`, and using it to print the contents of the
component.  The first argument to `Handle.create` is a value returned by
`Bonsai_web_testing.Result_spec.vdom`, which finds a value of type `Vdom.Node.t`
inside of the result of the component.  For many applications and components that
you'll want to test, the component simply has the type `Vdom.Node.t
Computation.t`, so passing the identity function to `Result_spec.vdom` is sufficient.
(We'll see later that more complex result specs can be used to test more complex
components).

Finally, `Handle.show` will print the contents of the current virtual-dom returned
by the component.

## A more dynamic component

`Bonsai.const` can be useful, but it's certainly not the most exciting.  To
spice things up, let's build a component that operates on its input:

```ocaml
let hello_user (name : string Value.t) : Vdom.Node.t Computation.t =
  return
  @@ let%map name = name in
  Vdom.Node.span [] [ Vdom.Node.textf "hello %s" name ]
;;
```

Now, in your app, the `name` parameter may be a `Value.t` that comes
from the result of another computation, or it may originate from a
`Bonsai.Var.t` that is updated from an RPC (or something similar to an RPC).
To make things easier for us, let's use `Bonsai.Var.t` to get a mutable handle
on a `Value.t` and see what happens when we change the Var.

```ocaml
let%expect_test "shows hello to a user" =
  let user_var = Bonsai.Var.create "Bob" in
  let user = Bonsai.Var.value user_var in
  let handle = Handle.create (Result_spec.vdom Fn.id) (hello_user user) in
  Handle.show handle;
  [%expect {| <span> hello Bob </span> |}];
  Bonsai.Var.set user_var "Alice";
  Handle.show handle;
  [%expect {| <span> hello Alice </span> |}]
;;
```

As expected, after changing the `Var.t`, the contents in the DOM are updated!

For tests like this, where the contents of a component are printed more than
once, we have a helper function that will print the diff between two versions
of the view: `Handle.show_diff`.  This is how you'd write the example above:

```ocaml
let%expect_test "shows hello to a user" =
  let user_var = Bonsai.Var.create "Bob" in
  let user = Bonsai.Var.value user_var in
  let handle = Handle.create (Result_spec.vdom Fn.id) (hello_user user) in
  Handle.show handle;
  [%expect {| <span> hello Bob </span> |}];
  Bonsai.Var.set user_var "Alice";
  Handle.show_diff handle;
  [%expect
    {|
    -1,1 +1,1
    -|<span> hello Bob </span>
    +|<span> hello Alice </span> |}]
;;
```

While the diff in this instance isn't particularly illuminating, when testing
components that produce hundreds of lines of output, it can be *much* easier to
only review the diff.

## A more interactive component

Having values flow into the computation from outside is only one aspect of
Bonsai's dynamism.  Bonsai also permits components to maintain a state machine
that can be transitioned by actions stemming from a user interacting with the
view.  Before talking about testing these components, let's first build one.

Here, we actually use the `hello_user` component defined previously, but the
`string Value.t` comes from the component-local state instead of a `Var`:

```ocaml
let hello_textbox : Vdom.Node.t Computation.t =
  let%sub state, set = Bonsai.state [%here] (module String) ~default_model:"" in
  let%sub message = hello_user state in
  return
  @@ let%map message = message
  and set = set in
  Vdom.Node.div
    []
    [ Vdom.Node.input [ Vdom.Attr.on_input (fun _ text -> set text) ] []; message ]
;;
```

This component is fully self-contained -- its type is `Vdom.Node.t
Computation.t`, but that interior state is changeable by typing into the
`<input>` text-box.

Testing it is similar to the first component that we tested. We'll start out with
just printing its starting state, but after that, `Handle.input_text` makes an
appearance, allowing us to trigger the `on_input` event listener.

```ocaml
let%expect_test "shows hello to a specified user" =
  let handle = Handle.create (Result_spec.vdom Fn.id) hello_textbox in
  Handle.show handle;
  [%expect
    {|
    <div>
      <input oninput={handler}> </input>
      <span> hello  </span>
    </div> |}];
  Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:"Bob";
  Handle.show_diff handle;
  [%expect
    {|
    -1,4 +1,4
      <div>
        <input oninput={handler}> </input>
    -|  <span> hello  </span>
    +|  <span> hello Bob </span>
      </div> |}];
  Handle.input_text handle ~get_vdom:Fn.id ~selector:"input" ~text:"Alice";
  Handle.show_diff handle;
  [%expect
    {|
    -1,4 +1,4
      <div>
        <input oninput={handler}> </input>
    -|  <span> hello Bob </span>
    +|  <span> hello Alice </span>
      </div> |}]
;;
```

The parameters to `Handle.input_text` are

1. The `handle`.
2. `~get_vdom`: for some components, the result of the computation will be a
   record.  A function is needed to pick out the vdom node that you actually
   care about interacting with.  In our case, the computation returned just a
   vdom node, so we can use the identity function.
3. `~selector`: this is a css selector that can be used to find the actual element
   in the vdom that we are going to type into.
4. `~text`: the text to type into the text box.

## A component that exposes an injection function

Many components return a computation whose result contains "inject" functions (a
function which returns `unit Vdom.Effect.t`).  These functions can be used to
provide nearby components access to a principled way of interacting with
the state machine internal to that component.

A great example of this would be the `Bonsai.state` component, which returns
a value of this type:

<!-- $MDX skip -->
```ocaml
('model * ('model -> unit Ui_effect.t)) Computation.t
```

The second part of the tuple inside of the result is what we'd call an "injection function",
and it can be called to set the internal state.

Testing `Bonsai.state` (or really, any component that exposes an injection
function) will require a custom view spec and a new `Handle` function.

Without further ado, the test:

```ocaml
module State_view_spec = struct
  type t = string * (string -> unit Vdom.Effect.t)
  type incoming = string

  let view (view, _) = view
  let incoming (_, incoming) = incoming
end

let%expect_test "test Bonsai.state" =
  let component : (string * (string -> unit Vdom.Effect.t)) Computation.t =
    Bonsai.state [%here] (module String) ~default_model:"hello"
  in
  let handle = Handle.create (module State_view_spec) component in
  Handle.show handle;
  [%expect {| hello |}];
  Handle.do_actions handle [ "world" ];
  Handle.show handle;
  [%expect {| world |}]
;;
```

Instead of using the `Result_spec.vdom` helper function like before, we need to
define our view-spec module that caters specifically to the type returned by
`state`.  Of note are the functions that extract the "view", and the inject
function.

Then, in the actual test, we call the new `do_actions` function, which passes
its arguments on to the value setter function that the view spec extracted.

## A component which depends on time

Sometimes the behavior of a component depends on time. For example,
a double-click component will want to check whether two clicks happened close
together. An event simpler example is a basic clock component whicch might be
implemented in the following way:

<!-- $MDX file=../examples/time/src/bonsai_time_example.ml,part=untestable-clock-component -->
```ocaml
let _untestable_component =
  let now = Incr.Clock.watch_now Incr.clock |> Bonsai.Incr.to_value in
  return (now >>| Time_ns.to_string >>| Vdom.Node.text)
;;
```

A large problem with this implementation is that there is no way to write a test
for it. The rendered DOM output will contain a timestamp from when the test was
run. Each time the test runs, the output timestamp will be different, thus
causing the test to always fail.

The correct solution is to use `Incr.Clock.with_clock` to gain access to the
incremental clock that was passed to the Bonsai graph.

<!-- $MDX file=../examples/time/src/bonsai_time_example.ml,part=testable-clock-component -->
```ocaml
let component =
  let%sub now = Bonsai.Incr.with_clock Incr.Clock.watch_now in
  return (now >>| Time_ns.to_string >>| Vdom.Node.text)
;;
```

To test this component reliably, we must create a new clock for the Bonsai graph
to use.

<!-- $MDX file=../examples/time/test/app_test.ml,part=test-clock-component -->
```ocaml
let%expect_test _ =
  let clock = Incr.Clock.create ~start:Time_ns.epoch () in
  let handle = Handle.create ~clock (Result_spec.vdom Fn.id) component in
  Handle.show handle;
  [%expect {| 1970-01-01 00:00:00.000000000Z |}];
  Incr.Clock.advance_clock_by clock (Time_ns.Span.of_sec 2.0);
  Handle.show handle;
  [%expect {| 1970-01-01 00:00:02.000000000Z |}]
;;
```

The `with_clock` function has the following signature:

```
val with_clock : (Incr.Clock.t -> 'a Incr.t) -> 'a Computation.t
```

It allows for the incremental computation of some value that depends on the
current time, while still keeping the logic testable.

## Summary

So far we've learned how to test a number of different aspects of Bonsai
components:

1. Components that have dynamic input
2. Stateful components that have an interactive view
3. Stateful components that return an inject function
4. Components that depend on time

It should go without saying that you could have a component with all four, and
you'd be able to write comprehensive and deterministic tests.
