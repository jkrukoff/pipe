

# Module pipe #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A set of specialized folds for piping a value through a list of
functions.

<a name="types"></a>

## Data Types ##




### <a name="type-monad">monad()</a> ###


<pre><code>
monad() = fun((<a href="#type-monoid">monoid()</a>, term()) -&gt; term())
</code></pre>




### <a name="type-monoid">monoid()</a> ###


<pre><code>
monoid() = fun((term()) -&gt; term())
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#always-2">always/2</a></td><td>
A function application that always executes the given function.</td></tr><tr><td valign="top"><a href="#apply-2">apply/2</a></td><td>
A function application that expands a list or tuple into multiple
arguments to apply to the given function.</td></tr><tr><td valign="top"><a href="#compose-1">compose/1</a></td><td>
Combine a list of function applications into a single function
application.</td></tr><tr><td valign="top"><a href="#if_not_error-2">if_not_error/2</a></td><td>
A function application that only applies the given function when
the value is not an {error, _} tuple, otherwise the given value
is returned unchanged.</td></tr><tr><td valign="top"><a href="#if_not_exception-2">if_not_exception/2</a></td><td>
A function application that only applies the given function when
the value is not an {error, _} tuple, otherwise the given value
is returned unchanged.</td></tr><tr><td valign="top"><a href="#if_not_throw-2">if_not_throw/2</a></td><td>
A function application that only applies the given function when
the value is not an {error, _} tuple, otherwise the given value
is returned unchanged.</td></tr><tr><td valign="top"><a href="#if_ok-2">if_ok/2</a></td><td>
A function application that only applies the given function when
the value is an {ok, _} tuple, otherwise the given value is
returned unchanged.</td></tr><tr><td valign="top"><a href="#ignore-2">ignore/2</a></td><td>
A function application that ignores the return value of the given
function, instead passing the value through unchanged.</td></tr><tr><td valign="top"><a href="#line-2">line/2</a></td><td>
A pipe function which executes each function in turn, regardless of
the value returned.</td></tr><tr><td valign="top"><a href="#not_error-2">not_error/2</a></td><td>
A pipe function which executes each function only when the returned
value is not of the form {error, _}.</td></tr><tr><td valign="top"><a href="#not_exception-2">not_exception/2</a></td><td>
A pipe function which executes each function only when the returned
value is not of the form {error, _}.</td></tr><tr><td valign="top"><a href="#not_throw-2">not_throw/2</a></td><td>
A pipe function which executes each function only when the returned
value is not of the form {error, _}.</td></tr><tr><td valign="top"><a href="#ok-2">ok/2</a></td><td>
A pipe function which executes each function only when the returned
value is of the form {ok, _}.</td></tr><tr><td valign="top"><a href="#pipe-3">pipe/3</a></td><td>
The generic pipe function.</td></tr><tr><td valign="top"><a href="#via-2">via/2</a></td><td>
Combine a list of function applications with an unapplied single
argument function to create a pipeline stage that can have
customized behaviour.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="always-2"></a>

### always/2 ###

<pre><code>
always(Fun::<a href="#type-monoid">monoid()</a>, Value::term()) -&gt; term()
</code></pre>
<br />

A function application that always executes the given function.
This is also the identity function.

<a name="apply-2"></a>

### apply/2 ###

<pre><code>
apply(Fun::fun((...) -&gt; term()), Value::term()) -&gt; term()
</code></pre>
<br />

A function application that expands a list or tuple into multiple
arguments to apply to the given function. A generic adapter for
when single argument functions are difficult to derive.

<a name="compose-1"></a>

### compose/1 ###

<pre><code>
compose(Monads::[<a href="#type-monad">monad()</a>]) -&gt; <a href="#type-monad">monad()</a>
</code></pre>
<br />

Combine a list of function applications into a single function
application.

<a name="if_not_error-2"></a>

### if_not_error/2 ###

<pre><code>
if_not_error(Fun::<a href="#type-monoid">monoid()</a>, Value::term()) -&gt; term()
</code></pre>
<br />

A function application that only applies the given function when
the value is not an {error, _} tuple, otherwise the given value
is returned unchanged. Best for conditional execution over
functions with a variety of return formats, where a best effort is
required.

The value inside an {ok, _} tuple is unwrapped and used as the
argument to the called function, otherwise the value is used as is.

<a name="if_not_exception-2"></a>

### if_not_exception/2 ###

<pre><code>
if_not_exception(Fun::<a href="#type-monoid">monoid()</a>, Value::term()) -&gt; term()
</code></pre>
<br />

A function application that only applies the given function when
the value is not an {error, _} tuple, otherwise the given value
is returned unchanged. Best for when exceptions must not stop
execution and the calling code is prepared to match on and handle
caught exceptions.

If the called function throws an exception of any type (including
exit signals), it will be caught and converted to the form {error,
{Class, Reason}}.

The value inside an {ok, _} tuple is unwrapped and used as the
argument to the called function, otherwise the value is used as is.

<a name="if_not_throw-2"></a>

### if_not_throw/2 ###

<pre><code>
if_not_throw(Fun::<a href="#type-monoid">monoid()</a>, Value::term()) -&gt; term()
</code></pre>
<br />

A function application that only applies the given function when
the value is not an {error, _} tuple, otherwise the given value
is returned unchanged. Best for conditional execution over poorly
behaved functions which throw exceptions for expected failures.

If the called function throws an exception, it will be caught and
converted to the form {error, {throw, Reason}}.

The value inside an {ok, _} tuple is unwrapped and used as the
argument to the called function, otherwise the value is used as is.

<a name="if_ok-2"></a>

### if_ok/2 ###

<pre><code>
if_ok(Fun::<a href="#type-monoid">monoid()</a>, Value::term()) -&gt; term()
</code></pre>
<br />

A function application that only applies the given function when
the value is an {ok, _} tuple, otherwise the given value is
returned unchanged. Best for conditional execution over well
behaved functions that always return {ok, _} or {error, _}
tuples.

The value inside the {ok, _} tuple is unwrapped and used as the
argument to the called function.

<a name="ignore-2"></a>

### ignore/2 ###

<pre><code>
ignore(Fun::<a href="#type-monoid">monoid()</a>, Value::term()) -&gt; term()
</code></pre>
<br />

A function application that ignores the return value of the given
function, instead passing the value through unchanged. Useful for
functions which are executed solely for their side effects.

<a name="line-2"></a>

### line/2 ###

<pre><code>
line(Start::term(), Funs::[<a href="#type-monoid">monoid()</a>]) -&gt; term()
</code></pre>
<br />

A pipe function which executes each function in turn, regardless of
the value returned.

__See also:__ [pipe:always/2](pipe.md#always-2).

<a name="not_error-2"></a>

### not_error/2 ###

<pre><code>
not_error(Start::term(), Funs::[<a href="#type-monoid">monoid()</a>]) -&gt; term()
</code></pre>
<br />

A pipe function which executes each function only when the returned
value is not of the form {error, _}.

__See also:__ [pipe:if_not_error/2](pipe.md#if_not_error-2).

<a name="not_exception-2"></a>

### not_exception/2 ###

<pre><code>
not_exception(Start::term(), Funs::[<a href="#type-monoid">monoid()</a>]) -&gt; term()
</code></pre>
<br />

A pipe function which executes each function only when the returned
value is not of the form {error, _}. All exceptions are caught
and converted into error tuples.

__See also:__ [pipe:if_not_exception/2](pipe.md#if_not_exception-2).

<a name="not_throw-2"></a>

### not_throw/2 ###

<pre><code>
not_throw(Start::term(), Funs::[<a href="#type-monoid">monoid()</a>]) -&gt; term()
</code></pre>
<br />

A pipe function which executes each function only when the returned
value is not of the form {error, _}. Thrown exceptions are caught
and converted into error tuples.

__See also:__ [pipe:if_not_throw/2](pipe.md#if_not_throw-2).

<a name="ok-2"></a>

### ok/2 ###

<pre><code>
ok(Start::term(), Funs::[<a href="#type-monoid">monoid()</a>]) -&gt; term()
</code></pre>
<br />

A pipe function which executes each function only when the returned
value is of the form {ok, _}. Ensure the initial value is also of
that form, otherwise none of the functions will be executed.

__See also:__ [pipe:if_ok/2](pipe.md#if_ok-2).

<a name="pipe-3"></a>

### pipe/3 ###

<pre><code>
pipe(Apply::[<a href="#type-monad">monad()</a>] | <a href="#type-monad">monad()</a>, Start::term(), Funs::[<a href="#type-monoid">monoid()</a>]) -&gt; term()
</code></pre>
<br />

The generic pipe function. This works as a specialized fold. The
initial value is used as an argument to the first unapplied
function in the given list, the result of that used as the argument
to the next function and so on. The returned value is the result of
the final function.

Control over if each function is to be applied is given to the list
of function applications, which decide what to do based on the
current value to be applied. Function applications execute in left
to right order, with the leftmost being the first able to control
execution.

To customize an individual pipe stage, use pipe:via/2. To pipe over
multiple argument functions, use pipe:apply/2.

__See also:__ [pipe:apply/2](pipe.md#apply-2), [pipe:via/2](pipe.md#via-2).

<a name="via-2"></a>

### via/2 ###

<pre><code>
via(Apply::[<a href="#type-monad">monad()</a>], Start::<a href="#type-monoid">monoid()</a>) -&gt; <a href="#type-monoid">monoid()</a>
</code></pre>
<br />

Combine a list of function applications with an unapplied single
argument function to create a pipeline stage that can have
customized behaviour.

When used with a pipe function such as pipe:line/2, allows for each
stage to have custom error handling.

