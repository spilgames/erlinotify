erlinotify
==========

Notification of inotify events

## Description

The library interfaces with inotify using erlang NIF. The NIF spawns
off a thread which then polls the inotify file descriptor for any
changes to a watched descriptor.

## Installation

Add the library to your rebar config

```
{
  deps,
  [
   {erlnotify, "0.1.*", {git, "git://github.com/dipthegeezer/erlinotify.git", "HEAD"}},
   ....
  ]
}.

```

## API

```
application:start(ets_manager),
application:start(erlinotify),

%% Watch a file for an event
CB = fun({_Path,_Type,_Event,_Cookie,_Filename}=Var)->
   io:fwrite("Event has happened:~p~n", [Var]),
erlinotify:watch("/path/to/file/or/directory", CB),

%% Do some further stuff

%% Un-watch a file.
erlinotify:unwatch("/path/to/file/or/directory"),

application:stop(ets_manager),
application:stop(erlinotify),

```

## License

(The MIT License)

Copyright (c) 2013 dipthegeezer

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
