# Erlang Code Compile

The library is a thin wrapper over Erlang native `compile` module. It provides a convenient way to compile text to beam code and load to VM.

[![Build Status](https://secure.travis-ci.org/fogfish/erlcc.svg?branch=master)](http://travis-ci.org/fogfish/erlcc) 
[![Coverage Status](https://coveralls.io/repos/github/fogfish/erlcc/badge.svg?branch=master)](https://coveralls.io/github/fogfish/erlcc?branch=master) 

## Getting Started

The latest version of the library is available at its `master` branch. All development, including new features and bug fixes, take place on the `master` branch using forking and pull requests as described in contribution guidelines.

If you use rebar3 you can include the library in your project with

```erlang
{erlcc, ".*",
   {git, "https://github.com/fogfish/erlcc", {branch, master}}
}
```

Build library and run the development console

```bash
make && make run
```

Usage

```erlang
%%
%% compile source code to byte code
{ok, Code} = erlcc:compile(hw, <<"-module(hw). -export([say/0]). say() -> <<\"Hello World!\">>.">>).

%%
%% load byte code 
{ok, _} = erlcc:inject(hw, Code).

%%
%% <<"Hello World!">>
hw:say().
```

## License

Copyright 2016 Dmitry Kolesnikov

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
