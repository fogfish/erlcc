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

[![See LICENSE](https://img.shields.io/github/license/fogfish/erlcc.svg?style=for-the-badge)](LICENSE)

