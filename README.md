EnvPatcher
=========
A generic rule-driven environment variables injector.

Overview
--------

**EnvPatcher** is a simple application which aids problem with sharing common variables among several applications configured with standard `.config` files. Now settings may be shared with no great work among applications. Thus you could avoid forbidden settings problem as a developer and downgrade the overall size of `.config`s or gather critical settings in the single place as a maintainer.

The rules are located in `rules.config` by default and are very self-descriptive:

``` erlang
[
    {inets, [
        {some_value, {ref, env_patcher, [some_value]}}
    ]},

    {sasl, [
        {some_value, {ref, env_patcher, [error_logger]}}
    ]}
].
```

Given the first rule you declares that application `inets` will be told that its enviroment variable `some_value` should be equal to one under application `env_patcher` at `{some_value, Value}`. The same story with the second rule for application `sasl`.

Consult sample configuration files in the repository for the details you are intrested in.

Building and configuring
------------------------

In order to build a project you should execute something like that:

```
git clone git://github.com/keynslug/envpatcher.git
cd envpatcher
make compile
```

Then consider consulting sample configuration files in the repository.

