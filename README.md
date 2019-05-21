phone_number_dictionary
=====

an erlang application for converting phone number to their respective words from the given dictionary

Build & Running Application
---------------------------

    $ ./rebar3 compile
    $ ./rebar3 shell

QuickStart
----------

#### to jump straight into searching for the given phone number ####

```erlang
1>  dictionary_server:search_number(6686787825).
[[<<"NOT">>,<<"OOT">>,<<"MOT">>],
 [<<"ORT">>,<<"OPT">>],
 [<<"ONTO">>,<<"NOUN">>],
 [<<"NOUNS">>,<<"MOTOR">>],
 [<<"TRUCK">>,<<"USUAL">>],
 [],
 [<<"SUCK">>,<<"PUCK">>,<<"RUCK">>],
 [<<"MOTORTRUCK">>]]
2>  dictionary_server:search_number(9442461377).
[[<<"WIG">>,<<"ZIG">>],
 [<<"AIM">>,<<"AGO">>,<<"BIO">>,<<"BIN">>,<<"AIN">>],
 [],
 [<<"WHICH">>],
 [],[],[],[]]
```
