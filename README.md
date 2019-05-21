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
   [[<<"MOTORTRUCK">>],
    [<<"NOT">>,<<"OOT">>,<<"ONTO">>],
    [<<"OOT">>,<<"MOT">>,<<"ONTO">>],
    [<<"MOT">>,<<"TAJ">>,<<"ONTO">>],
    [<<"ONTO">>,<<"STRUCK">>],
    [<<"ONTO">>,<<"TAJ">>,<<"TAJ">>],
    [<<"NOUN">>,<<"STRUCK">>],
    [<<"NOUN">>,<<"TAJ">>,<<"TAJ">>],
    [<<"STRUCK">>,<<"SUCK">>],
    [<<"NOUNS">>,<<"MOTOR">>],
    [<<"MOTOR">>,<<"TRUCK">>],
    [<<"TRUCK">>,<<"USUAL">>],
    [<<"SUCK">>,<<"TAJ">>,<<"TAJ">>],
    [<<"PUCK">>,<<"TAJ">>,<<"TAJ">>],
    [<<"RUCK">>,<<"TAJ">>,<<"TAJ">>],
    [<<"TAJ">>,<<"NOT">>,<<"SUCK">>],
    [<<"NOT">>,<<"OOT">>,<<"SUCK">>],
    [<<"OOT">>,<<"MOT">>,<<"SUCK">>],
    [<<"MOT">>,<<"ORT">>,<<"SUCK">>],
    [<<"ORT">>,<<"OPT">>,<<"SUCK">>],
    [<<"OPT">>,<<"NOT">>,<<"SUCK">>],
    [<<"SUCK">>,<<"NOT">>,<<"NOT">>],
    [<<"PUCK">>,<<"NOT">>,<<"NOT">>],
    [<<"RUCK">>,<<"NOT">>,<<"NOT">>],
    [<<"NOT">>,<<"OOT">>,<<"ORTS">>],
    [<<"OOT">>,<<"MOT">>,<<"ORTS">>],
    [<<"MOT">>,<<"TAJ">>,<<"ORTS">>],
    [<<"ORTS">>,<<"TAJ">>,<<"TAJ">>],
    [<<"OPUS">>,<<"TAJ">>,<<"TAJ">>],
    [<<"OPTS">>,<<"TAJ">>,<<"TAJ">>],
    [<<"TAJ">>,<<"PUS">>,<<"ONTO">>],
    [<<"ONTO">>,<<"PUS">>,<<"PUS">>],
    [<<"NOUN">>,<<"PUS">>,<<"PUS">>]]
```
