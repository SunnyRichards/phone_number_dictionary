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
   [[<<"TAJ">>,<<"NOT">>,<<"RUCK">>],
     [<<"NOT">>,<<"OOT">>,<<"RUCK">>],
     [<<"OOT">>,<<"MOT">>,<<"RUCK">>],
     [<<"MOT">>,<<"ORT">>,<<"RUCK">>],
     [<<"NOUNS">>,<<"MOTOR">>],
     [<<"MOTOR">>,<<"TRUCK">>],
     [<<"ORT">>,<<"OPT">>,<<"RUCK">>],
     [<<"OPT">>,<<"NOT">>,<<"RUCK">>], 
     [<<"SUCK">>,<<"STRUCK">>],
     [<<"SUCK">>,<<"NOT">>,<<"TAJ">>],
     [<<"PUCK">>,<<"STRUCK">>],
     [<<"PUCK">>,<<"NOT">>,<<"TAJ">>],
     [<<"RUCK">>,<<"STRUCK">>],
     [<<"RUCK">>,<<"NOT">>,<<"TAJ">>],
     [<<"ONTO">>,<<"STRUCK">>],
     [<<"ONTO">>,<<"NOT">>,<<"TAJ">>],
     [<<"NOUN">>,<<"STRUCK">>],
     [<<"NOUN">>,<<"NOT">>,<<"TAJ">>],
     [<<"ONTO">>,<<"STRUCK">>],
     [<<"ONTO">>,<<"NOT">>,<<"TAJ">>],
     [<<"NOUN">>,<<"STRUCK">>],
     [<<"NOUN">>,<<"NOT">>,<<"TAJ">>],
     [<<"NOT">>,<<"OOT">>,<<"RUCK">>],
     [<<"OOT">>,<<"MOT">>,<<"RUCK">>],
     [<<"MOT">>,<<"TAJ">>,<<"RUCK">>],
     [<<"ORTS">>,<<"STRUCK">>],
     [<<"ORTS">>,<<"TAJ">>,<<"TAJ">>],
     [<<"OPUS">>,<<"STRUCK">>],
     [<<"OPUS">>,<<"TAJ">>,<<"TAJ">>],
     [<<"OPTS">>,<<"STRUCK">>],
     [<<"OPTS">>,<<"TAJ">>,<<"TAJ">>],
     [<<"TAJ">>,<<"NOT">>,<<"RUCK">>],
     [<<"NOT">>,<<"OOT">>,<<"RUCK">>],
     [<<"OOT">>,<<"MOT">>,<<"RUCK">>],
     [<<"MOT">>,<<"PUS">>,<<"RUCK">>],
     [<<"SUCK">>,<<"STRUCK">>],
     [<<"SUCK">>,<<"PUS">>,<<"TAJ">>],
     [<<"PUCK">>,<<"STRUCK">>],
     [<<"PUCK">>,<<"PUS">>,<<"TAJ">>],
     [<<"RUCK">>,<<"STRUCK">>],
     [<<"RUCK">>,<<"PUS">>,<<"TAJ">>],
     [<<"MOTORTRUCK">>],
     [<<"TRUCK">>,<<"USUAL">>]]
     ```
