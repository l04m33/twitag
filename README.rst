######
Twitag
######

A simple tagging bot for Twitter.

#######
Install
#######

1. Clone this repo into your ``<quicklisp-dir>/local-projects`` directory;

2. Also clone `cl-async-oauth`_ into your ``local-projects`` directory;

2. In your REPL (with Quicklisp already set-up),

.. code-block:: lisp

    CL-USER> (ql:quickload :twitag)

.. _cl-async-oauth: https://github.com/l04m33/cl-async-oauth

#####
Usage
#####

To host Twitag, you'll need to create an application on https://apps.twitter.com/ ,
and obtain the credentials (consumer key & consumer secret) for that application.

To launch Twitag, load the system with ``(ql:quickload :twitag)``, and then call
the ``main`` function,

.. code-block:: lisp

    CL-USER> (ql:quickload :twitag)
    ...
    CL-USER> (twitag:main "<consumer key>" "<consumer secret>" "<path to sqlite db file>")

The bot will then present an authorization URL and prompt for a verifier code.
Just retrieve the verifier code from that URL and you are good to go.

To tag someone on Twitter, you may tweet to the bot or send it direct messages.
For example, a tweet saying

    @TwitagBot @someone #funny

Will assign the user ``someone`` a ``funny`` tag. Things other than hashtags are
ignored, so you may instead say

    Hey @TwitagBot, @someone is really #funny!

To see someone's tags, tweet to the bot (or send it direct messages) again, mentioning
the user you want to see, with NO hashtags,

    Hey @TwitagBot, tell me about @someone.

#####
Tests
#####

To run the tests,

.. code-block:: lisp

    CL-USER> (ql:quickload :prove)
    ...
    CL-USER> (asdf:test-system :twitag)

#######
License
#######

MIT
