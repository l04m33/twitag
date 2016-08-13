######
Twitag
######

A simple tagging bot for Twitter.

#######
Install
#######

1. Clone this repo into your ``<quicklisp-dir>/local-projects`` directory;

2. Also clone `cl-async-oauth`_ and `cl-async-twitter`_ into your
   ``local-projects`` directory;

3. In your REPL (with Quicklisp already set-up),

.. code-block:: lisp

    CL-USER> (ql:quickload :twitag)

.. _cl-async-oauth: https://github.com/l04m33/cl-async-oauth
.. _cl-async-twitter: https://github.com/l04m33/cl-async-twitter

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

Log levels can be configured by calling ``vom:config``,

.. code-block:: lisp

    CL-USER> (vom:config :twitag :debug)
    CL-USER> (vom:config :twitag/db :debug)

To tag someone on Twitter, you may tweet to the bot or send it direct messages.
For example, a tweet saying

    @TwitagBot @someone #funny

Will assign the user ``someone`` a ``funny`` tag. Things other than mentioned
users and hashtags are ignored, so you may instead say

    Hey @TwitagBot, @someone is really #funny!

To see someone's tags, tweet to the bot (or send it direct messages) again, mentioning
the user you want to see, with NO hashtags,

    Hey @TwitagBot, tell me about @someone.

To search for tagged users, send some tags to the bot instead,

    Hey @TwitagBot, is there any #funny guy?

The bot will then randomly choose at most 10 user names for you. When using tweets
to show these names, the bot will skip the ``@`` so that it won't annoy the users
mentioned.

#####
Tests
#####

To run the tests,

.. code-block:: lisp

    CL-USER> (ql:quickload :twitag-test)
    ...
    CL-USER> (asdf:test-system :twitag)

#######
License
#######

MIT
