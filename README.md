bang
====

###################
SQL:

I've set up a postgres SQL database for now. I'll probably switch over to some NoSQL option soon, but let's take care of one thing at a time.

Once you've installed postgres, you can from the command line:

createdb bang

Then launch psql, and: \i /path/to/setup.sql

which will leave you with a very simple, unpopulated db:

    bang=# \d
                      List of relations
     Schema |          Name           |   Type   | Owner  
    --------+-------------------------+----------+--------
     public | messages                | table    | eschow
     public | messages_message_id_seq | sequence | eschow
     public | room_user               | table    | eschow
     public | rooms                   | table    | eschow
     public | rooms_room_id_seq       | sequence | eschow
     public | user_location           | table    | eschow
     public | users                   | table    | eschow
     public | users_user_id_seq       | sequence | eschow
    (8 rows)

###################
YAWS:

I've included the yaws.conf here for reference, but you should move it to the standard yaws.conf home, and of course change the paths as appropriate. The important bit of the conf file is the &lt;server&gt; block. Check that the docroot and appmod are set correctly.

Assuming you've set up the project in ~/code/bang, you need to compile the src and place the .beams in your erlang path. To do this, compile the your source, move the .beams to ebin/, and add the following line to ~/.erlang:

    code:add_patha("/path/to/home/code/bang/ebin").

###################
ERLANG:

I'm running 64-bit Erlang R16B on Mac OS 10.8.4.

There's not too much code yet, but bang.erl is set up to parse the incoming url and do some initial routing. Note that the teapot resource returns a 418. My initial model is that I've created a handler for three resources, each with a handle/2 method. handle/2 extracts the method and farms out the work. 

Also, note that there's no session control at this point...

I'm using the erlang-rfc4627 library for JSON (available for download at https://github.com/tonyg/erlang-rfc4627) . I've included in the src/ directory for convendience. You'll need to compile and make sure the .beams are in the yaws path. I'm not doing much with it, but I have an example of encoding and decoding in the bang&#95;user.

Next, I'll start interfacing with postgres in order to do something real.
