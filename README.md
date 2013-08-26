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

I've set up a create user resource, which can be tested with curl:

curl -v -X POST -H "Content-Type: application/json" -d '{"username":"abc","password":"xyz"}' --user foo:bar http://localhost:8000/user

This simply inserts the username and the sha of the password. I've moved the db interaction to a separate module, so that I can easily replace postgres with another db solution down the line. I've added a helper class that wraps the crypto calls and converts the binary to hexadecimal strings that behave nicely with postgres.

Still no session control...

###################
DEPENDENCIES:

I'm running 64-bit Erlang R16B on Mac OS 10.8.4 with yaws 1.96 and postgres 9.2.4.

The code also requires the following erlang libraries:

1) erlang-rfc4627 (download at https://github.com/tonyg/erlang-rfc4627)

2) epgsql (https://github.com/wg/epgsql)
