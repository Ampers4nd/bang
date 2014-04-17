bang
====

Bang is the beginning of a user management system, using REST and JSON. It currently supports the following operations:

 1. **Create user**

 ```curl
 URL: $HOST/api/1.0/register/
 METHOD: POST
 BODY: {"username": "abc", "password": "def", "data":{ -- arbitrary JSON --}}
 RETURNS: 
     201, {"success":"true","token":"ed418f086c4040b62cb93aef4c02e9e7","user_type":"0"}
     400, {"message":"invalid request"}
     5xx, {"message": "something bad happened"}
 ```

 2. **Validate user**

 ```curl
 URL: $HOST/api/1.0/validation/$TOKEN (valid token returned upon creation of new user)
 METHOD: GET
 BODY: --
 RETURNS: 
     201, {"success":"true","user_type":"1","application_id":"AP_572LP48O7MVOV","client_id":"CL_0J2MF3L08JNBP"}
     400, {"message":"invalid request"}
     401, {"message":"You don't belong here."} - invalid credentials or tried to user is already validated
     404, {"message":"Could not retrieve record"}
     5xx, {"message": "something bad happened"} 
 ```

 3. **Retrieve auth code for validated user (expires in 60 sec)**

 ```curl
 URL: $HOST/api/1.0/form-sign-in (valid token returned upon creation of new user)
 METHOD: POST
 BODY: {"application_id":"${APP_ID}", "client_id":"${CL_ID}", "redirect_uri":"${REDIRECT_URI}" ,"credentials":{"username":"${UNAME}", "password":"${PW}"}}
 RETURNS: 
     201, {"success":"true","auth_code":"XC7TPSBWYLIF"}
     400, {"message":"invalid request"}    
     401, {"message":"Invalid credentials or some other reason"}
     404, {"message":"Could not retrieve record"}
     5xx, {"message": "something bad happened"} 
 ```

 4. **Exchange auth code for session token**

 ```curl
     201, {"application_id":"AP_572LP48O7MVOV","client_id":"CL_0J2MF3L08JNBP","session_token":"83HJ387PJU1J"}
     400, {"message":"invalid request"}    
     401, {"message":"Invalid Auth Code"}
     401, {"message":"Invalid credentials or some other reason"}
     404, {"message":"Could not retrieve record"}
     5xx, {"message": "something bad happened"}
 ```



<!--  SQL is gone, using CouchDB now
####################
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
-->


---------------------------------------------------------
## <a id="dependencies"></a>DEPENDENCIES: 

 * bang_private: This module contains local paths and keys, and I didn't feel good about posting that to github, so I didn't. You'll need to implement this one yourself, and it should start like this:

   ```erl
   -module(bang_private).
   
   -export([mailDomain/0, mailUser/0, mailKey/0, mailFromValidation/0, ganimasToken/0,
       couchEnterpriseURL/0, couchEnterpriseByAppURL/0, couchEnterpriseByClientURL/0,
       couchSessionURL/0, couchRedirectURL/0, couchEnterpriseByCredentialsURL/0, 
       couchSessionByAuthCode/0, couchSessionInvalidate/0]).
   ```


---------------------------------------------------------
## <a id="yaws"></a>Yaws: 

I've included the yaws.conf here for reference, but you should move it to the standard yaws.conf home, and of course change the paths as appropriate. The important bit of the conf file is the &lt;server&gt; block. Check that the docroot and appmod are set correctly.

Assuming you've set up the project in ~/code/bang, you need to compile the src and place the .beams in your erlang path. To do this, compile the your source, move the .beams to ebin/, and add the following line to ~/.erlang:

 ```erl
 code:add_patha("/path/to/home/code/bang/ebin").
 ```

The yaws record definitions are in `/$BANG_DIR/include/yaws_api.hrl`

I've removed this from the commit log, because these definitions are not stable across yaws versions. In particular, the definitions have changed between yaws v1.96 and v1.97. 


---------------------------------------------------------
## <a id="erlang"></a>Erlang: 

I'm running 64-bit Erlang R16B on Mac OS 10.8.5. I'm also using the [erlang-rfc4627 module][2] for processing JSON, so that will need to be installed.

[2]: https://github.com/tonyg/erlang-rfc4627              "erlang rfc"


---------------------------------------------------------
## <a id="couchdb"></a>CouchDB: 

I'm using CouchDB. The DB retrieval code depends on some custom CoucchDB views, so those will need to be set up before anything will work. I'll add some documenation regarding that later.


---------------------------------------------------------
## <a id="curl"></a>curl: 

I'm using curl to test right now. The tests are configured to run against localhost:4443. In the curl/ directory, you'll find several test scripts. It makes the most sense to run them n order like this:

 ```bash
 $ ./testCreate.sh #create new user
 $ ./testValidate.sh VAL_ID #validate user from previous step
 $ ./testAuth.sh APP_ID CL_ID UNAME PW #get auth code for validated user
 $ ./testToken.sh APP_ID CL_ID AUTH_CODE #exchange auth code for session token
 ```

---------------------------------------------------------
## <a id="todo"></a>TODO: 

There's lots to do, but here's my short list: 

 1. Set up for OTP and rebar. The current build process is very messy.
 2. Add unit testing to the project to replace the curl scripts. 
 3. Do something about redirect uris. The auth step checks that the request contains a valid redirect_uri, but there is no way to create them yet, so currently the tests have a hardcoded dummy value.
 4. Better wrapping around erlang-rfc4627, so that I can pass and retrieve strings always, instead of converting back and forth between binary and list data types.


