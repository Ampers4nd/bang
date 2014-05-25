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




-----------------------------------------
# Getting Dependencies

`bang` uses Erlang, Yaws and CouchDB.


## Erlang

 * **Install [erlang][20]**
   ```bash
   $ sudo apt-get install erlang
   ```

## Yaws

 * **Install [yaws][21] http server**

   ```bash
   $ sudo apt-get install yaws
   ```

   or Download and build the yaws source [as recommended][23].


   ```bash
   $ git clone git://github.com/klacke/yaws.git
   $ cd yaws
   $ autoconf
   $ ./configure --help
   $ ./configure --prefix=/usr/local
   $ make
   $ make install
   
   $ sudo apt-get install yaws
   $ yaws --version
   Yaws 1.98
   ```

 * **Start and test the service**
   ```bash
   $ yaws -i
   ```

   Created directories and files owned by group 'yaws' may be inaccessible. Add yourself to the yaws group.
   ```bash
   $ sudo usermod -a -G yaws duko
   ```
   
   Test a response by requesting a page from http://127.0.0.1:8080. The configuration file is `/etc/yaws/conf.d/localhost.conf`.


## CouchDB

 * **Install [couchdb][22]**
   ```bash
   $ sudo apt-get install couchdb
   $ couchdb -V
   couchdb - Apache CouchDB 1.5.0
   ```

 * **Start and test the service**
   ```bash
   $ sudo service couchdb start
   ```

   Test a response by requesting a page from http://127.0.0.1:5984/ The configuration file is `/etc/couchdb/local.ini`.


## erlang-rfc4627

 [erlang-rfc4627][25] is used to process JSON.

 * **Install erlang-rfc4627**
   ```bash
   $ git clone https://github.com/tonyg/erlang-rfc4627.git
   $ cd erlang-rfc4627
   $ make all test-compile
   ```

   *~/.erlang*
   ```erlang
   code:add_patha("/path/to/erlang-rfc4627/ebin/").
   ```


-----------------------------------------
# Get Started

Edit yaws configuration files to use `bang`.

*/etc/yaws/conf.avail/localhost.conf*
```xml
<server localhost>
  port = 8000
  listen = 0.0.0.0
  docroot = /path/to/bang
  dir_listings = true
  auth_log = true
  statistics = true
  appmods = </, bang>
</server>
```

*/etc/yaws/conf.avail/localhost-ssl.conf*
```xml
<server localhost>
  port = 4443
  docroot = /path/to/bang
  listen = 0.0.0.0
  dir_listings = true
  auth_log = true
  <ssl>
    keyfile = /etc/yaws/yaws-key.pem
    certfile = /etc/yaws/yaws-cert.pem
  </ssl>
</server>
```

Run yaws with the defined configuration
```bash
$ yaws -i --conf /etc/yaws/yaws.conf
```

If you installed yaws with a package manager, you may find yaws only starts as root due to file permissions.

The cert files on Ubuntu are symlinks to files owned by root
```bash
yaws-cert.pem -> ../ssl/certs/ssl-cert-snakeoil.pem
yaws-key.pem -> ../ssl/private/ssl-cert-snakeoil.key
```

Make new keys to reference in the configuration file, or Change permissions of these files to the yaws group. Do it for a development environment -not recommended for a production environment
```bash
sudo chown :yaws /etc/ssl
sudo chown :yaws /etc/ssl/certs
sudo chown :yaws /etc/ssl/certs/ssl-cert-snakeoil.pem
sudo chown :yaws /etc/ssl/private
sudo chown :yaws /etc/ssl/private/ssl-cert-snakeoil.key
```

Fake keys can be made with openssl
```bash
$ openssl genrsa -out server-key.pem 1024
$ openssl req -new -key server-key.pem -out server-csr.pem
$ openssl x509 -req -in server-csr.pem -signkey server-key.pem -out server-cert.pem
```

After you've started yaws, `$ bash ./reloadBang.sh` then visit `localhost:8000/api/1.0/teapot`


[20]: http://www.erlang.org/                            "erlang"
[21]: http://yaws.hyber.org/                              "yaws"
[22]: http://couchdb.apache.org/                       "couchdb"
[23]: http://yaws.hyber.org/configuration.yaws      "yaws setup"
[24]: https://bitbucket.org/etc/erlang-web/src/248534cbc1dfd0485aaf145f013698672a7e6f45/lib/yaws-1.80/include/yaws_api.hrl?at=eptic-1.3
[25]: https://github.com/tonyg/erlang-rfc4627   "erlang-rfc4627"

