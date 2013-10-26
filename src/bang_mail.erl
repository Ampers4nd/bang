-module(bang_mail).

-export([sendValidation/1]).

validationCC() -> "".
validationBCC() -> bang_private:mailFromValidation().
validationSubject() -> "Whoa...".
validationMessage() -> "Look out behind you!!!".

url() -> "https://api.mailgun.net/v2/" ++ bang_private:domain() ++ "/messages".

sendValidation(To) ->
	sendMail(bang_private:mailFromValidation(), To, validationCC(), validationBCC(), validationSubject(), validationMessage()).

sendMail(From, To, CC, BCC, Subject, Message) ->
	JSONEncodedBody = bang_json:encodedJson([{"from", From}, {"to", To}, {"cc", CC}, {"bcc", BCC}, {"subject", Subject}, {"text", Message}]),
	bang_https:post(bang_private:mailUser(), bang_private:mailKey(), url(), JSONEncodedBody). 