DEFINE VARIABLE oSuccessful AS LOGICAL.
DEFINE VARIABLE cMess AS CHAR.

RUN smtpmailv5_8c.p (
                 INPUT "smtp.gmail.com:465", /*mailhub*/
                 INPUT "luiscmart@gmail.com", /*EmailTo--- pulled from the database table*/
                 INPUT "lcmartinez@fodun.com", /*EmailFrom --- loginName from the database table*/
                 INPUT "",/*EmailCC*/
                 INPUT "", /*Attachments*/
                 INPUT "", /*LocalFiles*/
                 INPUT "Feliz cumpleaños", /*Subject*/
                 INPUT "Te deseamos un hermoso feliz cumpleaños <img src='https://www.fodun.com.co/userfiles//logoweb-01(5).jpg'/>", /*Body---- a char/string*/
                 INPUT "type=text/html:charset=us-ascii:filetype=ascii", /*MIMEHeader*/
                 INPUT "TEXT", /*BodyType*/
                 INPUT 3, /*Importance*/
                 INPUT TRUE, /*doAUth*/
                 INPUT "base64",
                 INPUT "lcmartinez@fodun.com", /* loginName */
                 INPUT "G0d1sMyP4st0R", /* password */
                 OUTPUT oSuccessful, /*oSuccessful*/
                 OUTPUT cMess ) NO-ERROR.  /*vMessage*/
