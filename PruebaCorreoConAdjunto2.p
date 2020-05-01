DEF VAR lsuc AS LOGICAL.
DEF VAR cMess AS CHARACTER.
RUN smtpmailv5_8c2.p("smtp-relay.gmail.com:25",
"""Prof. Anonimo"" <lcmartinez@fodun.com.co>",
"comunica_no_reply@fodun.com.co^bogota@fodun.com.co","",
"Prueba.pdf:type=application/pdf:filetype=binary",
"D:\SFG\Desarrollo\Prog\Solicitudes\6402643.pdf","Mensaje de prueba con adjunto, copia oculta y responder a Bogota",
"<img src=""https://www.fodun.com.co/Files/Logo/logo-fodun-1.png""/><br>Este es un mensaje de prueba. Con una tilde en Asociaci&oacute;n<br><br>Cordial saludo","type=text/html:charset=utf-8","text",2,NO,"base64","comunica_no_reply@fodun.com.co","ujhw632gh.+12W",
OUTPUT lsuc, OUTPUT cMess).
MESSAGE string(lsuc) + cMess VIEW-AS ALERT-BOX ERROR.
