DEF VAR lsuc AS LOGICAL.
DEF VAR cMess AS CHARACTER.
RUN smtpmailv5_8c.p("smtp.antispamcloud.com:587",
"""Prof. Anonimo"" <lcmartinez@fodun.com.co>",
"""Gest. Cartera FODUN"" <comunica@fodun.com.co>^""Gerencia Bog.""<bogota@fodun.com.co>","luiscmart@gmail.com^B",
"Prueba.pdf:type=application/pdf:filetype=binary",
"D:\SFG\Desarrollo\Prog\Solicitudes\6402643.pdf","Mensaje de prueba con adjunto, copia oculta y responder a Bogota",
"<img src=""https://www.fodun.com.co/Files/Logo/logo-fodun-1.png""/><br>Este es un mensaje de prueba. Con una tilde en Asociaci&oacute;n<br><br>Cordial saludo","type=text/html:charset=utf-8","text",2,yes,"base64","comunica@fodun.com.co","Mercadeo123",
OUTPUT lsuc, OUTPUT cMess).
MESSAGE string(lsuc) + cMess VIEW-AS ALERT-BOX ERROR.
