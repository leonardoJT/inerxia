/*-------------------------------------------------------------------
Input Param : 
     EmailTo    Email del receptor - Un solo receptor
     NameTo     Nombre del receptor
     EmailFrom  Email de emisor y de "responder a"
     NameFrom   Nombre de emisor del mensaje
     EmailCC    email para enviar copia del mensaje
     CCO        Booleano si es con copia oculta o copia normal
     Subject    Asunto del mensaje
     Body       Cuerpo del mensaje en HTML  
     LocalFile  Ruta a un archivo adjunto
     
Output Param: 
     Estado     Devuelve si o no, si se env�a el mensaje o no respectivamente
     Respuesta  Respuesta del servidor de correo

*/
/********** forward declare functions ***********/
DEF INPUT PARAMETER EmailTo         AS CHAR NO-UNDO.
DEF INPUT PARAMETER NameTo         AS CHAR CASE-SENSITIVE NO-UNDO.
DEF INPUT PARAMETER EmailFrom       AS CHAR NO-UNDO.
DEF INPUT PARAMETER NameFrom       AS CHAR CASE-SENSITIVE NO-UNDO.
DEF INPUT PARAMETER EmailCC         AS CHAR NO-UNDO.
DEF INPUT PARAMETER EmailCCO     AS CHAR NO-UNDO.
DEF INPUT PARAMETER Subject         AS CHAR CASE-SENSITIVE NO-UNDO.
DEF INPUT PARAMETER Body            AS CHAR NO-UNDO.
DEF INPUT PARAMETER LocalFile      AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER Estado AS LOGICAL NO-UNDO.
DEF OUTPUT PARAMETER Respuesta AS CHAR NO-UNDO.

DEF VAR nombreArchivo AS CHARACTER INIT "".
DEF VAR tipoArchivo AS CHARACTER INIT "".
DEF VAR emailsCC AS CHARACTER INIT "".
DEF VAR email AS CHARACTER INIT "".
DEF VAR next-space AS INTEGER INIT 0.

NameTo = REPLACE(NameTo, "�", "A").
NameTo = REPLACE(NameTo, "�", "E").
NameTo = REPLACE(NameTo, "�", "I").
NameTo = REPLACE(NameTo, "�", "O").
NameTo = REPLACE(NameTo, "�", "U").
NameTo = REPLACE(NameTo, "�", "U").
NameTo = REPLACE(NameTo, "�", "N").
NameTo = REPLACE(NameTo, "�", "a").
NameTo = REPLACE(NameTo, "�", "e").
NameTo = REPLACE(NameTo, "�", "i").
NameTo = REPLACE(NameTo, "�", "o").
NameTo = REPLACE(NameTo, "�", "u").
NameTo = REPLACE(NameTo, "�", "u").
NameTo = REPLACE(NameTo, "�", "n").
NameFrom = REPLACE(NameFrom, "�", "A").
NameFrom = REPLACE(NameFrom, "�", "E").
NameFrom = REPLACE(NameFrom, "�", "I").
NameFrom = REPLACE(NameFrom, "�", "O").
NameFrom = REPLACE(NameFrom, "�", "U").
NameFrom = REPLACE(NameFrom, "�", "U").
NameFrom = REPLACE(NameFrom, "�", "N").
NameFrom = REPLACE(NameFrom, "�", "a").
NameFrom = REPLACE(NameFrom, "�", "e").
NameFrom = REPLACE(NameFrom, "�", "i").
NameFrom = REPLACE(NameFrom, "�", "o").
NameFrom = REPLACE(NameFrom, "�", "u").
NameFrom = REPLACE(NameFrom, "�", "u").
NameFrom = REPLACE(NameFrom, "�", "n").
Subject = REPLACE(Subject, "�", "A").
Subject = REPLACE(Subject, "�", "E").
Subject = REPLACE(Subject, "�", "I").
Subject = REPLACE(Subject, "�", "O").
Subject = REPLACE(Subject, "�", "U").
Subject = REPLACE(Subject, "�", "U").
Subject = REPLACE(Subject, "�", "N").
Subject = REPLACE(Subject, "�", "a").
Subject = REPLACE(Subject, "�", "e").
Subject = REPLACE(Subject, "�", "i").
Subject = REPLACE(Subject, "�", "o").
Subject = REPLACE(Subject, "�", "u").
Subject = REPLACE(Subject, "�", "u").
Subject = REPLACE(Subject, "�", "n").

nombreArchivo = substring( LocalFile, r-index( LocalFile, "\" ) + 1 ).
CASE substring( LocalFile, r-index( LocalFile, "." ) + 1 ):
    WHEN "pdf" THEN
        tipoArchivo = "type=application/pdf:filetype=binary".
    WHEN "txt" THEN
        tipoArchivo = "type=text/plain:filetype=text".
    WHEN "csv" THEN
        tipoArchivo = "type=text/csv:filetype=text".
    WHEN "doc" OR WHEN "docx" THEN
        tipoArchivo = "type=application/msword:filetype=binary".
    WHEN "xls" OR WHEN "xlsx" THEN
        tipoArchivo = "type=application/vnd.ms-excel:filetype=binary".
    WHEN "xml" THEN
        tipoArchivo = "type=application/xml:filetype=binary".
    WHEN "zip" THEN
        tipoArchivo = "type=application/zip:filetype=binary".
    OTHERWISE
        tipoArchivo = "type=application/octet-stream:filetype=binary".
END CASE.


EmailCC = TRIM(EmailCC," ").
DO WHILE TRUE:
    next-space = INDEX(EmailCC, ",").
    IF next-space = 0 THEN 
      next-space = LENGTH(EmailCC) + 1.
    IF next-space = 1 THEN LEAVE.
    email = SUBSTRING(EmailCC, 1, next-space - 1).
    EmailCC = TRIM(SUBSTRING(EmailCC, next-space + 1)).
    IF emailsCC = "" THEN
        emailsCC = email. 
    ELSE
        emailsCC = emailsCC + " , " + email. 
END.

EmailCCO = TRIM(EmailCCO," ").
DO WHILE TRUE:
    next-space = INDEX(EmailCCO, ",").
    IF next-space = 0 THEN 
      next-space = LENGTH(EmailCCO) + 1.
    IF next-space = 1 THEN LEAVE.
    email = SUBSTRING(EmailCCO, 1, next-space - 1).
    EmailCCO = TRIM(SUBSTRING(EmailCCO, next-space + 1)).
    IF emailsCC = "" THEN
        emailsCC = email + "^B". 
    ELSE 
        emailsCC = emailsCC + " , " + email + "^B". 
END.

RUN smtpmailv5_8c.r(INPUT "smtp.antispamcloud.com:587",
                    INPUT """" + NameTo + """ <" + EmailTo + ">",
                    INPUT """FODUN no-reply"" <comunica@fodun.com.co>^""" + NameFrom + """ <" + EmailFrom + ">",
                    INPUT emailsCC,
                    INPUT nombreArchivo + ":" + tipoArchivo,
                    INPUT LocalFile,
                    INPUT Subject,
                    INPUT Body,
                    INPUT "type=text/html:charset=utf-8",
                    INPUT "text",
                    INPUT 2,
                    INPUT yes,
                    INPUT "base64",
                    INPUT "comunica@fodun.com.co",
                    INPUT "Mercadeo123",
                    OUTPUT Estado,
                    OUTPUT Respuesta).

CREATE Logs.
ASSIGN Logs.Agencia = 1
       Logs.Usuario = "Correo"
       Logs.Fecha = TODAY
       Logs.Observacion = Respuesta + ". Mensaje de: " + EmailFrom + ". Para: " + EmailTo + ". CC: " + emailsCC + ". Asunto: " + Subject + ". " + Body
       Logs.Estado = Estado
       Logs.HoraE = TIME.
