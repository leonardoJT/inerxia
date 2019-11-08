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
     Estado     Devuelve si o no, si se envía el mensaje o no respectivamente
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

NameTo = REPLACE(NameTo, "Á", "A").
NameTo = REPLACE(NameTo, "É", "E").
NameTo = REPLACE(NameTo, "Í", "I").
NameTo = REPLACE(NameTo, "Ó", "O").
NameTo = REPLACE(NameTo, "Ú", "U").
NameTo = REPLACE(NameTo, "Ü", "U").
NameTo = REPLACE(NameTo, "Ñ", "N").
NameTo = REPLACE(NameTo, "á", "a").
NameTo = REPLACE(NameTo, "é", "e").
NameTo = REPLACE(NameTo, "í", "i").
NameTo = REPLACE(NameTo, "ó", "o").
NameTo = REPLACE(NameTo, "ú", "u").
NameTo = REPLACE(NameTo, "ü", "u").
NameTo = REPLACE(NameTo, "ñ", "n").
NameFrom = REPLACE(NameFrom, "Á", "A").
NameFrom = REPLACE(NameFrom, "É", "E").
NameFrom = REPLACE(NameFrom, "Í", "I").
NameFrom = REPLACE(NameFrom, "Ó", "O").
NameFrom = REPLACE(NameFrom, "Ú", "U").
NameFrom = REPLACE(NameFrom, "Ü", "U").
NameFrom = REPLACE(NameFrom, "Ñ", "N").
NameFrom = REPLACE(NameFrom, "á", "a").
NameFrom = REPLACE(NameFrom, "é", "e").
NameFrom = REPLACE(NameFrom, "í", "i").
NameFrom = REPLACE(NameFrom, "ó", "o").
NameFrom = REPLACE(NameFrom, "ú", "u").
NameFrom = REPLACE(NameFrom, "ü", "u").
NameFrom = REPLACE(NameFrom, "ñ", "n").
Subject = REPLACE(Subject, "Á", "A").
Subject = REPLACE(Subject, "É", "E").
Subject = REPLACE(Subject, "Í", "I").
Subject = REPLACE(Subject, "Ó", "O").
Subject = REPLACE(Subject, "Ú", "U").
Subject = REPLACE(Subject, "Ü", "U").
Subject = REPLACE(Subject, "Ñ", "N").
Subject = REPLACE(Subject, "á", "a").
Subject = REPLACE(Subject, "é", "e").
Subject = REPLACE(Subject, "í", "i").
Subject = REPLACE(Subject, "ó", "o").
Subject = REPLACE(Subject, "ú", "u").
Subject = REPLACE(Subject, "ü", "u").
Subject = REPLACE(Subject, "ñ", "n").

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
