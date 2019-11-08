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
DEF VAR EmailTo         AS CHAR INIT "lcmartinez@fodun.com.co".
DEF VAR NameTo         AS CHAR INIT "Luis Martinez".
DEF VAR EmailFrom       AS CHAR INIT "cajimenez@fodun.com.co".
DEF VAR NameFrom       AS CHAR INIT "Carlos Jimenez".
DEF VAR EmailCC         AS CHAR INIT "leonardo_ie@hotmail.com^B , luiscarma@hotmail.com".
DEF VAR CCO     AS LOGICAL INIT TRUE.
DEF VAR Subject         AS CHAR INIT "Respuesta a su solicitud 2 CCO".
DEF VAR Body            AS CHAR INIT "Este mensaje tiene 1 destinatario, un emisor y 2 destinatarios CC".
DEF VAR LocalFile      AS CHAR INIT "".
DEF VAR Estado AS LOGICAL NO-UNDO.
DEF VAR Respuesta AS CHAR NO-UNDO.

DEF VAR nombreArchivo AS CHARACTER INIT "".
DEF VAR tipoArchivo AS CHARACTER INIT "".

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

RUN smtpmailv5_8c.p(INPUT "smtp.antispamcloud.com:587",
                    INPUT """" + NameTo + """ <" + EmailTo + ">",
                    INPUT """FODUN no-reply"" <comunica@fodun.com.co>^""" + NameFrom + """ <" + EmailFrom + ">",
                    INPUT EmailCC + "^B",
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

MESSAGE string(Estado) + " " + string(Respuesta) VIEW-AS ALERT-BOX INFORMATION.
