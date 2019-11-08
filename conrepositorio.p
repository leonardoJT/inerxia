/*  NOMBRE      : conrepositorio.p
    DESCRIPCION : Conecta El Repositorio De Seguimiento
    LOG         : 15 MAY 2008, Creado, Ing. Edilberto Mariño Moya
    NOTA    : USE ESTA CONEXION PARA DESARROLLO
*/

IF NOT CONNECTED("repositorio")
THEN DO:
    SESSION:SET-WAIT("general").
    /*CONNECT C:\repositorio\SEGUIMIENTO\segui.db -ld segui -1.*/
    CONNECT segui -ld repositorio -H 172.16.31.154 -N tcp -S 5060 NO-ERROR.  
    SESSION:SET-WAIT("").
END.

MESSAGE "USTED HA SELECCIONADO LA BASE DE DATOS DE REPOSITORIO(segui) SERVIDOR 172.16.31.154" 
    VIEW-AS ALERT-BOX INFORMATION TITLE "CONEXION A BASE DE DATOS".

