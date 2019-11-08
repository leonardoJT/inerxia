/*  NOMBRE      : conseguimiento.p
    DESCRIPCION : Conecta El Repositorio De Seguimiento
    LOG         : 28 nov 2007, Creado, Ing. Edilberto Mariño Moya
*/

IF NOT CONNECTED("segui")
THEN DO:
    SESSION:SET-WAIT("general").
    /*CONNECT C:\repositorio\SEGUIMIENTO\segui.db -ld segui -1.*/
    CONNECT segui -ld segui -H 172.16.31.154 -N tcp -S 5060 NO-ERROR.  
    SESSION:SET-WAIT("").
END.
CREATE ALIAS repositorio FOR DATABASE segui.
MESSAGE "USTED HA SELECCIONADO LA BASE DE DATOS DE SEGUIMIENTO SERVIDOR 172.16.31.154" 
    VIEW-AS ALERT-BOX INFORMATION TITLE "CONEXION A BASE DE DATOS".

