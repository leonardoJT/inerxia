/*  NOMBRE      : conhistorico.p
    DESCRIPCION : Conecta El Repositorio Histórico
    LOG         : 28 nov 2007, Creado, Ing. Edilberto Mariño Moya
*/

IF NOT CONNECTED("otorga")
THEN DO:
    SESSION:SET-WAIT("general").
    /*CONNECT C:\repositorio\OTORGAMIENTO\otorga.db -ld otorga -1. */
    CONNECT otorga -ld otorga -H 172.16.31.154 -N tcp -S 5050 no-error.  
    SESSION:SET-WAIT("").
END.
CREATE ALIAS repositorio FOR DATABASE otorga.
MESSAGE "USTED HA SELECCIONADO LA BASE DE DATOS DE OTORGAMIENTO SERVIDOR 172.16.31.154" 
    VIEW-AS ALERT-BOX INFORMATION TITLE "CONEXION A BASE DE DATOS".
