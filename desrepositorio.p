/*  NOMBRE      : desrepositorio.p
    DESCRIPCION : DesConecta El Repositorio
    LOG         : 28 nov 2007, Creado, Ing. Edilberto Mariño Moya
*/

IF CONNECTED(LDBNAME(2))
THEN DO:
    MESSAGE "Usted Se Dispone A Desconectar La Base De Datos 'repositorio' "
            PDBNAME(2)
            SKIP(2)
            "Continua?"
            VIEW-AS ALERT-BOX INFORMATION 
                BUTTONS YES-NO 
            TITLE "DESCONECTANDO LA BASE DE DATOS" UPDATE lsino AS LOGICAL.
    IF lsino
    THEN DO:
        DISCONNECT "repositorio".
        MESSAGE     "USTED ACABA DE DESCONECTAR LA BASE DE DATOS  'REPOSITORIO'"
                    VIEW-AS ALERT-BOX INFORMATION TITLE "DESCONEXION A BASE DE DATOS".
    END.
END.
ELSE MESSAGE "LA BASE DE DATOS REPOSITORIO NO SE ENCUENTRA CONECTADA"
        VIEW-AS ALERT-BOX INFORMATION.
