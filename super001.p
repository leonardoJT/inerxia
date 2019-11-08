/*
    Nombre: super001.p
    Descripcion: Superprocedimiento
    Log: Por cada función
*/

/*
D I R E C T O R I O  D E  F U N C I O N E S

*/
DEF TEMP-TABLE tbh NO-UNDO
    FIELD ctbla AS CHAR
    FIELD ccmpo AS CHAR
    FIELD htbla AS HANDLE 
    FIELD hcmpo AS HANDLE
    INDEX tblacmpo  ctbla ccmpo
    INDEX htbla ccmpo.
EMPTY TEMP-TABLE tbh.
{incluido\StoreTableBuffers.i} /* crea los handles de las tablas fijas */


FUNCTION fFchaaaaammdd RETURN INTEGER(daFcha AS DATE):
    /*  Descripcion: Devuelve una fecha en formato aaaammdd
        creado: 23 oct 2007, Ing. Edilberto Mariño Moya
    */
    RETURN YEAR(daFcha) * 10000 + MONTH(dafcha) * 100 + DAY(dafcha).
END FUNCTION. /* FUNCTION fFchaaaaammdd */


FUNCTION fGetTblasYDscrpciones RETURN CHAR():
    /*  Descripcion: Devuelve una lista separada por chr(1) de parejas 
        label, tabla separados a su vez por chr(2)
        creado: 12 oct 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR cRtrno AS CHAR NO-UNDO.
    FOR EACH bdcentral._file NO-LOCK
        WHERE
            bdcentral._file._tbl-type = "T":
        cRtrno = cRtrno + 
            (   IF bdcentral._file._file-label = ? 
                THEN bdcentral._file._file-name 
                ELSE 
                (IF bdcentral._file._file-label = "" 
                 THEN bdcentral._file._file-name 
                 ELSE bdcentral._file._file-label)) + CHR(2) + bdcentral._file._file-name + CHR(1).
    END.
    crtrno = crtrno + " CAMPO CALCULADO" + CHR(1) + "CALCULADO".
    RETURN crtrno.
END FUNCTION. /* FUNCTION fGetTblasYDscrpciones */


FUNCTION fGetBufferHandle RETURNS HANDLE (ctbla AS CHARACTER):
    /*  Descripcion: obtiene el handle estatico de la tabla 
        creado: 12 oct 2007, Ing. Edilberto Mariño Moya */
    FIND FIRST ttTableBuffers WHERE cTableName = ctbla NO-ERROR.
    IF NOT AVAILABLE ttTableBuffers 
    THEN DO:
        MESSAGE "ERROR CRITICO:" SKIP(2) "BUFFER Estático Para La Tabla '" + ctbla + "' No Definido. " SKIP " Actualice StoreTableBuffers.i Y Recompile super001.p"
            VIEW-AS ALERT-BOX ERROR TITLE "ERROR".
        RETURN ?.
    END.
    RETURN hTableHandle.
END FUNCTION. /* FUNCTION fGetBufferHandle */


FUNCTION fNoVrDscncdo RETURN CHAR(cv AS CHAR):
    /* Descripcion: valida si el valor es ? y devuelve "" de lo contrario devuelve el valor
        Creado: 12 oct 2007, Ing. Edilberto Mariño Moya */
    RETURN IF cv = ? THEN "" ELSE replace(cv,CHR(10)," ").
END FUNCTION. /* FUNCTION fNoVrDscncdo */


FUNCTION fGetCmposTbla RETURN CHAR(ctbla AS CHAR):
    /* Descripcion: devuelve una lista de los campos separados por chr(1)
    NOTA: LOS CAMPOS Y TABLAS QUE NO HAYAN SIDO USADOS EN EL REPOSITORIO
    Creado: 16 oct 2007
    devuelve 5 entradas separadas por chr(1).
    Cada entrada a su vez se separa por chr(2).
    1 ENTRADA: Lista de nombre
    2 ENTRADA: Lista de Labels
    3 ENTRADA: Tipos de datos
    4 ENTRADA: Formato Del Campo
    5 ENTRADA: Numero de extents definidos
     */
    DEF VAR h AS HANDLE NO-UNDO.
    DEF VAR h1 AS HANDLE NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR crtrno AS CHAR NO-UNDO.
    DEF VAR c AS CHAR NO-UNDO EXTENT 5.
    IF ctbla = "CALCULADO"
    THEN DO:
        c[1] = "CALINT" + CHR(2) + "CALCHAR" + CHR(2) + "CALDAT" + CHR(2) + "CALDEC" + CHR(2) + "CALLOG".
        c[2] = "INTEGER" + CHR(2) + "CHARACTER" + CHR(2) + "DATE" + CHR(2) + "DECIMAL" + CHR(2) + "LOGICAL".
        c[3] = "INTEGER" + CHR(2) + "CHARACTER" + CHR(2) + "DATE" + CHR(2) + "DECIMAL" + CHR(2) + "LOGICAL".
        c[4] = "999" + CHR(2) + "X(8)" + CHR(2) + "99/99/99" + CHR(2) + "999.99" + CHR(2) + "YES/NO".
        c[5] = "0" + CHR(2) + "0" + CHR(2) + "0" + CHR(2) + "0" + CHR(2) + "0".
        CRTRNO = "".
        DO i = 1 TO 5:
            crtrno = crtrno + trim(c[i],CHR(2)) + (IF i = 5 THEN "" ELSE CHR(1)).
        END.
        RETURN crtrno.
    END.
    h = fGetBufferHandle(ctbla).
    IF h = ? THEN RETURN "".

    DO i = 1 TO h:num-FIELDS:
        h1 = h:BUFFER-FIELD(i).
        IF  not(CAN-FIND(FIRST conf_repositorio WHERE conf_repositorio.tablaorigen = ctbla AND conf_repositorio.campoorigen = h1:NAME) 
            AND h1:EXTENT = 0)
        THEN DO:
            c[1] = c[1] + fNoVrDscncdo(h1:NAME) + CHR(2).
            c[2] = c[2] + fNoVrDscncdo(h1:LABEL) + CHR(2).
            c[3] = c[3] + fNoVrDscncdo(h1:DATA-TYPE) + CHR(2).
            c[4] = c[4] + fNoVrDscncdo(h1:FORMAT) + CHR(2).
            c[5] = c[5] + STRING(h1:EXTENT) + CHR(2).
        END.
    END.
    DO i = 1 TO 5:
        crtrno = crtrno + trim(c[i],CHR(2)) + (IF i = 5 THEN "" ELSE CHR(1)).
    END.
    RETURN crtrno.
END. /* FIN FUNCTION fGetCmposTbla */

FUNCTION fGetDtosCmposTbla RETURN CHAR(ctbla AS CHAR):
    /* Descripcion: devuelve una lista de los campos separados por chr(1)
    Creado: 22 oct 2007
    devuelve 5 entradas separadas por chr(1).
    Cada entrada a su vez se separa por chr(2).
    1 ENTRADA: Lista de nombre
    2 ENTRADA: Lista de Labels
    3 ENTRADA: Tipos de datos
    4 ENTRADA: Formato Del Campo
    5 ENTRADA: Numero de extents definidos
     */

    DEF VAR h AS HANDLE NO-UNDO.
    DEF VAR h1 AS HANDLE NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR crtrno AS CHAR NO-UNDO.
    DEF VAR c AS CHAR NO-UNDO EXTENT 5.
    IF ctbla = "CALCULADO"
    THEN DO:
        c[1] = "CALINT" + CHR(2) + "CALCHAR" + CHR(2) + "CALDAT" + CHR(2) + "CALDEC" + CHR(2) + "CALLOG".
        c[2] = "INTEGER" + CHR(2) + "CHARACTER" + CHR(2) + "DATE" + CHR(2) + "DECIMAL" + CHR(2) + "LOGICAL".
        c[3] = "INTEGER" + CHR(2) + "CHARACTER" + CHR(2) + "DATE" + CHR(2) + "DECIMAL" + CHR(2) + "LOGICAL".
        c[4] = "999" + CHR(2) + "X(8)" + CHR(2) + "99/99/99" + CHR(2) + "999.99" + CHR(2) + "YES/NO".
        c[5] = "0" + CHR(2) + "0" + CHR(2) + "0" + CHR(2) + "0" + CHR(2) + "0".
        CRTRNO = "".
        DO i = 1 TO 5:
            crtrno = crtrno + trim(c[i],CHR(2)) + (IF i = 5 THEN "" ELSE CHR(1)).
        END.
        RETURN crtrno.
    END.

    h = fGetBufferHandle(ctbla).
    IF h = ? THEN RETURN "".

    DO i = 1 TO h:num-FIELDS:
        h1 = h:BUFFER-FIELD(i).
        c[1] = c[1] + fNoVrDscncdo(h1:NAME) + CHR(2).
        c[2] = c[2] + fNoVrDscncdo(h1:LABEL) + CHR(2).
        c[3] = c[3] + fNoVrDscncdo(h1:DATA-TYPE) + CHR(2).
        c[4] = c[4] + fNoVrDscncdo(h1:FORMAT) + CHR(2).
        c[5] = c[5] + STRING(h1:EXTENT) + CHR(2).
    END.
    DO i = 1 TO 5:
        crtrno = crtrno + trim(c[i],CHR(2)) + (IF i = 5 THEN "" ELSE CHR(1)).
    END.
    RETURN crtrno.
END. /* FIN FUNCTION fGetDtosCmposTbla */


FUNCTION fGetCmposDspnbleRpstrio RETURN CHAR(ctbla AS CHAR):
    /*  Descripcion: Devuelve los campos disponibles del repositorio
        Creado: 12 oct 2007, Ing. Edilberto Mariño Moya 
        devuelve 5 entradas separadas por chr(1).
        Cada entrada a su vez se separa por chr(2).
        1 ENTRADA: Lista de nombre
        2 ENTRADA: Lista de Labels
        3 ENTRADA: Tipos de datos
        4 ENTRADA: Formato Del Campo
        5 ENTRADA: Numero de extents definidos
    */

    DEF VAR h AS HANDLE NO-UNDO.
    DEF VAR h1 AS HANDLE NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR crtrno AS CHAR NO-UNDO.
    DEF VAR c AS CHAR NO-UNDO EXTENT 5.
    DEF VAR ctpo AS CHAR NO-UNDO.
    DEF VAR ctpos AS CHAR NO-UNDO.
    DEF VAR cabrviatra AS CHAR NO-UNDO.
    ctpos = "integer,character,date,decimal,logical".
    cabrviatra = "int,char,dat,dec,log".
    ctpo = ENTRY(2,ctbla,CHR(1)).
    i = LOOKUP(ctpo,ctpos,",").
    IF i = 0  THEN RETURN "".
    ctpo = ENTRY(i,cabrviatra,",").
    
    h = fGetBufferHandle(entry(1,ctbla,CHR(1))).
    IF h = ? THEN RETURN "".

    DO i = 1 TO h:num-FIELDS:
        h1 = h:BUFFER-FIELD(i).
        IF  h1:NAME BEGINS ctpo AND NOT can-find(FIRST conf_repositorio WHERE conf_repositorio.campodestino = h1:NAME)
        THEN DO:
            c[1] = c[1] + fNoVrDscncdo(h1:NAME) + CHR(2).
            c[2] = c[2] + fNoVrDscncdo(h1:LABEL) + CHR(2).
            c[3] = c[3] + fNoVrDscncdo(h1:DATA-TYPE) + CHR(2).
            c[4] = c[4] + fNoVrDscncdo(h1:FORMAT) + CHR(2).
            c[5] = c[5] + STRING(h1:EXTENT) + CHR(2).
        END.
    END.
    DO i = 1 TO 5:
        crtrno = crtrno + trim(c[i],CHR(2)) + (IF i = 5 THEN "" ELSE CHR(1)).
    END.
    RETURN crtrno.
END. /* FUNCTION fGetCmposDspnbleRpstrio */


FUNCTION fLabelTbla RETURN CHAR(ctbla AS CHAR):
    /*  DESCRIPCION: DEVUELVE EL LABEL DE LA TABLA
        LOG: CREADO, 18 OCT 2007, ING. EDILBERTO MARIÑO MOYA
    */
    DEF VAR crtrno AS CHAR NO-UNDO.
    crtrno = "".
    FIND bdcentral._file NO-LOCK
        WHERE
            bdcentral._file._file-name = ctbla NO-ERROR.
    IF AVAILABLE bdcentral._file
    THEN DO:
        crtrno = IF bdcentral._file._file-label-sa = ? THEN bdcentral._file._file-name ELSE bdcentral._file._file-label-sa.
    END.
    RETURN crtrno.
END FUNCTION. /* FUNCTION fLabelTbla */


FUNCTION fLabelCmpo RETURN CHAR(cTblaCmpo AS CHAR):
    /*  DESCRIPCION: DEVUELVE EL LABEL DEL CAMPO
        LOG: CREADO, 18 OCT 2007, ING. EDILBERTO MARIÑO MOYA
    */
    DEF VAR cTbla AS CHAR NO-UNDO.
    DEF VAR cCmpo AS CHAR NO-UNDO.
    DEF VAR crtrno AS CHAR NO-UNDO.
    cTblaCmpo = cTblaCmpo + CHR(1).
    cTbla = ENTRY(1,cTblaCmpo,CHR(1)).
    ccmpo = ENTRY(2,cTblaCmpo,CHR(1)).
    FOR EACH bdcentral._file NO-LOCK
        WHERE
            bdcentral._file._file-name = ctbla,
        EACH bdcentral._field NO-LOCK
            WHERE
            bdcentral._field._file-recid = RECID(bdcentral._file) 
        AND bdcentral._field._field-name = ccmpo:
        crtrno = bdcentral._field._label.
        LEAVE.
    END.
    RETURN crtrno.
END FUNCTION. /* FUNCTION fLabelCmpo */

FUNCTION fVldaNmbreCmpo RETURN CHAR(c AS CHAR):
    /*  DESCRIPCION: VALIDA QUE EL NOMBRE DE UN CAMPO SOLAMENTE CONTENGA CARACTERES PERMITIDOS
        LOG: CREADO, 18 OCT 2007, ING. EDILBERTO MARIÑO MOYA
    */
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR cRtrno AS CHAR NO-UNDO.
    DEF VAR c1 AS CHAR NO-UNDO.
    DEF VAR clsta AS CHAR NO-UNDO.
    clsta = "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,0,1,2,3,4,5,6,7,8,9,-,_".
    DO i = 1 TO LENGTH(c):
        c1 = SUBSTRING(c,i,1).
        IF NOT(CAN-DO(clsta,c1)) 
        THEN DO:
            crtrno = "El Nombre Contiene Caracteres NO Permitidos".
            LEAVE.
        END.
    END.
    RETURN crtrno.    
END FUNCTION. /* FUNCTION fVldaNmbreCmpo */

FUNCTION fGetCmpoHndle RETURN HANDLE(PHtbla AS HANDLE,PCCmpo AS CHAR):
    /*
        DESCRIPCION :   RETORNA EL HANDLE DE UN CAMPO
        LOG         :   CREADO, 22 OCT 2007, ING. EDILBERTO MARIÑO MOYA
    */
    DEF VAR  i AS INTEGER NO-UNDO.
    DEF VAR h1 AS HANDLE NO-UNDO.
    DEF VAR hRtrno AS HANDLE NO-UNDO.
    IF can-find(tbh 
        WHERE
            tbh.htbla   = phtbla
        AND tbh.ccmpo   = Pccmpo)
    THEN DO:    
        FIND tbh NO-LOCK 
                WHERE
                    tbh.htbla   = phtbla
                AND tbh.ccmpo   = pccmpo NO-ERROR.
        hrtrno = tbh.hcmpo.
    END.
    ELSE DO:
        DO i = 1 TO phTbla:NUM-FIELDS:
            h1 = phTbla:BUFFER-FIELD(i).
            IF h1:NAME = pcCmpo
            THEN do:
                hRtrno = h1.
                LEAVE.
            END.
        END.
        CREATE tbh.
        ASSIGN  tbh.ctbla = phtbla:NAME
                tbh.ccmpo = pccmpo
                tbh.htbla = phtbla
                tbh.hcmpo = hrtrno.
    END.
    RETURN hrtrno.
END FUNCTION. /* FUNCTION fGetCmpoHndle */

FUNCTION fddtt RETURN LOGICAL(INPUT hT AS HANDLE).
    /*
        DESCRIPCION :   recorre temp-table dinamico y muestra los datos
        LOG         :   CREADO, 22 OCT 2007, ING. EDILBERTO MARIÑO MOYA
    */

    DEFINE VARIABLE hBufHndle AS HANDLE.
    DEFINE VARIABLE hQHndle AS HANDLE.
    DEFINE VARIABLE hFieldHndle AS HANDLE     NO-UNDO.
    DEFINE VARIABLE i AS INTEGER    NO-UNDO.
    
/*     ASSIGN hBufHndle = hT:DEFAULT-BUFFER-HANDLE. */
    
    CREATE QUERY hQHndle.
    hQHndle:SET-BUFFERS(ht).
    hQHndle:QUERY-PREPARE( "for each " + hT:NAME + " no-lock").
    hQHndle:QUERY-OPEN.
OUTPUT TO c:\tmp\exito.txt.
    REPEAT i = 1 TO ht:NUM-FIELDS:
        hFieldHndle = ht:BUFFER-FIELD(i).
        PUT UNFORMATTED hFieldHndle:NAME ";".
    END.
    PUT SKIP.
    REPEAT i = 1 TO ht:NUM-FIELDS:
        hFieldHndle = ht:BUFFER-FIELD(i).
        PUT UNFORMATTED hFieldHndle:LABEL ";".
    END.
    PUT SKIP.
    REPEAT:
        hQHndle:GET-NEXT.
        IF hQHndle:QUERY-OFF-END THEN LEAVE.
        REPEAT i = 1 TO ht:NUM-FIELDS:
            hFieldHndle = ht:BUFFER-FIELD(i).
            PUT UNFORMATTED hFieldHndle:BUFFER-VALUE ";".
        END.
        PUT SKIP.
    END.
    OUTPUT CLOSE.
END FUNCTION. /* FUNCTION fddtt */


FUNCTION fLlnaRpstrio RETURN CHAR(daFchaPrcso AS DATE):
    /*
        DESCRIPCION :   LLENA REPOSITORIO
        LOG         :   CREADO, 19 OCT 2007, ING. EDILBERTO MARIÑO MOYA
    */
    DEF VAR hRpstrio AS HANDLE NO-UNDO.
    DEF VAR hCRpstrio AS HANDLE NO-UNDO.
    DEF VAR cDtosCmpo AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR c AS CHAR NO-UNDO EXTENT 5.
    DEF VAR htrepo AS HANDLE NO-UNDO.
    DEF VAR htrepoDBH AS HANDLE NO-UNDO.
    DEF VAR hCmpo AS HANDLE NO-UNDO.
    DEF VAR hClientes AS HANDLE NO-UNDO.
    hRpstrio = fGetBufferHandle("repositorio"). 
    hCRpstrio = fGetBufferHandle("conf_repositorio"). 
    
    /* CREA TEMP-TABLE LIKE repositorio, SEGUN LA DEFINICION CONSIGNADA EN conf_repositorio */
    CREATE TEMP-TABLE htrepo.
    htrepo:ADD-LIKE-FIELD("fecha" ,"repositorio.Fecha").
    htrepo:ADD-LIKE-FIELD("nit" ,"repositorio.Nit").
    htrepo:ADD-LIKE-FIELD("numerocredito" ,"repositorio.NumeroCredito"). 
    htrepo:ADD-LIKE-FIELD("ano" ,"repositorio.NumeroCredito"). 
    htrepo:ADD-LIKE-FIELD("mes" ,"repositorio.NumeroCredito"). 
    htrepo:ADD-LIKE-FIELD("dia" ,"repositorio.NumeroCredito"). 
    htrepo:ADD-LIKE-FIELD("secuencialmes" ,"repositorio.NumeroCredito"). 

    FOR EACH conf_repositorio NO-LOCK
        WHERE
            conf_repositorio.estado = 1
        BY conf_repositorio.codigo: /* SOLAMENTE LOS CAMPOS ACTIVOS */
        cDtosCmpo = fGetDtosCmposTbla(conf_repositorio.tablaorigen).
        DO i = 1 TO 5:
            c[i] = ENTRY(i,cDtosCmpo,CHR(1)).
        END.
        i = LOOKUP(conf_repositorio.campoorigen,c[1],CHR(2)).
        htrepo:ADD-NEW-FIELD(IF NOT conf_repositorio.tablaorigen = "CALCULADO" THEN conf_repositorio.campodestino ELSE conf_repositorio.NomCampoCal,
            conf_repositorio.tipoorigen,
            0,
            ENTRY(i,c[4],CHR(2)),
            ?,
            IF NOT conf_repositorio.tablaorigen = "CALCULADO" THEN ENTRY(i,c[2],CHR(2)) ELSE conf_repositorio.DesCampoCal).
    END.
    htrepo:TEMP-TABLE-PREPARE("trepo").
    htrepoDBH = htrepo:DEFAULT-BUFFER-HANDLE.

    /* RECORRIDO BASICO SOBRE CLIENTES Y SUS CREDITOS */
    hClientes = BUFFER bdcentral.clientes:HANDLE.
    FOR EACH clientes FIELDS(nit) NO-LOCK:
        htrepoDBH:BUFFER-CREATE.
        hcmpo = fGetCmpoHndle(htrepoDBH,"fecha").
        hcmpo:BUFFER-VALUE = ffchaaaaammdd(TODAY).
        hcmpo = fGetCmpoHndle(htrepoDBH,"nit").
        hcmpo:BUFFER-VALUE = clientes.nit NO-ERROR.
        htrepoDBH:BUFFER-RELEASE().
        i = i + 1.
    END.
    
    fddtt(htrepodbh).    
    DELETE OBJECT htrepo.
END FUNCTION. /* FUNCTION fLlnaRpstrio */

/****************************************************************************************************/
/* fLlnaRpstrio(TODAY). */

