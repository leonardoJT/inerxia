/*
    Nombre: super001.p
    Descripcion: Superprocedimiento
    Log: Por cada función
*/

/*
D I R E C T O R I O  D E  F U N C I O N E S

*/
SESSION:TIME-SOURCE = "bdcentral".
DEF VAR chExcel AS COM-HANDLE NO-UNDO.
DEF VAR GAGNCIA AS INTEGER NO-UNDO.    
DEF VAR GUSUARIO AS CHAR NO-UNDO.    
DEF VAR GIAGENCIAACTUAL AS INTEGER NO-UNDO.
DEF VAR GSguimientoUOtrgmiento AS CHAR NO-UNDO. /* Toma Los Valores 0=Total o Seguimiento, 1=Otorgamiento O Solo Los Desembolsos Del Mes */
DEF TEMP-TABLE tbh NO-UNDO
    FIELD ctbla AS CHAR
    FIELD ccmpo AS CHAR
    FIELD htbla AS HANDLE 
    FIELD hcmpo AS HANDLE
    INDEX tblacmpo  ctbla ccmpo
    INDEX htbla ccmpo.
EMPTY TEMP-TABLE tbh.
SUBSCRIBE "DNEElHdeExel" ANYWHERE.
SUBSCRIBE "AgnciaSlccionda" ANYWHERE. 
SUBSCRIBE "UsuarioActual" ANYWHERE.
SUBSCRIBE "AgenciaActual" ANYWHERE.    
SUBSCRIBE "SeguimientoUOtorgamiento" ANYWHERE.
{incluido\StoreTableBuffers.i} /* crea los handles de las tablas fijas */
        
FUNCTION fPropath RETURN CHAR():
    /*
        Descripcion: Devuelve el PROPATH con separador chr(10)
        Creado: 20 de nov de 2007, Ing. Edilberto Mariño Moya
    */
    RETURN REPLACE(PROPATH,",",CHR(10)).
END FUNCTION. /* FUNCTION fPropath RETURN CHAR(): */

FUNCTION fCambiaLabelFilter RETURNS CHAR(h_dynfilter AS HANDLE):
    /*  
        Descripcion: CAMBIA LABELS DEL SMARTFILTER
        ADAPTADO: 6 NOV 2007, Ing. Edilberto Mariño Moya
    */
    DEFINE VARIABLE iNumObjects AS INTEGER NO-UNDO.
    DEFINE VARIABLE cObjects AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hObject AS HANDLE NO-UNDO.
    
    ASSIGN cObjects = DYNAMIC-FUNCTION('getEnabledObjHdls':U IN h_dynfilter).
    
    DO iNumObjects = 1 TO NUM-ENTRIES(cObjects):
        hObject = widget-handle(ENTRY(iNumObjects, cObjects)).
        
        IF hObject:TYPE = "BUTTON":U 
        THEN DO:
            IF hObject:LABEL = "&Apply Filter" THEN ASSIGN hObject:LABEL = "Aplicar".
            IF hObject:LABEL = "&Blank" THEN ASSIGN hObject:LABEL = "Blanco".
            IF hObject:LABEL = "&Reset" THEN ASSIGN hObject:LABEL = "Reinicia".    
        END.
    END.

  RETURN "".
    
END FUNCTION. /* FUNCTION fCambiaLabelFilter RETURNS CHAR(h_dynfilter AS HANDLE): */


FUNCTION fToExcelVlor RETURNS LOGICAL(pCol AS char,pval AS char):
    /*  
        Descripcion: Envia Valor A Excel
        creado: 29 oct 2007, Ing. Edilberto Mariño Moya
    */
    IF SUBSTRING(pval,1,1) = "="
    THEN chExcel:Range(pCol):FORMULA = trim(pVal) NO-ERROR.
    ELSE chExcel:Range(pCol):VALUE = pVal NO-ERROR.
    RETURN TRUE.    
END FUNCTION. /* FUNCTION fToExcelVlor RETURNS LOGICAL(pCol AS char,pval AS char): */

FUNCTION fColExcel RETURN CHAR(j AS INTEGER):
    /*  
        Descripcion: Devuelve en letras el equivalente a una columna de excel
        creado: 29 oct 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR k AS INTEGER NO-UNDO.
    DEF VAR cRt AS CHAR NO-UNDO.
    i = TRUNCATE(j / 26,0).
    k = j MOD 26.
    crt = CHR(i + 64) + CHR(1) + CHR(k + 64).
    IF ENTRY(2,crt,CHR(1)) = "@" 
    THEN DO:
        ENTRY(1,crt,CHR(1)) = CHR(ASC(ENTRY(1,crt,CHR(1))) - 1).
        ENTRY(2,crt,CHR(1)) = "Z".
    END.
    crt = REPLACE(crt,"@","").
    crt = TRIM(crt,CHR(1)).
    crt = REPLACE(crt,CHR(1),"").
    RETURN crt.
END FUNCTION. /* FUNCTION fColExcel RETURN CHAR(j AS INTEGER): */

FUNCTION fL1TOL2 RETURN CHAR(cVBscar AS CHAR,cOrgen AS CHAR,cDstno AS CHAR):
    /*
        DESCRIPCION SACA UNA PAREJA DE VALORES de cOrgen y los pasa a cDstno.
        El valor a buscar está en la posición par de la lista.
        Las dos listas cada una contiene un número par de valores
        separados por coma.
        El mensaje de retorno es:
        La primera entrada devuelta corresponde a cOrgen
        La segunda entrada devuelta corresponde a cDstno
        El separador de listas es chr(1).
        cOrgen y cDstno son listas separadas por comas.
        LOG:    26 oct 2007,    Ing. Edilberto Mariño Moya
        
    */
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR corgen1 AS CHAR NO-UNDO.
    cvbscar = TRIM(cvbscar).
    IF cvbscar = "" THEN RETURN corgen + CHR(1) + cdstno. /*  si el valor a buscar es blanco no hace nada */
    IF NOT ((NUM-ENTRIES(corgen,",") MOD 2) = 0 AND (NUM-ENTRIES(corgen,",") MOD 2) = 0) THEN RETURN corgen + CHR(1) + cdstno. /* alguno de los dos parametros de entrada vienen emparejados */
    DO i = 2 TO NUM-ENTRIES(corgen,",") BY 2:
        IF ENTRY(i,corgen,",") = cvbscar
        THEN DO:
            cdstno = cdstno + "," + ENTRY(i - 1,corgen,",") + "," + ENTRY(i,corgen,",").
            ENTRY(i - 1,corgen,",") = " ".
            ENTRY(i,corgen,",") = " ".
            LEAVE.
        END. /* IF ENTRY(i,corgen,",") = cvbscar */
    END. /* DO i = 2 TO NUM-ENTRIES(corgen,","): */
    DO i = 1 TO NUM-ENTRIES(corgen,","):
        IF NOT trim(ENTRY(i,corgen,",")) = ""
        THEN corgen1 = corgen1 + ENTRY(i,corgen,",") + ",".
    END. /* DO i = 1 TO NUM-ENTRIES(corgen,","): */
    corgen = corgen1.
    RETURN trim(corgen,",") + CHR(1) + trim(cdstno,",").
END FUNCTION. /* FUNCTION fL1TOL2 RETURN CHAR(cVBscar AS CHAR,cOrgen AS CHAR,cDstno AS CHAR): */

FUNCTION fFchaaaaammdd RETURN INTEGER(daFcha AS DATE):
    /*  Descripcion: Devuelve una fecha en formato aaaammdd
        creado: 23 oct 2007, Ing. Edilberto Mariño Moya
    */
    RETURN YEAR(daFcha) * 10000 + MONTH(dafcha) * 100 + DAY(dafcha).
END FUNCTION. /* FUNCTION fFchaaaaammdd */

FUNCTION fFchaaaaamm RETURN INTEGER(daFcha AS DATE):
    /*  Descripcion: Devuelve una fecha en formato aaaammdd
        creado: 24 oct 2007, Ing. Edilberto Mariño Moya
    */
    RETURN YEAR(daFcha) * 100 + MONTH(dafcha).
END FUNCTION. /* FUNCTION fFchaaaaamm */


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
            c[2] = c[2] + fNoVrDscncdo(IF h1:LABEL = ? OR h1:LABEL = "" THEN h1:NAME ELSE h1:LABEL) + CHR(2).
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
        c[2] = c[2] + fNoVrDscncdo(IF h1:LABEL = ? OR h1:LABEL = "" THEN h1:NAME ELSE h1:LABEL) + CHR(2).
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
            c[2] = c[2] + fNoVrDscncdo(IF h1:LABEL = ? OR h1:LABEL = "" THEN h1:NAME ELSE h1:LABEL) + CHR(2).
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
    FIND FIRST bdcentral._file NO-LOCK
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
        crtrno = IF bdcentral._field._label = ? OR bdcentral._field._label = "" THEN bdcentral._field._field-name ELSE bdcentral._field._label.
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
    IF can-find(FIRST tbh  
        WHERE
            tbh.htbla   = phtbla
        AND tbh.ccmpo   = Pccmpo)
    THEN DO:    
        FIND FIRST tbh NO-LOCK 
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
    hQHndle:QUERY-CLOSE.
    OUTPUT CLOSE.
END FUNCTION. /* FUNCTION fddtt */



FUNCTION fLlnaRpstrio RETURN CHAR(daFchaPrcso AS DATE):
    /*
        DESCRIPCION :   LLENA REPOSITORIO
        LOG         :   CREADO      , 19 OCT 2007   , ING. EDILBERTO MARIÑO MOYA
        log001      :   MODIFICADO  , 29 nov de 2007, Ing. Edilberto Mariño Moya
                        Se incluye el filtro para total o desembolsos del mes 
    */

    DEF VAR hRpstrio AS HANDLE NO-UNDO.
    DEF VAR hCRpstrio AS HANDLE NO-UNDO.
    DEF VAR cDtosCmpo AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR j AS INTEGER NO-UNDO.
    DEF VAR k AS INTEGER NO-UNDO.
    DEF VAR c AS CHAR NO-UNDO EXTENT 5.
    DEF VAR htrepo AS HANDLE NO-UNDO.
    DEF VAR htrepoDBH AS HANDLE NO-UNDO.
    DEF VAR hCmpo AS HANDLE NO-UNDO.
    DEF VAR hCmpo1 AS HANDLE NO-UNDO.
    DEF VAR hClientes AS HANDLE NO-UNDO.
    DEF VAR cCmpoOrgen AS CHAR NO-UNDO.
    DEF VAR htbla AS HANDLE NO-UNDO.
    DEF VAR cMnsje AS CHAR NO-UNDO.
    DEF VAR cPrgrma AS CHAR NO-UNDO.
    DEF VAR hQHndle AS HANDLE NO-UNDO.
    DEF VAR hRepo AS HANDLE NO-UNDO.
    DEF VAR daFchai AS DATE NO-UNDO.
    DEF VAR daFchaf AS DATE NO-UNDO.

    dafchai = DATE(MONTH(dafchaprcso),1,YEAR(dafchaprcso)).
    dafchaf = dafchaprcso.

    IF NOT can-find(FIRST conf_repositorio WHERE conf_repositorio.estado = 1)
    THEN DO:
        cmnsje =    "ERROR: Repositorio NO Definido." + CHR(10) + CHR(10) + 
                    "Proceso Interrumpido.".
        MESSAGE cmnsje VIEW-AS ALERT-BOX ERROR.
        RETURN cmnsje.
    END. /* IF NOT can-find(conf_repositorio WHERE conf_repositorio.estado = 1) */

    IF CAN-FIND(FIRST repositorio WHERE  repositorio.fecha = ffchaaaaamm(daFchaPrcso))
    THEN DO:
        cmnsje =    "ERROR: Repositorio para '" + string(ffchaaaaamm(daFchaPrcso)) + "' YA Existe." + CHR(10) + CHR(10) + 
                    "Proceso Interrumpido.".
        MESSAGE cmnsje VIEW-AS ALERT-BOX ERROR.
        RETURN cmnsje.
    END. /* IF CAN-FIND(FIRST repositorio WHERE  repositorio.fecha = ffchaaaaamm(daFchaPrcso)) */

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
    htrepo:ADD-LIKE-FIELD("tiponovedad" ,"repositorio.tiponovedad"). 

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
            IF NOT conf_repositorio.tablaorigen = "CALCULADO" THEN ENTRY(i,c[2],CHR(2)) ELSE conf_repositorio.DesCampoCal + "." + conf_repositorio.campodestino,
            IF NOT conf_repositorio.tablaorigen = "CALCULADO" THEN conf_repositorio.tablaorigen + "." + conf_repositorio.campoorigen ELSE "CALCULADO." + conf_repositorio.DesCampoCal + "." + conf_repositorio.ProcCampoCal).
    END. /*  FOR EACH conf_repositorio */
    htrepo:TEMP-TABLE-PREPARE("trepo").
    htrepoDBH = htrepo:DEFAULT-BUFFER-HANDLE.

    /* RECORRIDO BASICO SOBRE CLIENTES Y SUS CREDITOS */
    hClientes = BUFFER bdcentral.clientes:HANDLE.
    DO:
        FOR EACH creditos NO-LOCK
            WHERE
                NOT (creditos.estado > 2 AND creditos.estado < 5)
            AND (IF GAGNCIA = 0 THEN TRUE ELSE creditos.agencia = GAGNCIA)
            AND (IF GSguimientoUOtrgmiento = "0" THEN TRUE ELSE (creditos.fec_desembolso >= dafchai AND creditos.fec_desembolso <= dafchaf)),
            FIRST clientes NO-LOCK
                WHERE
                    clientes.nit = creditos.nit: 
            htrepoDBH:BUFFER-CREATE.
            DO j = 1 TO htrepodbh:NUM-FIELDS:
                hcmpo = htrepodbh:BUFFER-FIELD(j).     
                CASE hcmpo:NAME:
                    WHEN "fecha"            THEN hcmpo:BUFFER-VALUE = ffchaaaaamm(daFchaPrcso).
                    WHEN "nit"              THEN hcmpo:BUFFER-VALUE = clientes.nit NO-ERROR.
                    WHEN "ano"              THEN hcmpo:BUFFER-VALUE = YEAR(daFchaPrcso).
                    WHEN "mes"              THEN hcmpo:BUFFER-VALUE = MONTH(daFchaPrcso).
                    WHEN "dia"              THEN hcmpo:BUFFER-VALUE = DAY(daFchaPrcso).
                    WHEN "numerocredito"    THEN hcmpo:BUFFER-VALUE = creditos.num_credito.
                    WHEN "secuencialmes"    THEN hcmpo:BUFFER-VALUE = next-value(ConRepoCodigo).
                    WHEN "tiponovedad"      THEN hcmpo:BUFFER-VALUE = IF ffchaaaaamm(daFchaPrcso) = ffchaaaaamm(Creditos.Fec_Desembolso) THEN 1 ELSE 2.
                    OTHERWISE DO:
                        cCmpoOrgen = hcmpo:COLUMN-LABEL.
                        IF NOT CAN-DO("clientes,creditos",ENTRY(1,cCmpoOrgen,".")) 
                        AND NOT ENTRY(1,cCmpoOrgen,".") = "CALCULADO" /* SOLO APLICA PARA LAS TABLAS LISTADAS */
                        THEN DO:
                            MESSAGE "ERROR: Se hace Referencia A Un Buffer (" + cCmpoOrgen + ") Que NO Se Encuentra Disponible En El Actual Contexto" SKIP
                                "Su Valor Será Asignado Cómo Desconocido '?'"
                                VIEW-AS ALERT-BOX WARNING.
                        END.
                        ELSE DO:
                            IF NOT ENTRY(1,cCmpoOrgen,".") = "CALCULADO" 
                            THEN DO:
                                htbla = fGetBufferHandle(ENTRY(1,cCmpoOrgen,".")).
                                DO k = 1 TO htbla:NUM-FIELDS:
                                    hcmpo1 = htbla:BUFFER-FIELD(k).
                                    IF hcmpo1:NAME = ENTRY(2,cCmpoOrgen,".")
                                    THEN DO:
                                        hcmpo:BUFFER-VALUE = hcmpo1:buffer-VALUE.
                                    END.
                                END. /* DO k = 1 TO htbla:NUM-FIELDS */
                            END. /* IF NOT ENTRY(1,cCmpoOrgen,".") = "CALCULADO"  */
                            ELSE DO:
                                cPrgrma = ENTRY(3,cCmpoOrgen + "..",".").
                                IF SEARCH(cPrgrma + ".r") = ? AND SEARCH(cPrgrma + ".p") = ? AND SEARCH(cPrgrma + ".w") = ?
                                THEN DO:
                                    MESSAGE "El Programa Definido En '" cCmpoOrgen "' NO Ha Sido Encontrado."
                                        VIEW-AS ALERT-BOX ERROR.
                                END.
                                ELSE DO:
                                    cMnsje = "".
                                    RUN VALUE(ENTRY(3,cCmpoOrgen + "..",".") + "." + ENTRY(4,cCmpoOrgen + "..","."))
                                        (INPUT-OUTPUT cMnsje) NO-ERROR.
                                    IF ERROR-STATUS:ERROR
                                    THEN DO:
                                        MESSAGE "ERROR: Generado Por El Procedimiento '" cCmpoOrgen "'."
                                            VIEW-AS ALERT-BOX ERROR.
                                    END.
                                    ELSE DO:
                                        hcmpo:BUFFER-VALUE = cMnsje.
                                    END. /* IF ERROR-STATUS:ERROR */
                                END. /* IF SEARCH(cPrgrma + ".r") = ? AND SEARCH(cPrgrma + ".p") = ? AND SEARCH(cPrgrma + ".w") = ? */
                            END.
                        END. /*  IF NOT CAN-DO("clientes,creditos",ENTRY(1,cCmpoOrgen,".")) AND NOT ENTRY(1,cCmpoOrgen,".") = "CALCULADO" */
                    END. /* OTHERWISE DO: */
                END CASE. /* CASE hcmpo:NAME */
            END. /* DO j = 1 TO htrepodbh:NUM-FIELDS */
            htrepoDBH:BUFFER-RELEASE().
        END. /* FOR EACH creditos */
        /* las siguientes dos líneas son para prueba. se procesan solamente los primeros 100 clientes */
/*         i = i + 1.              */
/*         IF i >= 100 THEN LEAVE. */
    END. /* FOR EACH clientes */

    /*****************************/
    /* ESCRIBE EN EL REPOSITORIO */
    /*****************************/
    CREATE QUERY hQHndle.
    hQHndle:SET-BUFFERS(htrepoDBH).
    hQHndle:QUERY-PREPARE( "for each " + htrepoDBH:NAME + " no-lock").
    hQHndle:QUERY-OPEN.
    hRepo = fGetBufferHandle("repositorio").
    j = 0.
    repo:
    REPEAT TRANSACTION ON ERROR UNDO repo, LEAVE repo:
        REPEAT:
            hQHndle:GET-NEXT.
            IF hQHndle:QUERY-OFF-END THEN LEAVE.
            CREATE repositorio.
            j = j + 1.
            REPEAT i = 1 TO htrepoDBH:NUM-FIELDS:
                hcmpo = htrepoDBH:BUFFER-FIELD(i).
                IF NOT ENTRY(1,hcmpo:COLUMN-LABEL,".") = "CALCULADO" 
                THEN hcmpo1 = hrepo:BUFFER-FIELD(hcmpo:NAME).
                ELSE hcmpo1 = hrepo:BUFFER-FIELD(ENTRY(2,hcmpo:LABEL,".")).
                hcmpo1:BUFFER-VALUE = hcmpo:BUFFER-VALUE.
            END.
            RELEASE repositorio.
        END.
        /* ESCRIBE UN REGISTRO EN CONTROL_CARGA */
        CREATE CONTROL_CARGA.
        ASSIGN  control_carga.agencia = GIAGENCIAACTUAL
                control_carga.fechaCarga = TODAY
                control_carga.horaCarga = TIME
                control_carga.periodo = ffchaaaaamm(daFchaPrcso)
                control_carga.TotalRegistros = j
                control_carga.usuario = GUSUARIO.
        /* FIN : ESCRIBE UN REGISTRO EN CONTROL_CARGA */    
        LEAVE.
    END.
    /*********************************/
    /* FIN ESCRIBE EN EL REPOSITORIO */
    /*********************************/

    /* fddtt(htrepodbh).    */
    DELETE OBJECT htrepo.
END FUNCTION. /* FUNCTION fLlnaRpstrio */


FUNCTION o_fLlnaRpstrio RETURN CHAR(daFchaPrcso AS DATE):
    /*
        DESCRIPCION :   LLENA REPOSITORIO
        LOG         :   CREADO, 19 OCT 2007, ING. EDILBERTO MARIÑO MOYA
    */
    DEF VAR hRpstrio AS HANDLE NO-UNDO.
    DEF VAR hCRpstrio AS HANDLE NO-UNDO.
    DEF VAR cDtosCmpo AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR j AS INTEGER NO-UNDO.
    DEF VAR k AS INTEGER NO-UNDO.
    DEF VAR c AS CHAR NO-UNDO EXTENT 5.
    DEF VAR htrepo AS HANDLE NO-UNDO.
    DEF VAR htrepoDBH AS HANDLE NO-UNDO.
    DEF VAR hCmpo AS HANDLE NO-UNDO.
    DEF VAR hCmpo1 AS HANDLE NO-UNDO.
    DEF VAR hClientes AS HANDLE NO-UNDO.
    DEF VAR cCmpoOrgen AS CHAR NO-UNDO.
    DEF VAR htbla AS HANDLE NO-UNDO.
    DEF VAR cMnsje AS CHAR NO-UNDO.
    DEF VAR cPrgrma AS CHAR NO-UNDO.
    DEF VAR hQHndle AS HANDLE NO-UNDO.
    DEF VAR hRepo AS HANDLE NO-UNDO.
    IF NOT can-find(FIRST conf_repositorio WHERE conf_repositorio.estado = 1)
    THEN DO:
        cmnsje =    "ERROR: Repositorio NO Definido." + CHR(10) + CHR(10) + 
                    "Proceso Interrumpido.".
        MESSAGE cmnsje VIEW-AS ALERT-BOX ERROR.
        RETURN cmnsje.
    END. /* IF NOT can-find(conf_repositorio WHERE conf_repositorio.estado = 1) */

    IF CAN-FIND(FIRST repositorio WHERE  repositorio.fecha = ffchaaaaamm(daFchaPrcso))
    THEN DO:
        cmnsje =    "ERROR: Repositorio para '" + string(ffchaaaaamm(daFchaPrcso)) + "' YA Existe." + CHR(10) + CHR(10) + 
                    "Proceso Interrumpido.".
        MESSAGE cmnsje VIEW-AS ALERT-BOX ERROR.
        RETURN cmnsje.
    END. /* IF CAN-FIND(FIRST repositorio WHERE  repositorio.fecha = ffchaaaaamm(daFchaPrcso)) */

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
    htrepo:ADD-LIKE-FIELD("tiponovedad" ,"repositorio.tiponovedad"). 

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
            IF NOT conf_repositorio.tablaorigen = "CALCULADO" THEN ENTRY(i,c[2],CHR(2)) ELSE conf_repositorio.DesCampoCal + "." + conf_repositorio.campodestino,
            IF NOT conf_repositorio.tablaorigen = "CALCULADO" THEN conf_repositorio.tablaorigen + "." + conf_repositorio.campoorigen ELSE "CALCULADO." + conf_repositorio.DesCampoCal + "." + conf_repositorio.ProcCampoCal).
    END. /*  FOR EACH conf_repositorio */
    htrepo:TEMP-TABLE-PREPARE("trepo").
    htrepoDBH = htrepo:DEFAULT-BUFFER-HANDLE.

    /* RECORRIDO BASICO SOBRE CLIENTES Y SUS CREDITOS */
    hClientes = BUFFER bdcentral.clientes:HANDLE.
    FOR EACH clientes NO-LOCK
        WHERE
            CAN-FIND(FIRST creditos WHERE creditos.nit = clientes.nit
                        AND (IF GAGNCIA = 0 THEN TRUE ELSE creditos.agencia = GAGNCIA)):
        /* CURRENT-WINDOW:TITLE = string(i) + ") " + clientes.nit. */
        FOR EACH creditos NO-LOCK
            WHERE
                creditos.nit = clientes.nit
            AND NOT (creditos.estado > 2 AND creditos.estado < 5)
            AND (IF GAGNCIA = 0 THEN TRUE ELSE creditos.agencia = GAGNCIA): 
            htrepoDBH:BUFFER-CREATE.
            DO j = 1 TO htrepodbh:NUM-FIELDS:
                hcmpo = htrepodbh:BUFFER-FIELD(j).     
                CASE hcmpo:NAME:
                    WHEN "fecha"            THEN hcmpo:BUFFER-VALUE = ffchaaaaamm(daFchaPrcso).
                    WHEN "nit"              THEN hcmpo:BUFFER-VALUE = clientes.nit NO-ERROR.
                    WHEN "ano"              THEN hcmpo:BUFFER-VALUE = YEAR(daFchaPrcso).
                    WHEN "mes"              THEN hcmpo:BUFFER-VALUE = MONTH(daFchaPrcso).
                    WHEN "dia"              THEN hcmpo:BUFFER-VALUE = DAY(daFchaPrcso).
                    WHEN "numerocredito"    THEN hcmpo:BUFFER-VALUE = creditos.num_credito.
                    WHEN "secuencialmes"    THEN hcmpo:BUFFER-VALUE = next-value(ConRepoCodigo).
                    WHEN "tiponovedad"      THEN hcmpo:BUFFER-VALUE = IF ffchaaaaamm(daFchaPrcso) = ffchaaaaamm(Creditos.Fec_Desembolso) THEN 1 ELSE 2.
                    OTHERWISE DO:
                        cCmpoOrgen = hcmpo:COLUMN-LABEL.
                        IF NOT CAN-DO("clientes,creditos",ENTRY(1,cCmpoOrgen,".")) 
                        AND NOT ENTRY(1,cCmpoOrgen,".") = "CALCULADO" /* SOLO APLICA PARA LAS TABLAS LISTADAS */
                        THEN DO:
                            MESSAGE "ERROR: Se hace Referencia A Un Buffer (" + cCmpoOrgen + ") Que NO Se Encuentra Disponible En El Actual Contexto" SKIP
                                "Su Valor Será Asignado Cómo Desconocido '?'"
                                VIEW-AS ALERT-BOX WARNING.
                        END.
                        ELSE DO:
                            IF NOT ENTRY(1,cCmpoOrgen,".") = "CALCULADO" 
                            THEN DO:
                                htbla = fGetBufferHandle(ENTRY(1,cCmpoOrgen,".")).
                                DO k = 1 TO htbla:NUM-FIELDS:
                                    hcmpo1 = htbla:BUFFER-FIELD(k).
                                    IF hcmpo1:NAME = ENTRY(2,cCmpoOrgen,".")
                                    THEN DO:
                                        hcmpo:BUFFER-VALUE = hcmpo1:buffer-VALUE.
                                    END.
                                END. /* DO k = 1 TO htbla:NUM-FIELDS */
                            END. /* IF NOT ENTRY(1,cCmpoOrgen,".") = "CALCULADO"  */
                            ELSE DO:
                                cPrgrma = ENTRY(3,cCmpoOrgen + "..",".").
                                IF SEARCH(cPrgrma + ".r") = ? AND SEARCH(cPrgrma + ".p") = ? AND SEARCH(cPrgrma + ".w") = ?
                                THEN DO:
                                    MESSAGE "El Programa Definido En '" cCmpoOrgen "' NO Ha Sido Encontrado."
                                        VIEW-AS ALERT-BOX ERROR.
                                END.
                                ELSE DO:
                                    cMnsje = "".
                                    RUN VALUE(ENTRY(3,cCmpoOrgen + "..",".") + "." + ENTRY(4,cCmpoOrgen + "..","."))
                                        (INPUT-OUTPUT cMnsje) NO-ERROR.
                                    IF ERROR-STATUS:ERROR
                                    THEN DO:
                                        MESSAGE "ERROR: Generado Por El Procedimiento '" cCmpoOrgen "'."
                                            VIEW-AS ALERT-BOX ERROR.
                                    END.
                                    ELSE DO:
                                        hcmpo:BUFFER-VALUE = cMnsje.
                                    END. /* IF ERROR-STATUS:ERROR */
                                END. /* IF SEARCH(cPrgrma + ".r") = ? AND SEARCH(cPrgrma + ".p") = ? AND SEARCH(cPrgrma + ".w") = ? */
                            END.
                        END. /*  IF NOT CAN-DO("clientes,creditos",ENTRY(1,cCmpoOrgen,".")) AND NOT ENTRY(1,cCmpoOrgen,".") = "CALCULADO" */
                    END. /* OTHERWISE DO: */
                END CASE. /* CASE hcmpo:NAME */
            END. /* DO j = 1 TO htrepodbh:NUM-FIELDS */
            htrepoDBH:BUFFER-RELEASE().
        END. /* FOR EACH creditos */
        /* las siguientes dos líneas son para prueba. se procesan solamente los primeros 100 clientes */
/*         i = i + 1.              */
/*         IF i >= 100 THEN LEAVE. */
    END. /* FOR EACH clientes */

    /*****************************/
    /* ESCRIBE EN EL REPOSITORIO */
    /*****************************/
    CREATE QUERY hQHndle.
    hQHndle:SET-BUFFERS(htrepoDBH).
    hQHndle:QUERY-PREPARE( "for each " + htrepoDBH:NAME + " no-lock").
    hQHndle:QUERY-OPEN.
    hRepo = fGetBufferHandle("repositorio").
    repo:
    REPEAT TRANSACTION ON ERROR UNDO repo, LEAVE repo:
        REPEAT:
            hQHndle:GET-NEXT.
            IF hQHndle:QUERY-OFF-END THEN LEAVE.
            CREATE repositorio.
            REPEAT i = 1 TO htrepoDBH:NUM-FIELDS:
                hcmpo = htrepoDBH:BUFFER-FIELD(i).
                IF NOT ENTRY(1,hcmpo:COLUMN-LABEL,".") = "CALCULADO" 
                THEN hcmpo1 = hrepo:BUFFER-FIELD(hcmpo:NAME).
                ELSE hcmpo1 = hrepo:BUFFER-FIELD(ENTRY(2,hcmpo:LABEL,".")).
                hcmpo1:BUFFER-VALUE = hcmpo:BUFFER-VALUE.
            END.
            RELEASE repositorio.
        END.
        LEAVE.
    END.
    /*********************************/
    /* FIN ESCRIBE EN EL REPOSITORIO */
    /*********************************/

    /* fddtt(htrepodbh).    */
    DELETE OBJECT htrepo.
END FUNCTION. /* FUNCTION fLlnaRpstrio */
    
FUNCTION fPuedeNavegar RETURNS LOGICAL(h_vconf_repositoriowi AS HANDLE):
    /*  
        Descripcion: retorna si se puede navegar o no.
            no si está actualizando o adicionando sobre un viewer
        creado: 7 NOV 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR l AS LOGICAL NO-UNDO.
    l = NOT DYNAMIC-FUNCTION('canNavigate':U IN h_vconf_repositoriowi) = "no".
    IF NOT l
    THEN DO:
        MESSAGE "Usted Se Encuentra En Proceso De Adición O Inserción" SKIP(2)
                "TERMINE O CANCELE LA OPERACION"
                VIEW-AS ALERT-BOX WARNING TITLE "MENSAJE".
    END.
    RETURN l.
END FUNCTION. /* FUNCTION fPuedeNavegar RETURNS LOGICAL(): */
    
    
    
    
/****************************************************************************************************/
/* fLlnaRpstrio(TODAY). */

    
    
PROCEDURE DNEElHdeExel:
    DEF INPUT PARAMETER chExcelApplication AS COM-HANDLE NO-UNDO.
    chExcel = chExcelApplication.         
END PROCEDURE. /* fin ----- ElHdeExel */
    
PROCEDURE AgnciaSlccionda:
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    GAGNCIA = INTEGER(ENTRY(1,c,CHR(1))).        
END PROCEDURE.

PROCEDURE UsuarioActual:
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.        
    GUSUARIO = ENTRY(1,c,CHR(1)) NO-ERROR.        
END PROCEDURE.
PROCEDURE AgenciaActual:
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    GIAGENCIAACTUAL = integer(ENTRY(1,c,CHR(1))) NO-ERROR.        
END PROCEDURE.
PROCEDURE SeguimientoUOtorgamiento:
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    GSguimientoUOtrgmiento = c.
END PROCEDURE.
