/*
    Nombre: SuperSuper001.p
    Descripcion: Superprocedimiento Para Los Formatos De La SUPER
    Log: Por cada función
    Creado por: Ing. Edilberto Mariño Moya
*/
DEFINE STREAM stRegTpo1.
DEFINE STREAM stRegTpo2.
DEFINE STREAM stRegTpo3.
DEFINE STREAM stRegTpo4.
DEFINE STREAM stRegTpo5.
DEFINE STREAM stRegTpo6.

SESSION:TIME-SOURCE = "bdcentral".
DEF VAR GFORMATO AS CHAR NO-UNDO.
DEF VAR GNmbreArchvoSlda AS CHAR NO-UNDO INITIAL "c:\tmp\ArchivoDeSalida.txt".
DEF VAR chExcel AS COM-HANDLE NO-UNDO.
DEF VAR GAGNCIA AS INTEGER NO-UNDO.
DEF VAR GUSUARIO AS CHAR NO-UNDO.
DEF VAR GIAGENCIAACTUAL AS INTEGER NO-UNDO.
DEF VAR GiNmroRgstro AS INTEGER NO-UNDO.
DEF VAR GDAFECHACORTE AS DATE NO-UNDO INITIAL TODAY.
DEF VAR GdaFchaI AS DATE NO-UNDO.
GdaFchaI = DATE(MONTH(GDAFECHACORTE),1,YEAR(GDAFECHACORTE)).
DEF VAR GdaFchaf AS DATE NO-UNDO.
GdaFchaf = GDAFECHACORTE.
DEF VAR cl AS CHAR NO-UNDO.
DEF VAR iFILA AS INTEGER NO-UNDO.
DEF VAR iColumna AS INTEGER NO-UNDO.
DEF VAR ch AS COM-HANDLE NO-UNDO.
DEF VAR hQHndle AS HANDLE NO-UNDO.
DEF VAR cTbla AS CHAR NO-UNDO.
DEF VAR hTbla AS HANDLE NO-UNDO.
DEF VAR i AS INTEGER NO-UNDO.
DEF VAR deVlor AS DECIMAL NO-UNDO.

/* ARCHIVO TEMPORAL DE AHORROS PARA FORMATO 338 */
DEF TEMP-TABLE t338Ahorros NO-UNDO
    FIELD UnidadDeCaptura AS INTEGER
    FIELD SubCuenta AS INTEGER
    FIELD Columna AS INTEGER
    FIELD iFila AS INTEGER /* excel */
    FIELD iColumna AS INTEGER /* excel */
    FIELD SaldoDisponible AS DECIMAL
    FIELD TipoDeAhorro AS INTEGER
    FIELD Producto AS INTEGER
    FIELD nit AS CHAR
    INDEX pk UnidadDeCaptura SubCuenta Columna producto
    INDEX sk iFila iColumna.
    

/* ARCHIVO TEMPORAL DE CREDITOS PARA EL FORMATO 338 */
DEF TEMP-TABLE t338Creditos NO-UNDO
    FIELD UnidadDeCaptura AS INTEGER
    FIELD SubCuenta AS INTEGER
    FIELD Columna AS INTEGER
    FIELD iFila AS INTEGER /* excel */
    FIELD iColumna AS INTEGER /* excel */
    FIELD SaldoCapital AS DECIMAL
    FIELD InteresCorriente AS DECIMAL
    FIELD InteresAnticipado AS DECIMAL
    FIELD InteresMoraCobrar AS DECIMAL
    FIELD InteresDificilCobro AS DECIMAL
    FIELD nit AS CHAR
    FIELD TipoDeCredito AS INTEGER
    FIELD monto AS DECIMAL
    FIELD agencia AS INTEGER
    INDEX pk UnidadDeCaptura SubCuenta Columna
    INDEX sk iFila iColumna.

DEF TEMP-TABLE hTblas NO-UNDO /* CONTIENE LOS HANDLES A LOS BUFFERS CREADOS DINAMICAMENTE SEGUN LA DEFINICIÓN DE PARAMETROS */
    FIELD htbla AS HANDLE
    FIELD htNmbre AS CHAR.

/* 
    NOTA
    12 de may de 2008    
    Esquema Para Nombrar Las Funciones.
    f: Función
    317: Formato
    1: Unidad De Captura
    15: Subcuenta
    1: COlumna
*/    
    
FUNCTION f317-1-15-1 RETURNS DECIMAL(iTpoCrdto AS INTEGER):
    /*
        Descripcion: Promedio Ponderado sobre saldos de la tasa efectiva de credito
        Creado: 12 may 2008, Ing. Edilberto Mariño Moya
    */
    DEF VAR deTsaSldo AS DECIMAL NO-UNDO.
    DEF VAR deSldo AS DECIMAL NO-UNDO.
    FOR EACH creditos NO-LOCK
        WHERE
            creditos.sdo_capital > 0
        AND creditos.tip_credito = iTpoCrdto:
        deTsaSldo = deTsaSldo + creditos.tasa * creditos.sdo_capital.
        deSldo = desldo + creditos.sdo_capital.
    END.
    RETURN IF deSldo = 0 THEN 0 ELSE deTsaSldo / deSldo.
END FUNCTION. /* FUNCTION f317-1-15-1 RETURNS DECIMAL(iTpoCrdto AS INTEGER): */

FUNCTION f317-1-15-2 RETURNS DECIMAL(iTpoCrdto AS INTEGER):
    /*
        Descripcion: saldos creditos
        Creado: 15 may 2008, Ing. Edilberto Mariño Moya
    */
    DEF VAR deSldo AS DECIMAL NO-UNDO.
    FOR EACH creditos NO-LOCK
        WHERE
            creditos.sdo_capital > 0
        AND creditos.tip_credito = iTpoCrdto:
        deSldo = desldo + creditos.sdo_capital.
    END.
    RETURN deSldo.
END FUNCTION. /* FUNCTION f317-1-15-2 RETURNS DECIMAL(iTpoCrdto AS INTEGER): */
 
FUNCTION f317-1-20-1 RETURNS DECIMAL(iTpoCrdto AS INTEGER):
    /*
        Descripcion: Promedio aritmético de la tasa efectiva de crédito
        Creado: 12 may 2008, Ing. Edilberto Mariño Moya
    */
    DEF VAR deTsa AS DECIMAL NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    FOR EACH creditos NO-LOCK
        WHERE
            creditos.sdo_capital > 0
        AND creditos.tip_credito = iTpoCrdto:
        deTsa = deTsa + creditos.tasa.
        i = i + 1.
    END.
    RETURN IF i = 0 THEN 0 ELSE deTsa / i.
END FUNCTION. /* FUNCTION f317-1-20-1 RETURNS DECIMAL(iTpoCrdto AS INTEGER): */



FUNCTION fTRM RETURNS DECIMAL(daFcha AS DATE):
    /*
        Descripcion: Devuelve La TRM a la fecha especifica
        Creado: 02 may 2008, Ing. Edilberto Mariño Moya
    */
    DEF VAR deRtrno AS DECIMAL NO-UNDO.
    FIND LAST indicadores NO-LOCK
        WHERE
            indicadores.fecha <= daFcha NO-ERROR.
    RETURN IF AVAILABLE indicadores THEN indicadores.valor ELSE 0.
END FUNCTION. /* FUNCTION fTRM RETURNS DECIMAL(daFcha AS DATE): */

FUNCTION fSldo RETURNS CHAR(cCuenta AS CHAR,cNit AS CHAR,imes AS INTEGER, iano AS INTEGER):
    /*
        Descripcion: Cálcula Saldos En anexos, devuelve so,db,cr,sf separados por chr(1)
        Creado:27 feb 2008, Ing. Edilberto Mariño Moya
    */
    DEF BUFFER banexos FOR anexos.
    DEF VAR deDbtos AS DECIMAL NO-UNDO.
    DEF VAR deCrdtos AS DECIMAL NO-UNDO.
    DEF VAR deSldoFnal AS DECIMAL NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR cr AS CHAR NO-UNDO.

    FOR EACH banexos NO-LOCK
        WHERE
            (IF cnit = "" THEN TRUE ELSE banexos.nit = cnit)
        AND (IF ccuenta = "" THEN TRUE ELSE banexos.cuenta = ccuenta)
        AND banexos.ano = iano
        BREAK
            BY banexos.nit:
        ACCUMULATE banexos.sdo_inicial (TOTAL BY banexos.nit).
        deDbtos = 0.
        deCrdtos = 0.
        DO i = 1 TO imes:
            deDbtos     = deDbtos + banexos.db[i].
            deCrdtos    = deCrdtos + banexos.cr[i].
        END.
        deSldoFnal = banexos.sdo_inicial + deDbtos - deCrdtos.
        ACCUMULATE deDbtos (TOTAL BY banexos.nit).    
        ACCUMULATE deCrdtos (TOTAL BY banexos.nit).    
        ACCUMULATE deSldoFnal (TOTAL BY banexos.nit).    
    END.
    cr = FILL(CHR(1),3).
    ENTRY(1,cr,CHR(1)) = string(ACCUM TOTAL banexos.sdo_inicial). /* SALDOS INICIAL */ 
    ENTRY(2,cr,CHR(1)) = string(ACCUM TOTAL deDbtos). /* DEBITOS */
    ENTRY(3,cr,CHR(1)) = string(ACCUM TOTAL deCrdtos). /* CREDITOS */
    ENTRY(4,cr,CHR(1)) = string(ACCUM TOTAL desldoFnal). /* SALDO FINAL */
    RETURN cr.
END FUNCTION.

FUNCTION f1000 RETURN DECIMAL(deVlor AS DECIMAL):
    /*  
        Descripcion: Redondea al multiplo de mil más cercano
        creado: 29 oct 2007, Ing. Edilberto Mariño Moya
    */
    RETURN ROUND(deVlor / 1000,0) * 1000.
END FUNCTION. /* FUNCTION f1000 RETURN DECIMAL(deVlor AS DECIMAL): */

FUNCTION fEn1000s RETURN DECIMAL(deVlor AS DECIMAL):
    /*  
        Descripcion: Redondea al multiplo de mil más cercano
        creado: 29 oct 2007, Ing. Edilberto Mariño Moya
    */
    RETURN ROUND(deVlor / 1000,0).
END FUNCTION. /* FUNCTION f1000 RETURN DECIMAL(deVlor AS DECIMAL): */

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
FUNCTION fCoor RETURN CHAR(i AS INTEGER, j AS INTEGER):
    RETURN fColExcel(j) + STRING(i).
END FUNCTION. /* FUNCTION fCoor RETURN CHAR(i AS INTEGER, j AS INTEGER): */
FUNCTION fPropath RETURN CHAR():
    /*
        Descripcion: Devuelve el PROPATH con separador chr(10)
        Creado: 20 de nov de 2007, Ing. Edilberto Mariño Moya
    */
    RETURN REPLACE(PROPATH,",",CHR(10)).
END FUNCTION. /* FUNCTION fPropath RETURN CHAR(): */
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
FUNCTION fFchaddmmaaaa RETURN CHAR(daFcha AS DATE):
    /*  Descripcion: Devuelve una fecha en formato aaaammdd
        creado: 23 oct 2007, Ing. Edilberto Mariño Moya
    */
    RETURN  string(DAY(dafcha),"99") + STRING(MONTH(dafcha),"99") + string(YEAR(daFcha),"9999").
END FUNCTION. /* FUNCTION fFchaaaaammdd */

FUNCTION fFchaaaaamm RETURN INTEGER(daFcha AS DATE):
    /*  Descripcion: Devuelve una fecha en formato aaaammdd
        creado: 24 oct 2007, Ing. Edilberto Mariño Moya
    */
    RETURN YEAR(daFcha) * 100 + MONTH(dafcha).
END FUNCTION. /* FUNCTION fFchaaaaamm */

FUNCTION fNoVrDscncdo RETURN CHAR(cv AS CHAR):
    /*  Descripcion: valida si el valor es ? y devuelve "" de lo contrario devuelve el valor
        Creado: 12 oct 2007, Ing. Edilberto Mariño Moya */
    RETURN IF cv = ? THEN "" ELSE replace(cv,CHR(10)," ").
END FUNCTION. /* FUNCTION fNoVrDscncdo */

FUNCTION fRgstroTpoUnoSTD3 RETURN CHAR(iAreaInfrmcion AS INTEGER,iTpoInfrmcion AS INTEGER):
    /*
        Descipcion: Formato Super 338
        Creado:27 feb 2008, Ing. Edilberto Mariño Moya
        NOTAS: Según Documento sbds003.doc
               Circular Formato 338_20080213
    */
    DEF VAR cl AS CHAR NO-UNDO.
    GiNmroRgstro = IF GiNmroRgstro > 1 THEN GiNmroRgstro ELSE GiNmroRgstro + 1.
     
    overlay(cl,1,5) = STRING(1,"99999"). /* NUMERO DEL REGISTRO */
    overlay(cl,6,1) = "1". /* CONSTANTE */
    overlay(cl,7,2) = "01". /* TIPO DE ENTIDAD: LO ASIGNA LA SUPERBANCARIA */
    overlay(cl,9,6) = "000001". /* CODIGO DE LA ENTIDAD: LO ASIGNA LA SUPERBANCARIA */
    overlay(cl,15,8) = fFchaddmmaaaa(GDAFECHACORTE). /* FECHA DE CORTE */
    overlay(cl,23,5) = STRING(GiNmroRgstro,"99999"). /* TOTAL DE REGISTROS */
    overlay(cl,28,11) = "PALABRACLAVE". /* PALABRA CLAVE QUE ASIGNA LA SUPER */
    overlay(cl,39,2) = string(1,"99"). /* AREA DE INFORMACION */
    overlay(cl,41,2) = string(0,"99"). /* TIPO DE INFORME */ 
    PUT UNFORMATTED cl SKIP.

/*     iFILA = 1.                                                                      */
/*     iColumna = 1.                                                                   */
/*     ftoexcelvlor(fcoor(iFILA,iColumna),"SUBCUENTA").                                */
/*     chexcel:range(fcolexcel(iColumna) + STRING(ifila)):HorizontalAlignment = -4108. */
/*     iColumna = iColumna + 1.                                                        */
/*     ftoexcelvlor(fcoor(iFILA,iColumna),"SECTORES Y ENTIDADES").                     */
/*     chexcel:range(fcolexcel(iColumna) + STRING(ifila)):HorizontalAlignment = -4108. */
/*     iColumna = iColumna + 1.                                                        */
    RETURN cl.
END FUNCTION. /* FUNCTION fRgstroTpoUnoSTD3 RETURN CHAR(): */

FUNCTION fRgstroTpoUnoSTD7 RETURN CHAR(iAreaInfrmcion AS INTEGER,iTpoInfrmcion AS INTEGER):
    /*
        Descipcion: REGISTRO TIPO UNO PARA EL ESTANDAR 7
        Creado: 19 MAY 2008, Ing. Edilberto Mariño Moya
    */
    DEF VAR cl AS CHAR NO-UNDO.
    GiNmroRgstro = GiNmroRgstro + 1.
    overlay(cl,1,8) = STRING(1,"99999999"). /* NUMERO DEL REGISTRO */
    overlay(cl,9,1) = "1". /* CONSTANTE */
    overlay(cl,10,2) = "02". /* TIPO DE ENTIDAD: LO ASIGNA LA SUPERBANCARIA */
    overlay(cl,12,6) = "000001". /* CODIGO DE LA ENTIDAD: LO ASIGNA LA SUPERBANCARIA */
    overlay(cl,18,8) = fFchaddmmaaaa(GDAFECHACORTE). /* FECHA DE CORTE */
    overlay(cl,26,8) = STRING(GiNmroRgstro,"99999"). /* TOTAL DE REGISTROS */
    overlay(cl,34,11) = "PALABRACLAVE". /* PALABRA CLAVE QUE ASIGNA LA SUPER */
    overlay(cl,45,2) = string(1,"99"). /* AREA DE INFORMACION */
    overlay(cl,47,2) = string(0,"99"). /* TIPO DE INFORME */ 
    PUT UNFORMATTED cl SKIP.

    RETURN cl.
END FUNCTION. /* FUNCTION fRgstroTpoUnoSTD7 RETURN CHAR(): */
    
FUNCTION fRgstroTpoDosSTD3 RETURN CHAR():
    /*
        Descipcion: Formato Super 338
        Creado:27 feb 2008, Ing. Edilberto Mariño Moya
        NOTAS: Según Documento sbds003.doc
    */
    DEF VAR cl AS CHAR NO-UNDO.
    GiNmroRgstro = GiNmroRgstro + 1.

    overlay(cl,1,5) = STRING(2,"99999"). /* NUMERO DEL REGISTRO */
    overlay(cl,6,1) = "2". /* CONSTANTE */
    overlay(cl,7,4) = "0000". /* DIRECCION GENERAL o CODIGO DE LA OFICINA */
    overlay(cl,11,1) = "0". /* CONSTANTE */
    overlay(cl,12,1) = "0". /* TIPO DE MONEDA */ 
    overlay(cl,13,1) = "1". /* INDICADOR DE TIPO DE INFORMACION ?????????????????????'*/
    overlay(cl,14,1) = "0". /* TIPO DE FIDEICOMISO O FONDO ?????????????????????'*/
    overlay(cl,15,4) = "0000". /* CODIGO DEL FIDEICOMISO O FONDO ?????????????????????'*/
    PUT UNFORMATTED cl SKIP.
    RETURN cl.
END FUNCTION. /* FUNCTION fRgstroTpoDosSTD3 RETURN CHAR() */

    
FUNCTION fRgstroTpoDosSTD7 RETURN CHAR(c AS CHAR):
    /*
        Descipcion: REGISTRO TIPO DOS PARA EL ESTANDAR 7
        Creado: 19 MAY 2008, Ing. Edilberto Mariño Moya
        Estructura Del Mensaje De Entrada:
            Entrada uno: NIT, SIN SEPARADORES, NI DIGITO DE VERIFICACION
            ENTRADA DOS: APELLIDOS + NOMBRES DEL CLIENTE            
    */
    DEF VAR cl AS CHAR NO-UNDO.
    DEF VAR cNit AS CHAR NO-UNDO.
    DEF VAR cNmbre AS CHAR NO-UNDO.
        
    GiNmroRgstro = GiNmroRgstro + 1.
    cnit = ENTRY(1,c,",").
    cnit = REPLACE(cnit,".","").
    cnit = REPLACE(cnit,",","").
    cnmbre = caps(ENTRY(2,c,",")).    
        
    overlay(cl,1,8) = STRING(2,"99999"). /* NUMERO DEL REGISTRO */
    overlay(cl,9,1) = "2". /* Tipo de registro ("2" en este caso) */
    overlay(cl,10,1) = "0000". /* Tipo de identificación: 0 = No aplica, 1 = Cédula de ciudadanía, 2 = Cédula de extranjería, 3 = NIT, 4 = Tarjeta de identidad, 5 = Pasaporte, 6 = Carné diplomático, 7 = Sociedad extranjera sin NIT en Colombia,  8 = Fideicomiso, 9 = Registro civil de nacimiento */
    overlay(cl,11,12) = string(DECIMAL(cnit),"999999999999") NO-ERROR. /* Número de Identificación */
    overlay(cl,23,1) = "N". /* Dígito de chequeo asignado únicamente por la Administración de Impuestos Nacionales (N = No aplica) */ 
    overlay(cl,24,50) = cNmbre. /* Nombre o razón social*/
    PUT UNFORMATTED cl SKIP.
    RETURN cl.
END FUNCTION. /* FUNCTION fRgstroTpoDosSTD7 RETURN CHAR(): */
    
FUNCTION fRgstroTpoTresSTD3 RETURN CHAR(): /* OTROS INTERESES columnas 19 y 20 */
    /*
        Descipcion: Formato Super 338
        Creado:27 feb 2008, Ing. Edilberto Mariño Moya
        NOTAS: Según Documento sbds003.doc
        ESTE TIPO DE REGISTRO NO APLICA PARA JURISCOOP PERO DEBE IR CON CEROS
    */
    DEF VAR cl AS CHAR NO-UNDO.
    DEF VAR cSldo AS CHAR NO-UNDO.
    GiNmroRgstro = GiNmroRgstro + 1.
    
    overlay(cl,1,5) = STRING(GiNmroRgstro,"99999"). /* NUMERO DEL REGISTRO */
    overlay(cl,6,1) = "3". /* CONSTANTE */
    overlay(cl,7,27) = FILL("0",27). /* CONSTANTE */
    PUT UNFORMATTE cl SKIP.

    RETURN "".
END FUNCTION. /* FUNCTION fRgstroTpoDosSTD3 RETURN CHAR() */

FUNCTION fRgstroTpoTresSTD7 RETURN CHAR(): 
    /*
        Descipcion: REGISTRO TIPO TRES PARA EL ESTANDAR 7
        Creado:19 MAY 2008, Ing. Edilberto Mariño Moya
        NOTAS: Según Documento sbds003.doc
        ESTE TIPO DE REGISTRO NO APLICA PARA JURISCOOP PERO DEBE IR CON CEROS
    */
    DEF VAR cl AS CHAR NO-UNDO.
    DEF VAR cSldo AS CHAR NO-UNDO.
    GiNmroRgstro = GiNmroRgstro + 1.
    
    overlay(cl,1,8) = STRING(GiNmroRgstro,"99999999"). /* NUMERO DEL REGISTRO */
    overlay(cl,9,1) = "3". /* Tipo de registro ("3" en este caso) */
    overlay(cl,10,1) = "0". /* Tipo de evaluación, 0 = De la entidad,  1 = Del fideicomiso */
    overlay(cl,11,2) = "00". /* Tipo de fideicomiso 0 = No es fideicomiso 1 = Fideicomiso de inversión  2 = Fideicomiso inmobiliario  3 = Fideicomiso de administración  4 = Otros fideicomisos   5 = Fondo de cesantía  6 = Fondos de Pensiones  7 = Prima media ISS  8 = Prima media CAXDAC */
    overlay(cl,13,6) = "000000". /* Código del fideicomiso */
    overlay(cl,19,2) = "00". /* Tipo de entidad vigilada originaria o fideicomitente */
    overlay(cl,21,6) = "000000". /* Codigo de entidad vigilada originaria o fideicomitente */
    overlay(cl,27,17) = fill("0",17). /* Valor del fideicomiso */
        
    PUT UNFORMATTE cl SKIP.

    RETURN cl.
END FUNCTION. /* FUNCTION fRgstroTpoDosSTD7 RETURN CHAR() */
    


FUNCTION fRgstroTpoCuatroSTD7 RETURN CHAR():
    /*
        Descipcion: REGISTRO TIPO CUATRO PARA EL ESTANDAR 7
        Creado: 20 MAY 2008, Ing. Edilberto Mariño Moya
                
    */
    DEF VAR cl AS CHAR NO-UNDO.
    DEF VAR cSldo AS CHAR NO-UNDO.
    DEF VAR deVlor AS DECIMAL NO-UNDO.

    FOR EACH t338Creditos
        BREAK
            BY t338Creditos.UnidadDeCaptura
            BY t338Creditos.SubCuenta
            BY t338Creditos.Columna:
        ACCUMULATE  t338Creditos.SaldoCapital (TOTAL BY t338Creditos.Columna BY t338Creditos.SubCuenta BY t338Creditos.UnidadDeCaptura).
        IF FIRST-OF(t338Creditos.Columna)
        THEN DO:
/*             iFILA = iFILA + 1.                                           */
/*             ftoexcelvlor(fcoor(iFILA,1),string(t338Creditos.SubCuenta)). */
        END.
        IF LAST-OF(t338Creditos.Columna)
        THEN DO:
            GiNmroRgstro = GiNmroRgstro + 1.
            overlay(cl,1,5) = STRING(GiNmroRgstro,"99999"). /* NUMERO DEL REGISTRO */
            overlay(cl,6,1) = "4". /* CONSTANTE */
            overlay(cl,7,3) = "338". /* NUMERO DE FORMATO */
            overlay(cl,10,2) = STRING(t338Creditos.Columna,"99").
            overlay(cl,12,2) = string(t338Creditos.UnidadDeCaptura,"99").
            overlay(cl,14,3) = STRING(t338Creditos.SubCuenta,"999").
            deVlor = ACCUM TOTAL BY t338Creditos.Columna t338Creditos.SaldoCapital.
            overlay(cl,17,1) = IF devlor >= 0 THEN "+" ELSE "-". /* signo */
            overlay(cl,18,20) = replace(string(abs(devlor),"99999999999999999.99"),",",".").
            PUT UNFORMATTE cl SKIP.
        END.
    END.
    RETURN cl.
END FUNCTION. /* FUNCTION fRgstroTpoCuatroSTD7 RETURN CHAR(): */


FUNCTION fPraFrmtoSper338Crdtos RETURNS LOGICAL():
    /*
        Descipcion: Crea La Tabla Temporal t338 que contiene los datos resumen de creditos para ser usados en los querys
        Creado:03 Mar 2008, Ing. Edilberto Mariño Moya
    */
    EMPTY TEMP-TABLE  t338Creditos.
    DEF VAR cl AS CHAR NO-UNDO.
    FOR EACH clientes NO-LOCK
        WHERE
            CAN-FIND(
                FIRST creditos 
                    WHERE 
                        creditos.nit = clientes.nit 
                    AND creditos.fec_desembolso >= GdaFchai 
                    AND creditos.fec_desembolso <= GdaFchaF 
                    AND creditos.estado = 2
                    AND creditos.sdo_capital > 0)
        BREAK
            BY clientes.superUnidadDeCaptura
            BY clientes.SuperSubCuenta:
        FOR EACH creditos NO-LOCK
            WHERE
                creditos.nit = clientes.nit
            AND creditos.fec_desembolso >= GdaFchai 
            AND creditos.fec_desembolso <= GdaFchaF
            AND creditos.estado = 2:
            CREATE t338Creditos.
            assign  t338Creditos.UnidadDeCaptura        = clientes.superUnidadDeCaptura
                    t338Creditos.SubCuenta              = clientes.SuperSubCuenta
                    t338Creditos.Columna                = 0
                    t338Creditos.iFila                  = 0
                    t338Creditos.iColumna               = 0
                    t338Creditos.SaldoCapital           = creditos.sdo_capital
                    t338Creditos.InteresCorriente       = Creditos.Int_Corrientes
                    t338Creditos.InteresAnticipado      = Creditos.Int_Anticipado
                    t338Creditos.InteresMoraCobrar      = Creditos.Int_MorCobrar
                    t338Creditos.InteresDificilCobro    = Creditos.Int_DifCobro
                    t338Creditos.nit                    = clientes.nit
                    t338Creditos.TipoDeCredito          = creditos.Tip_Credito
                    t338Creditos.Monto                  = creditos.Monto
                    t338Creditos.Agencia                = creditos.agencia.
        END. /* FOR EACH creditos NO-LOCK */
    END. /* FOR EACH clientes NO-LOCK */
    RETURN TRUE.
END FUNCTION. /* FUNCTION fPraFrmtoSper338Crdtos RETURNS LOGICAL(): */

FUNCTION fNDesembolsosCreditosConsumo RETURNS INTEGER (iAgncia AS INTEGER):
    /*  Descripcion: Devuelve el Número de Desembolsos de Créditos de Consumo en rl período dado 
        Creado:14 Abr 2008, Ing. Edilberto Mariño Moya
    */
    DEF VAR iVRtrno AS INTEGER NO-UNDO.
    FOR EACH t338Creditos
        WHERE
            t338Creditos.TipoDeCredito = 1
        AND t338Creditos.Agencia = iAgncia:
        iVRtrno = iVRtrno + 1.
    END.
    RETURN iVRtrno.
END FUNCTION.
/* aqui */

/*RUN FchaDeCrteSUPER(STRING(DATE(01/01/2008)) + chr(1) + STRING(DATE(03/31/2008)) + chr(1) + STRING(TODAY)). */


FUNCTION fRgstroTpoCincoSTD3 RETURN CHAR():
    /*
        Descipcion: Formato Super 338
        Creado:3 Mar 2008, Ing. Edilberto Mariño Moya
        NOTAS: Según Documento sbds003.doc
               Circular Formato 338_20080213
    */
    DEF VAR cl AS CHAR NO-UNDO.
    GiNmroRgstro = GiNmroRgstro + 1.
    overlay(cl,1,5) = STRING(GiNmroRgstro,"99999"). /* NUMERO DEL REGISTRO */
    overlay(cl,6,1) = "5". /* CONSTANTE */
    PUT UNFORMATTED cl SKIP.
    RETURN cl.
END FUNCTION. /* FUNCTION fRgstroTpoUnoSTD3 RETURN CHAR(): */

FUNCTION fCrearBuffer RETURNS HANDLE(c AS CHAR):
    /*
        DESCRIPCION: CREA UN BUFFER DINAMICO
        CREADA: 14 MAR 2008, ING EDILBERTO MARIÑO MOYA
    */
    DEF VAR htbla AS HANDLE.
    CREATE BUFFER htbla FOR TABLE c NO-ERROR.
    IF NOT VALID-HANDLE(htbla)
    THEN DO:
        MESSAGE "ERROR FATAL: Creado Buffer Para '" c "'. Imposible Continuar." 
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        QUIT.
    END.
    RETURN htbla.
END. /* FUNCTION fCrearBuffer RETURNS HANDLE(c AS CHAR): */
    
FUNCTION fLeerVlorAAcmlar RETURNS DECIMAL(c AS CHAR):
    /*
        DESCRIPCION: ENCUENTRA EL VALOR A ACUMULAR
        CREADA: 14 MAR 2008, ING EDILBERTO MARIÑO MOYA
    */
    DEF VAR deVlorRtrno AS DECIMAL NO-UNDO.
    DEF VAR hTbla AS HANDLE NO-UNDO.
    DEF VAR hCmpo AS HANDLE NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR daFcha AS DATE NO-UNDO.
    DEF VAR hCmpoFcha AS HANDLE NO-UNDO.
    DEF VAR deTRM AS DECIMAL NO-UNDO.
    deVlorRtrno = 0.
    FOR EACH hTblas:
        hTbla = hTblas.hTbla.
        IF NOT trim(superformatod.CampoFechaParaTRM) = "" 
        THEN DO:
            hCmpoFcha = htbla:BUFFER-FIELD(superformatod.CampoFechaParaTRM) NO-ERROR.
            dafcha = hCmpoFcha:BUFFER-VALUE NO-ERROR. /* fecha para la TRM */
            detrm = ftrm(dafcha).
        END.
        DO i = 1 TO NUM-ENTRIES(c,","):
            hcmpo = htbla:BUFFER-FIELD(entry(i,c,",")) NO-ERROR.
            IF VALID-HANDLE(hcmpo)
            THEN deVlorRtrno = deVlorRtrno + hcmpo:BUFFER-VALUE.
        END.
        deVlorRtrno = deVlorRtrno / (IF SuperFormatoC.EnDolares = "S" THEN (IF NOT detrm = 0 THEN detrm ELSE 1) ELSE 1).
    END.
    RETURN deVlorRtrno.
END. /* FUNCTION fLeerVlorAAcmlar RETURNS DECIMAL(c AS CHAR): */

FUNCTION fRgstroTpoCuatroSTD3 RETURNS LOGICAL(cFrmto AS CHAR):
    /* 
        DESCRIPCION: PROCESA PARAMETROS PARA EL FORMULARIO 338
        CREADA: 17 MAR 2008, ING EDILBERTO MARIÑO MOYA
    */
    DEF BUFFER SuperFormatoDUndadDeCptra    FOR SuperFormatoD.
    DEF BUFFER SuperFormatoDSubCuenta       FOR SuperFormatoD.
    DEF VAR iFlaUno AS INTEGER NO-UNDO.
    DEF VAR cTtloClmna AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    ifila = 2.
    icolumna = 2.
    FOR EACH SUPERformatoc NO-LOCK
        WHERE
            SuperFormatoc.Codigo = cFrmto:
        FOR EACH SuperFormatoD NO-LOCK
            WHERE
                SuperFormatoD.codigo = SuperFormatoC.codigo
            AND (NOT SuperFormatoD.cquery = "" OR NOT trim(SuperFormatoD.funcion) = ""),
            FIRST SuperFormatoDUndadDeCptra NO-LOCK
                WHERE
                    SuperFormatoDUndadDeCptra.codigo            = SuperFormatoC.codigo
                AND SuperFormatoDUndadDeCptra.UnidadDeCaptura   = SuperFormatoD.UnidadDeCaptura,
            FIRST SuperFormatoDSubCuenta NO-LOCK
                WHERE
                    SuperFormatoDSubCuenta.codigo               = SuperFormatoC.codigo
                AND SuperFormatoDSubCuenta.UnidadDeCaptura      = SuperFormatoD.UnidadDeCaptura
                AND SuperFormatoDSubCuenta.subCuenta            = SuperFormatoD.SubCuenta
            break
                BY  SuperFormatoD.UnidadDeCaptura
                BY  SuperFormatoD.Subcuenta
                BY  SuperFormatoD.Columna:
            i = LOOKUP(SuperFormatoD.titulo,cTtloClmna,CHR(1)).
            IF i = 0 
            THEN cTtloClmna = cTtloClmna + SuperFormatoD.titulo + CHR(1).
            
            IF FIRST(SuperFormatoD.UnidadDeCaptura)
            THEN DO:

            END.
            IF FIRST-OF(SuperFormatoD.UnidadDeCaptura)
            THEN DO:
                iFILA = iFILA + 1.
                iFlaUno = iFILA.
                iColumna = 1.
                ftoexcelvlor(fcoor(iFILA,iColumna),string(SuperFormatoDUndadDeCptra.UnidadDeCaptura)).
                iColumna = 2.
                ftoexcelvlor(fcoor(iFILA,iColumna),CAPS(SuperFormatoDUndadDeCptra.Titulo)).
            END. /* IF FIRST(SuperFormatoD.UnidadDeCaptura) */


            IF FIRST-OF(SuperFormatoD.Subcuenta)
            THEN DO:
                cTtloClmna = SuperFormatoD.titulo + CHR(1).
                iFILA = iFILA + 1.
                iColumna = 1.
                ftoexcelvlor(fcoor(iFILA,iColumna),string(SuperFormatoD.Subcuenta)).
                
                iColumna = iColumna + 1.
                ftoexcelvlor(fcoor(iFILA,iColumna),string(SuperFormatoDSubCuenta.Titulo)).
            END. /* IF FIRST-OF(SuperFormatoD.Subcuenta) */
            
            IF NOT trim(SuperFormatoD.funcion) = "" /* ejecuta la función definida */
            THEN DO:
                IF TRIM(SuperFormatoD.cPrmtro) = "" 
                THEN deVlor = DYNAMIC-FUNCTION(SuperFormatoD.funcion) NO-ERROR.                    
                ELSE deVlor = DYNAMIC-FUNCTION(SuperFormatoD.funcion,SuperFormatoD.cPrmtro) NO-ERROR.                    
                IF ERROR-STATUS:ERROR
                THEN DO:
                    MESSAGE "ERROR: La Función '" + SuperFormatoD.funcion + "' NO Se Encontró" 
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                END.
            END. /* IF NOT trim(SuperFormatoD.funcion) = ""  */ 
            ELSE DO: /* ejecuta el query definido */
                EMPTY TEMP-TABLE hTblas.
                DO i = 1 TO NUM-ENTRIES(SuperFormatoD.Tblas,","):
                    CREATE hTblas.
                    ASSIGN  hTblas.Htbla    = fCrearBuffer(ENTRY(i,SuperFormatoD.Tblas,","))
                            hTblas.htNmbre  = ENTRY(i,SuperFormatoD.Tblas,",").
                END. /* DO i = 1 TO NUM-ENTRIES(SuperFormatoD.Tblas,","): */
                CREATE QUERY hQHndle.
                FOR EACH hTblas:
                    hQHndle:SET-BUFFERS(hTblas.htbla).
                END.
                hQHndle:QUERY-PREPARE(SuperFormatoD.cquery).
                hQHndle:QUERY-OPEN().
                deVlor = 0.
                hQHndle:GET-FIRST().
                DO WHILE NOT hQHndle:QUERY-OFF-END:
                    deVlor = deVlor + fLeerVlorAAcmlar(SuperFormatoD.CamposASumar).
                    hQHndle:GET-NEXT().
                END. /* DO WHILE NOT hQHndle:QUERY-OFF-END: */
                hQHndle:GET-FIRST().
                hQHndle:QUERY-CLOSE().
                DELETE OBJECT hQHndle.
            END. /* ELSE DO: /* ejecuta el query definido */ */
/**********************************************************************************************************************************/
            deVlor = IF SuperFormatoC.EnMiles = "S" THEN  fEn1000s(deVlor) ELSE deVlor. /* SI O NO SE REPORTA EN MILES */
            GiNmroRgstro = GiNmroRgstro + 1.
            cl = "".
            overlay(cl,1,5) = STRING(GiNmroRgstro,"99999"). /* NUMERO DEL REGISTRO */
            overlay(cl,6,1) = "4". /* CONSTANTE */
            overlay(cl,7,3) = SuperFormatoD.Codigo. /* NUMERO DE FORMATO */
            overlay(cl,10,2) = STRING(SuperFormatoD.Columna,"99").
            overlay(cl,12,2) = string(SuperFormatoD.UnidadDeCaptura,"99").
            overlay(cl,14,3) = STRING(SuperFormatoD.Subcuenta,"999").
            overlay(cl,17,1) = IF devlor >= 0 THEN "+" ELSE "-". /* signo */
            overlay(cl,18,20) = replace(string(abs(devlor),"99999999999999999.99"),",",".").
            PUT UNFORMATTE cl /* + SuperFormatoD.cQuery + " " + SuperFormatoD.CamposASumar */ SKIP.
            icolumna = icolumna + 1. 
            ftoexcelvlor(fcoor(iFILA,iColumna),replace(string(devlor),",",".")).
            chexcel:COLUMNS(fcolexcel(iColumna)):columnwidth = 18.29.
            chexcel:COLUMNS(fcolexcel(iColumna + 1)):columnwidth = 18.29.

/***********************************************************************************************************************************/            

            IF LAST-OF(SuperFormatoD.Subcuenta)
            THEN DO:
                cTtloClmna = TRIM(cTtloClmna,CHR(1)).
            END. /* IF LAST-OF(SuperFormatoD.Subcuenta) */
            IF LAST-OF(SuperFormatoD.UnidadDeCaptura)
            THEN DO:
                /* POR CADA UNIDAD DE CAPTURA, SE MUESTRAN OS TITILOS DE LAS COLUMNAS */
                DO i = 1 TO num-entries(cTtloClmna,CHR(1)):
                    ftoexcelvlor(fcoor(iFlaUno,i + 2),CAPS(ENTRY(I,CttLOcLMNA,CHR(1)))).
                END.
                /* FIN por cada unidad de captura, se muestran los titulos */

            END.
        END. /* FOR EACH SuperFormatoD NO-LOCK */
    END. /* FOR EACH SUPERformatoc NO-LOCK */
END FUNCTION. /* FUNCTION fRgstroTpoCuatroSTD3 RETURNS LOGICAL(): */

FUNCTION fStndar7 RETURNS LOGICAL(cFrmto AS CHAR):
    /* 
        DESCRIPCION: GENERA REGISTRO TIPO 4 PARA EL ESTANDAR 7
        CREADA: 20 MAY 2008, ING EDILBERTO MARIÑO MOYA
    */
    DEF BUFFER SuperFormatoDUndadDeCptra    FOR SuperFormatoD.
    DEF BUFFER SuperFormatoDSubCuenta       FOR SuperFormatoD.
    DEF VAR iFlaUno AS INTEGER NO-UNDO.
    DEF VAR cTtloClmna AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    ifila = 2.
    icolumna = 2.
    FOR EACH SUPERformatoc NO-LOCK
        WHERE
            SuperFormatoc.Codigo = cFrmto:
        FOR EACH SuperFormatoD NO-LOCK
            WHERE
                SuperFormatoD.codigo = SuperFormatoC.codigo
            AND (NOT SuperFormatoD.cquery = "" OR NOT trim(SuperFormatoD.funcion) = ""),
            FIRST SuperFormatoDUndadDeCptra NO-LOCK
                WHERE
                    SuperFormatoDUndadDeCptra.codigo            = SuperFormatoC.codigo
                AND SuperFormatoDUndadDeCptra.UnidadDeCaptura   = SuperFormatoD.UnidadDeCaptura,
            FIRST SuperFormatoDSubCuenta NO-LOCK
                WHERE
                    SuperFormatoDSubCuenta.codigo               = SuperFormatoC.codigo
                AND SuperFormatoDSubCuenta.UnidadDeCaptura      = SuperFormatoD.UnidadDeCaptura
                AND SuperFormatoDSubCuenta.subCuenta            = SuperFormatoD.SubCuenta
            break
                BY  SuperFormatoD.UnidadDeCaptura
                BY  SuperFormatoD.Subcuenta
                BY  SuperFormatoD.Columna:
            i = LOOKUP(SuperFormatoD.titulo,cTtloClmna,CHR(1)).
            IF i = 0 
            THEN cTtloClmna = cTtloClmna + SuperFormatoD.titulo + CHR(1).

            IF FIRST(SuperFormatoD.UnidadDeCaptura)
            THEN DO:

            END.
            IF FIRST-OF(SuperFormatoD.UnidadDeCaptura)
            THEN DO:
                iFILA = iFILA + 1.
                iFlaUno = iFILA.
                iColumna = 1.
                ftoexcelvlor(fcoor(iFILA,iColumna),string(SuperFormatoDUndadDeCptra.UnidadDeCaptura)).
                iColumna = 2.
                ftoexcelvlor(fcoor(iFILA,iColumna),CAPS(SuperFormatoDUndadDeCptra.Titulo)).
            END. /* IF FIRST(SuperFormatoD.UnidadDeCaptura) */


            IF FIRST-OF(SuperFormatoD.Subcuenta)
            THEN DO:
                cTtloClmna = SuperFormatoD.titulo + CHR(1).
                iFILA = iFILA + 1.
                iColumna = 1.
                ftoexcelvlor(fcoor(iFILA,iColumna),string(SuperFormatoD.Subcuenta)).

                iColumna = iColumna + 1.
                ftoexcelvlor(fcoor(iFILA,iColumna),string(SuperFormatoDSubCuenta.Titulo)).
            END. /* IF FIRST-OF(SuperFormatoD.Subcuenta) */

            IF NOT trim(SuperFormatoD.funcion) = "" /* ejecuta la función definida */
            THEN DO:
                IF TRIM(SuperFormatoD.cPrmtro) = "" 
                THEN deVlor = DYNAMIC-FUNCTION(SuperFormatoD.funcion) NO-ERROR.                    
                ELSE deVlor = DYNAMIC-FUNCTION(SuperFormatoD.funcion,SuperFormatoD.cPrmtro) NO-ERROR.                    
                IF ERROR-STATUS:ERROR
                THEN DO:
                    MESSAGE "ERROR: La Función '" + SuperFormatoD.funcion + "' NO Se Encontró" 
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                END.
            END. /* IF NOT trim(SuperFormatoD.funcion) = ""  */ 
            ELSE DO: /* ejecuta el query definido */
                EMPTY TEMP-TABLE hTblas.
                DO i = 1 TO NUM-ENTRIES(SuperFormatoD.Tblas,","):
                    CREATE hTblas.
                    ASSIGN  hTblas.Htbla    = fCrearBuffer(ENTRY(i,SuperFormatoD.Tblas,","))
                            hTblas.htNmbre  = ENTRY(i,SuperFormatoD.Tblas,",").
                END. /* DO i = 1 TO NUM-ENTRIES(SuperFormatoD.Tblas,","): */
                CREATE QUERY hQHndle.
                FOR EACH hTblas:
                    hQHndle:SET-BUFFERS(hTblas.htbla).
                END.
                hQHndle:QUERY-PREPARE(SuperFormatoD.cquery).
                hQHndle:QUERY-OPEN().
                deVlor = 0.
                hQHndle:GET-FIRST().
                DO WHILE NOT hQHndle:QUERY-OFF-END:
                    deVlor = deVlor + fLeerVlorAAcmlar(SuperFormatoD.CamposASumar).
                    hQHndle:GET-NEXT().
                END. /* DO WHILE NOT hQHndle:QUERY-OFF-END: */
                hQHndle:GET-FIRST().
                hQHndle:QUERY-CLOSE().
                DELETE OBJECT hQHndle.
            END. /* ELSE DO: /* ejecuta el query definido */ */
/**********************************************************************************************************************************/
            deVlor = IF SuperFormatoC.EnMiles = "S" THEN  fEn1000s(deVlor) ELSE deVlor. /* SI O NO SE REPORTA EN MILES */
            GiNmroRgstro = GiNmroRgstro + 1.
            cl = "".
            overlay(cl,1,5) = STRING(GiNmroRgstro,"99999"). /* NUMERO DEL REGISTRO */
            overlay(cl,6,1) = "4". /* CONSTANTE */
            overlay(cl,7,3) = SuperFormatoD.Codigo. /* NUMERO DE FORMATO */
            overlay(cl,10,2) = STRING(SuperFormatoD.Columna,"99").
            overlay(cl,12,2) = string(SuperFormatoD.UnidadDeCaptura,"99").
            overlay(cl,14,3) = STRING(SuperFormatoD.Subcuenta,"999").
            overlay(cl,17,1) = IF devlor >= 0 THEN "+" ELSE "-". /* signo */
            overlay(cl,18,20) = replace(string(abs(devlor),"99999999999999999.99"),",",".").
            PUT UNFORMATTE cl /* + SuperFormatoD.cQuery + " " + SuperFormatoD.CamposASumar */ SKIP.
            icolumna = icolumna + 1. 
            ftoexcelvlor(fcoor(iFILA,iColumna),replace(string(devlor),",",".")).
            chexcel:COLUMNS(fcolexcel(iColumna)):columnwidth = 18.29.
            chexcel:COLUMNS(fcolexcel(iColumna + 1)):columnwidth = 18.29.

/***********************************************************************************************************************************/            

            IF LAST-OF(SuperFormatoD.Subcuenta)
            THEN DO:
                cTtloClmna = TRIM(cTtloClmna,CHR(1)).
            END. /* IF LAST-OF(SuperFormatoD.Subcuenta) */
            IF LAST-OF(SuperFormatoD.UnidadDeCaptura)
            THEN DO:
                /* POR CADA UNIDAD DE CAPTURA, SE MUESTRAN OS TITILOS DE LAS COLUMNAS */
                DO i = 1 TO num-entries(cTtloClmna,CHR(1)):
                    ftoexcelvlor(fcoor(iFlaUno,i + 2),CAPS(ENTRY(I,CttLOcLMNA,CHR(1)))).
                END.
                /* FIN por cada unidad de captura, se muestran los titulos */

            END.
        END. /* FOR EACH SuperFormatoD NO-LOCK */
    END. /* FOR EACH SUPERformatoc NO-LOCK */
END FUNCTION. /* FUNCTION fStndar7 RETURNS LOGICAL(): */


FUNCTION fPraFrmtoSper338Ahorros RETURNS LOGICAL ():
    /*
        Descipcion: Crea La Tabla Temporal t338Ahorros que contiene los datos resumen de Ahorros para ser usados en los querys
        Creado:27 Mar 2008, Ing. Edilberto Mariño Moya
    */
    EMPTY TEMP-TABLE t338Ahorros.
    FOR EACH clientes NO-LOCK
        WHERE
            CAN-FIND(
                FIRST ahorros 
                    WHERE 
                        ahorros.nit = clientes.nit 
                    /* AND ahorros.fec_desembolso >= GdaFchai 
                    AND creditos.fec_desembolso <= GdaFchaF  */
                    AND ahorros.estado = 1
                    AND ahorros.sdo_disponible > 0)
        BREAK
            BY clientes.superUnidadDeCaptura
            BY clientes.SuperSubCuenta:
        FOR EACH ahorros NO-LOCK
            WHERE
                ahorros.nit = clientes.nit
            AND ahorros.estado = 1
            AND ahorros.sdo_disponible > 0:
            CREATE t338Ahorros.
            ASSIGN  t338Ahorros.UnidadDeCaptura         = clientes.SuperUnidadDeCaptura
                    t338Ahorros.SubCuenta               = clientes.SuperSubCuenta
                    t338Ahorros.Columna                 = 0
                    t338Ahorros.iFila                   = 0
                    t338Ahorros.iColumna                = 0
                    t338Ahorros.SaldoDisponible         = Ahorros.sdo_disponible
                    t338Ahorros.nit                     = clientes.nit
                    t338Ahorros.TipoDeAhorro            = Ahorros.Tip_ahorro
                    t338Ahorros.Producto                = Ahorros.Cod_ahorro.
    
        END. /* FOR EACH ahorros NO-LOCK */
    END. /* FOR EACH clientes NO-LOCK */
END FUNCTION. /* FUNCTION fPraFrmtoSper338CrdtosAhorros RETURNS LOGICAL (): */

FUNCTION f338 RETURNS CHAR():
    /*
        Descipcion: Formato Super 338
        Creado:27 feb 2008, Ing. Edilberto Mariño Moya
    */
    GiNmroRgstro = 0.
    fPraFrmtoSper338Crdtos().           /* carga t338Creditos */ 
    fPraFrmtoSper338Ahorros().    /* carga t338Ahorros */ 
    OUTPUT TO VALUE(GNmbreArchvoSlda).
    fRgstroTpoUnoSTD3(1,0).
    fRgstroTpoDosSTD3().
    fRgstroTpoTresSTD3().
    fRgstroTpoCuatroSTD3("338").
    fRgstroTpoCincoSTD3().
    SEEK OUTPUT TO 0.
    fRgstroTpoUnoSTD3(1,0).
    OUTPUT CLOSE.
    
    /* EL PRIMER REGISTRO */
    PUT UNFORMATTE fRgstroTpoUnoSTD3(1,0) SKIP.
END FUNCTION. /* FUNCTION f338 RETURNS CHAR(): */

FUNCTION f170 RETURNS CHAR():
    /*
        Descipcion: Formato Super 170
        Creado:30 abr 2008, Ing. Edilberto Mariño Moya
    */
    GiNmroRgstro = 0.
    OUTPUT TO VALUE(GNmbreArchvoSlda).
    fRgstroTpoUnoSTD3(1,0).
    fRgstroTpoDosSTD3().
    fRgstroTpoTresSTD3().
    fRgstroTpoCuatroSTD3("170").
    fRgstroTpoCincoSTD3().
    SEEK OUTPUT TO 0.
    fRgstroTpoUnoSTD3(1,0).
    OUTPUT CLOSE.
    
    /* EL PRIMER REGISTRO */
    PUT UNFORMATTE fRgstroTpoUnoSTD3(1,0) SKIP.
END FUNCTION. /* FUNCTION f338 RETURNS CHAR(): */

FUNCTION f341 RETURNS CHAR():
    /* 
        Descipcion: FORMATO 341
        Creado: 21 may 2008, Ing. Edilberto Mariño Moya
    */
    GiNmroRgstro = 0.
    OUTPUT STREAM stRegTpo1 TO c:\tmp\formato170aRegTpo1.txt.
    OUTPUT STREAM stRegTpo2 TO c:\tmp\formato170aRegTpo2.txt.
    OUTPUT STREAM stRegTpo3 TO c:\tmp\formato170aRegTpo3.txt.
    OUTPUT STREAM stRegTpo4 TO c:\tmp\formato170aRegTpo4.txt.
    OUTPUT STREAM stRegTpo5 TO c:\tmp\formato170aRegTpo5.txt.
    fRgstroTpoUnoSTD3(1,0).
    fRgstroTpoDosSTD3().
    fRgstroTpoTresSTD3().
    fRgstroTpoCuatroSTD3("170").
    fRgstroTpoCincoSTD3().
    OUTPUT STREAM stRegTpo5 TO c:\tmp\formato170aRegTpo5.txt.
    OUTPUT STREAM stRegTpo1 CLOSE.
    OUTPUT STREAM stRegTpo2 CLOSE.
    OUTPUT STREAM stRegTpo3 CLOSE.
    OUTPUT STREAM stRegTpo4 CLOSE.
    OUTPUT STREAM stRegTpo5 CLOSE.
    OUTPUT STREAM stRegTpo6 CLOSE.
    
    /* EL PRIMER REGISTRO */
    PUT UNFORMATTE fRgstroTpoUnoSTD3(1,0) SKIP.
END FUNCTION. /* FUNCTION f338 RETURNS CHAR(): */
    
FUNCTION f317 RETURNS CHAR():
    /*
        Descipcion: Formato Super 317
        Creado: 12 may 2008, Ing. Edilberto Mariño Moya
    */
    GiNmroRgstro = 0.
    OUTPUT TO VALUE(GNmbreArchvoSlda).
    fRgstroTpoUnoSTD3(1,0).
    fRgstroTpoDosSTD3().
    fRgstroTpoTresSTD3().
    fRgstroTpoCuatroSTD3("317").
    fRgstroTpoCincoSTD3().
    SEEK OUTPUT TO 0.
    fRgstroTpoUnoSTD3(1,0).
    OUTPUT CLOSE.
    
    /* EL PRIMER REGISTRO */
    PUT UNFORMATTE fRgstroTpoUnoSTD3(1,0) SKIP.
END FUNCTION. /* FUNCTION f317 RETURNS CHAR(): */


/* **************************************************************************************************************************************
    PROCEDIMIENTOS
***************************************************************************************************************************************/    
PROCEDURE AgnciaSlcciondaSUPER:
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    GAGNCIA = INTEGER(ENTRY(1,c,CHR(1))).        
END PROCEDURE.
PROCEDURE UsuarioActualSUPER:
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.        
    GUSUARIO = ENTRY(1,c,CHR(1)) NO-ERROR.        
END PROCEDURE.
PROCEDURE AgenciaActualSUPER:
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    GIAGENCIAACTUAL = integer(ENTRY(1,c,CHR(1))) NO-ERROR.        
END PROCEDURE.
PROCEDURE DNEElHdeExelSUPER:
    DEF INPUT PARAMETER chExcelApplication AS COM-HANDLE NO-UNDO.
    chExcel = chExcelApplication.         
END PROCEDURE. /* fin ----- ElHdeExel */
PROCEDURE FchaDeCrteSUPER:
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    GdaFchaI        = DATE(ENTRY(1,c,CHR(1))).
    GDaFchaF        = DATE(ENTRY(2,c,CHR(1))).
    GDAFECHACORTE   = DATE(ENTRY(3,c,CHR(1))).
END PROCEDURE.
PROCEDURE pFrmtoActual:
    DEFINE INPUT PARAMETER c AS CHAR NO-UNDO.
    GFORMATO = c.
    FOR FIRST SuperFormatoC NO-LOCK
        WHERE
            SuperFormatoC.Codigo = GFORMATO:
        GNmbreArchvoSlda = SuperFormatoC.NombreArchivoSalida.
    END.
    GNmbreArchvoSlda = REPLACE(GNmbreArchvoSlda,"%ano%",string(year(GDAFECHACORTE))).
    GNmbreArchvoSlda = REPLACE(GNmbreArchvoSlda,"%mes%",string(MONTH(GDAFECHACORTE),"99")).
    GNmbreArchvoSlda = REPLACE(GNmbreArchvoSlda,"%dia%",string(DAY(GDAFECHACORTE),"99")).
END PROCEDURE.
