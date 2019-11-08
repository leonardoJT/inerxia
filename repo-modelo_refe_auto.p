/*
    Nombre:         repo-modelo_refe_auto.p
    Descripción:    Modelo De Referencia, Programado De Acuerdo A Las Especificaciones Entregadas Por Santiago
    Log:            Creado, 22 nov 2007, Ing Edilberto Mariño Moya.
*/    

DEF VAR E AS DECIMAL NO-UNDO INITIAL 2.718281.
DEF VAR DAFECHADECORTE AS DATE NO-UNDO INITIAL TODAY.
DEF VAR CTIPOGARANTIASFG AS CHAR NO-UNDO.
DEF VAR ICODIGOTIPODEGARANTIAMODELO AS INTEGER NO-UNDO.
DEF VAR ITIPODECREDITO AS INTEGER NO-UNDO.
DEF VAR CNIT AS CHAR NO-UNDO.
DEF VAR INUMERODECREDITO AS INTEGER NO-UNDO.
DEF VAR CCALCULOS AS CHAR NO-UNDO.

SUBSCRIBE "pDatosCredito" ANYWHERE.

/********************************/
/* TEMP TABLE CALIFICACION MORA */
/********************************/
    
    DEF TEMP-TABLE tCalificacionMora NO-UNDO
        FIELD CategoriaReporte AS CHAR
        FIELD CategoriaAgrupada AS CHAR
        FIELD DesdeEnDias AS INTEGER
        FIELD HastaEnDias AS INTEGER
        FIELD Categoria AS CHAR.
    CREATE tCalificacionMora.
    ASSIGN  CategoriaReporte    = "AA"
            CategoriaAgrupada   = "A"
            DesdeEnDias         = 0
            HastaEnDias         = 29
            Categoria           = "AA".
    CREATE tCalificacionMora.
    ASSIGN  CategoriaReporte    = "A"
            CategoriaAgrupada   = "A"
            DesdeEnDias         = 30
            HastaEnDias         = 59
            Categoria           = "A".
    CREATE tCalificacionMora.
    ASSIGN  CategoriaReporte    = "BB"
            CategoriaAgrupada   = "B"
            DesdeEnDias         = 60
            HastaEnDias         = 89
            Categoria           = "BB".
    CREATE tCalificacionMora.
    ASSIGN  CategoriaReporte    = "B"
            CategoriaAgrupada   = "B"
            DesdeEnDias         = 90
            HastaEnDias         = 119
            Categoria           = "B".
    CREATE tCalificacionMora.
    ASSIGN  CategoriaReporte    = "CC"
            CategoriaAgrupada   = "C"
            DesdeEnDias         = 120
            HastaEnDias         = 149
            Categoria           = "CC".
    CREATE tCalificacionMora.
    ASSIGN  CategoriaReporte    = "C"
            CategoriaAgrupada   = "C"
            DesdeEnDias         = 150
            HastaEnDias         = 999999
            Categoria           = "INCUMPLIMIENTO".
/************************************/
/* FIN TEMP TABLE CALIFICACION MORA */
/************************************/


/************************************************************/
/*   DEFINICION DE VARIABLES BASICAS QUE REQUIERE EL MODELO */
/************************************************************/
    DEF TEMP-TABLE tPrmtros NO-UNDO
        FIELD iItem AS INTEGER /* consecutivo */
        FIELD cDscrpcion AS CHAR /* descripcion, lo que contiene ccmpo */
        FIELD cCmpo AS CHAR. /* Nombre del campo en el repositorio que contiene el valor */

    CREATE tPrmtros.
    ASSIGN  tprmtros.iItem      = 1
            tprmtros.cDscrpcion = "DIAS EN MORA"
            tprmtros.cCmpo      = "int004".
    CREATE tPrmtros.
    ASSIGN  tprmtros.iItem      = 2
            tprmtros.cDscrpcion = "TIPO DE CREDITO"
            tprmtros.cCmpo      = "int005".
/*****************************************************************/
/* FIN DEFINICION DE VARIABLES BASICAS QUE REQUIEREN LAS FUNCIONES */
/*****************************************************************/

/*****************/
/* PUNTAJE HASTA */
/*****************/
    DEF TEMP-TABLE tPuntaje NO-UNDO
        FIELD calificacion AS CHAR FORMAT "xx"
        FIELD tipo AS CHAR FORMAT "x(25)"
        FIELD puntaje AS DECIMAL FORMAT "9.999999"
        FIELD ProbabilidadIncumplimiento AS DECIMAL FORMAT "999.99"
        FIELD tpo_crdto AS INTEGER
        INDEX pk puntaje.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "AA"
            tpuntaje.tipo                       = "GENERAL - AUTOMOVILES"
            tpuntaje.puntaje                    = 0.2484
            tpuntaje.ProbabilidadIncumplimiento = 0.97
            tpuntaje.tpo_crdto                  = 2.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "AA"
            tpuntaje.tipo                       = "GENERAL - OTROS"
            tpuntaje.puntaje                    = 0.3767
            tpuntaje.ProbabilidadIncumplimiento = 2.10
            tpuntaje.tpo_crdto                  = 1.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "AA"
            tpuntaje.tipo                       = "TARJETA DE CREDITO"
            tpuntaje.puntaje                    = 0.3735
            tpuntaje.ProbabilidadIncumplimiento = 1.58
            tpuntaje.tpo_crdto                  = 3.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "AA"
            tpuntaje.tipo                       = "CFC - AUTOMOVILES"
            tpuntaje.puntaje                    = 0.21
            tpuntaje.ProbabilidadIncumplimiento = 1.02
            tpuntaje.tpo_crdto                  = 4.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "AA"
            tpuntaje.tipo                       = "CFC - OTROS"
            tpuntaje.puntaje                    =  0.25
            tpuntaje.ProbabilidadIncumplimiento = 3.54
            tpuntaje.tpo_crdto                  = 5.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "A"
            tpuntaje.tip                        = "GENERAL - AUTOMOVILES"
            tpuntaje.puntaje                    = 0.6842
            tpuntaje.ProbabilidadIncumplimiento = 3.12
            tpuntaje.tpo_crdto                  = 2.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "A"
            tpuntaje.tipo                       = "GENERAL - OTROS"
            tpuntaje.puntaje                    = 0.8205
            tpuntaje.ProbabilidadIncumplimiento = 3.88
            tpuntaje.tpo_crdto                  = 1.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "A"
            tpuntaje.tipo                       = "TARJETA DE CREDITO"
            tpuntaje.puntaje                    = 0.6703
            tpuntaje.ProbabilidadIncumplimiento = 5.35
            tpuntaje.tpo_crdto                  = 3.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "A"
            tpuntaje.tipo                       = "CFC - AUTOMOVILES"
            tpuntaje.puntaje                    = 0.6498
            tpuntaje.ProbabilidadIncumplimiento = 2.88
            tpuntaje.tpo_crdto                  = 4.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "A"
            tpuntaje.tipo                       = "CFC - OTROS"
            tpuntaje.puntaje                    = 0.6897
            tpuntaje.ProbabilidadIncumplimiento = 7.19
            tpuntaje.tpo_crdto                  = 5.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "BB"
            tpuntaje.tipo                       = "GENERAL - AUTOMOVILES"
            tpuntaje.puntaje                    = 0.81507
            tpuntaje.ProbabilidadIncumplimiento = 7.48
            tpuntaje.tpo_crdto                  = 2.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "BB"
            tpuntaje.tipo                       = "GENERAL - OTROS"
            tpuntaje.puntaje                    = 0.89
            tpuntaje.ProbabilidadIncumplimiento = 12.68
            tpuntaje.tpo_crdto                  = 1.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "BB"
            tpuntaje.tipo                       = "TARJETA DE CREDITO"
            tpuntaje.puntaje                    = 0.9382
            tpuntaje.ProbabilidadIncumplimiento = 9.53
            tpuntaje.tpo_crdto                  = 3.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "BB"
            tpuntaje.tipo                       = "CFC - AUTOMOVILES"
            tpuntaje.puntaje                    = 0.905
            tpuntaje.ProbabilidadIncumplimiento = 12.34
            tpuntaje.tpo_crdto                  = 4.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "BB"
            tpuntaje.tipo                       = "CFC - OTROS"
            tpuntaje.puntaje                    = 0.8763
            tpuntaje.ProbabilidadIncumplimiento = 15.86
            tpuntaje.tpo_crdto                  = 5.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "B"
            tpuntaje.tipo                       = "GENERAL - AUTOMOVILES"
            tpuntaje.puntaje                    = 0.94941
            tpuntaje.ProbabilidadIncumplimiento = 15.76
            tpuntaje.tpo_crdto                  = 2.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "B"
            tpuntaje.tipo                       = "GENERAL - OTROS"
            tpuntaje.puntaje                    = 0.9971
            tpuntaje.ProbabilidadIncumplimiento = 14.16
            tpuntaje.tpo_crdto                  = 1.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "B"
            tpuntaje.tipo                       = "TARJETA DE CREDITO"
            tpuntaje.puntaje                    = 0.9902
            tpuntaje.ProbabilidadIncumplimiento = 14.17
            tpuntaje.tpo_crdto                  = 3.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "B"
            tpuntaje.tipo                       = "CFC - AUTOMOVILES"
            tpuntaje.puntaje                    = 0.9847
            tpuntaje.ProbabilidadIncumplimiento = 24.27
            tpuntaje.tpo_crdto                  = 4.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "B"
            tpuntaje.tipo                       = "CFC - OTROS"
            tpuntaje.puntaje                    = 0.9355
            tpuntaje.ProbabilidadIncumplimiento = 31.18
            tpuntaje.tpo_crdto                  = 5.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "CC"
            tpuntaje.tipo                       = "GENERAL - AUTOMOVILES"
            tpuntaje.puntaje                    = 1
            tpuntaje.ProbabilidadIncumplimiento = 31.01
            tpuntaje.tpo_crdto                  = 2.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "CC"
            tpuntaje.tipo                       = "GENERAL - OTROS"
            tpuntaje.puntaje                    = 1
            tpuntaje.ProbabilidadIncumplimiento = 22.57
            tpuntaje.tpo_crdto                  = 1.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "CC"
            tpuntaje.tipo                       = "TARJETA DE CREDITO"
            tpuntaje.puntaje                    = 1
            tpuntaje.ProbabilidadIncumplimiento = 17.06
            tpuntaje.tpo_crdto                  = 3.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "CC"
            tpuntaje.tipo                       = "CFC -AUTOMOVILES"
            tpuntaje.puntaje                    = 1
            tpuntaje.ProbabilidadIncumplimiento = 43.32
            tpuntaje.tpo_crdto                  = 4.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "CC"
            tpuntaje.tipo                       = "CFC - OTROS"
            tpuntaje.puntaje                    = 1
            tpuntaje.ProbabilidadIncumplimiento = 41.01
            tpuntaje.tpo_crdto                  = 5.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "INCUMPLIMIENTO"
            tpuntaje.tipo                       = "GENERAL - AUTOMOVIL"
            tpuntaje.puntaje                    = 1
            tpuntaje.ProbabilidadIncumplimiento = 100
            tpuntaje.tpo_crdto                  = 2.

    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "INCUMPLIMIENTO"
            tpuntaje.tipo                       = "GENERAL - OTROS"
            tpuntaje.puntaje                    = 1
            tpuntaje.ProbabilidadIncumplimiento = 100
            tpuntaje.tpo_crdto                  = 1.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "INCUMPLIMIENTO"
            tpuntaje.tipo                       = "TARJETA DE CREDITO"
            tpuntaje.puntaje                    = 1
            tpuntaje.ProbabilidadIncumplimiento = 100
            tpuntaje.tpo_crdto                  = 3.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "INCUMPLIMIENTO"
            tpuntaje.tipo                       = "CFC - AUTOMOVIL"
            tpuntaje.puntaje                    = 1
            tpuntaje.ProbabilidadIncumplimiento = 100
            tpuntaje.tpo_crdto                  = 4.
    CREATE tpuntaje.
    ASSIGN  tpuntaje.calificacion               = "INCUMPLIMIENTO"
            tpuntaje.tipo                       = "CFC - OTROS"
            tpuntaje.puntaje                    = 1
            tpuntaje.ProbabilidadIncumplimiento = 100
            tpuntaje.tpo_crdto                  = 5.
/*********************/
/* FIN PUNTAJE HASTA */
/*********************/

/***********************************************************************/
/* PEVADI: PERDIDA ESPERADA DE VALOR DEL ACTIVO DADO EL INCUMPLIMIENTO */
/***********************************************************************/
    DEF TEMP-TABLE tpevadi NO-UNDO
        FIELD TipoGarantia AS INTEGER 
        FIELD Descripcion AS CHAR
        FIELD DiasDespuesIncumplimiento AS INTEGER
        FIELD pdi AS DECIMAL
        INDEX pk DiasDespuesIncumplimiento.

    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 1
            Descripcion                 = "Garantía No Admisible"
            DiasDespuesIncumplimiento   = 210
            pdi                         = 60.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 1
            Descripcion                 = "Garantía No Admisible"
            DiasDespuesIncumplimiento   = 420
            pdi                         = 70.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 1
            Descripcion                 = "Garantía No Admisible"
            DiasDespuesIncumplimiento   = 999999
            pdi                         = 100.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 2
            Descripcion                 = "Colateral Financiero Admisible"
            DiasDespuesIncumplimiento   = 999999
            pdi                         = 12.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 3
            Descripcion                 = "Bienes Raices Comerciales y Residenciales"
            DiasDespuesIncumplimiento   = 360
            pdi                         = 40.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 3
            Descripcion                 = "Bienes Raices Comerciales y Residenciales"
            DiasDespuesIncumplimiento   = 720
            pdi                         = 70.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 3
            Descripcion                 = "Bienes Raices Comerciales y Residenciales"
            DiasDespuesIncumplimiento   = 999999
            pdi                         = 100.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 4
            Descripcion                 = "Bienes Dados En Leasing Diferente A Inmobiliario"
            DiasDespuesIncumplimiento   = 270
            pdi                         = 45.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 4
            Descripcion                 = "Bienes Dados En Leasing Diferente A Inmobiliario"
            DiasDespuesIncumplimiento   = 540
            pdi                         = 70.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 4
            Descripcion                 = "Bienes Dados En Leasing Diferente A Inmobiliario"
            DiasDespuesIncumplimiento   = 999999
            pdi                         = 100.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 5
            Descripcion                 = "Otros Colaterales"
            DiasDespuesIncumplimiento   = 270
            pdi                         = 50.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 5
            Descripcion                 = "Otros Colaterales"
            DiasDespuesIncumplimiento   = 540
            pdi                         = 70.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 5
            Descripcion                 = "Otros Colaterales"
            DiasDespuesIncumplimiento   = 999999
            pdi                         = 100.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 6
            Descripcion                 = "Derechos De Cobro"
            DiasDespuesIncumplimiento   = 360
            pdi                         = 45.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 6
            Descripcion                 = "Derechos De Cobro"
            DiasDespuesIncumplimiento   = 720
            pdi                         = 80.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 6
            Descripcion                 = "Derechos De Cobro"
            DiasDespuesIncumplimiento   = 999999
            pdi                         = 100.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 7
            Descripcion                 = "Sin Garantía"
            DiasDespuesIncumplimiento   = 180
            pdi                         = 65.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 7
            Descripcion                 = "Sin Garantía"
            DiasDespuesIncumplimiento   = 360
            pdi                         = 85.
    CREATE tpevadi.
    ASSIGN  TipoGarantia                = 7
            Descripcion                 = "Sin Garantía"
            DiasDespuesIncumplimiento   = 999999
            pdi                         = 100.

/***************************************************************************/
/* FIN PEVADI: PERDIDA ESPERADA DE VALOR DEL ACTIVO DADO EL INCUMPLIMIENTO */
/***************************************************************************/


FUNCTION fCCalculos RETURN CHAR(c AS CHAR):
    /*
        Descripcion: Prepara string de calculos intermedios y valores a grabar
        log: Creada 6 de dic de 2007, Ing. Edilberto Mariño Moya 
    */
    DEF VAR cTtlo AS CHAR NO-UNDO.
    DEF VAR cVlor AS CHAR NO-UNDO.
    DEF VAR cCmpo AS CHAR NO-UNDO.

    cTtlo = ENTRY(1,c,CHR(1)) NO-ERROR.
    cVlor = ENTRY(2,c,CHR(1)) NO-ERROR.
    cCmpo = ENTRY(3,c,CHR(1)) NO-ERROR.

    IF CCALCULOS = "" THEN CCALCULOS = CHR(2) + CHR(2).
    DEF VAR cTtlos AS CHAR NO-UNDO.
    DEF VAR cVlres AS CHAR NO-UNDO.
    DEF VAR cCmpos AS CHAR NO-UNDO.

    DEF VAR i AS INTEGER NO-UNDO.
    cTtlos = ENTRY(1,CCALCULOS,CHR(2)) NO-ERROR.
    cVlres = ENTRY(2,CCALCULOS,CHR(2)) NO-ERROR.
    cCmpos = ENTRY(3,CCALCULOS,CHR(2)) NO-ERROR.

    i = LOOKUP(cTtlo,cTtlos,CHR(1)).
    IF i = 0 
    THEN do:
        cTtlos = cTTlos + CHR(1) + cTtlo.
        cVlres = cVlres + chr(1) + cVlor.
        cCmpos = cCmpos + CHR(1) + cCmpo.
    END.
    ELSE ENTRY(i,cVlres,CHR(1)) = cvlor.
    cttlos = TRIM(cttlos,CHR(1)).
    cvlres = TRIM(cvlres,CHR(1)).
    cCmpos = TRIM(cCmpos,CHR(1)).
    CCALCULOS = cttlos + CHR(2) + cvlres  + CHR(2) + ccmpos.
END FUNCTION. /* FUNCTION fCCalculos RETURN CHAR(c AS CHAR): */

FUNCTION fDiasEnMora RETURN CHAR():
    /*
        Descripción: Devuelve El Número De Días En Mora De Un Crédito
        Estructura Del Mensaje De Entrada Con Separador chr(1):
        Estructura Del Mensaje De Salida con Separador chr(1):
            Entrada 1: Número Días En Mora
        Log: Creada 22 de nov de 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR iDiasAtrso AS INTEGER NO-UNDO.
    DEF VAR cRtrno AS CHAR NO-UNDO.
    IF DAFECHADECORTE = ? THEN DAFECHADECORTE = TODAY.
    iDiasAtrso = 0.
    FOR FIRST creditos NO-LOCK
        WHERE
            creditos.num_credito = INUMERODECREDITO:
        idiasatrso = creditos.dias_atraso.
        LEAVE.
    END.
    cRtrno = STRING(iDiasAtrso).
    fccalculos("Dias En Mora" + CHR(1) + crtrno + CHR(1) + "NC" ).
    RETURN crtrno.
END FUNCTION. /* FUNCTION fDiasEnMora RETURN CHAR() */


FUNCTION fCalificacionMora RETURN CHAR(c AS CHAR):
    /*
        Descripción: Califica La Mora Del Crédito
        Estructura Del Mensaje De Entrada Con Separador chr(1)
            Entrada 1: Número De Dias En Mora
        Estructura Del Mensaje De Salida Con Separador chr(1)
            Entrada 1: Calificación
            Entrada 2: Suspende Causación (SI/NO)
        log: Creada, 22 de nov de 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR iMra AS INTEGER NO-UNDO.
    DEF VAR cRtrno AS CHAR NO-UNDO.
    iMra = 0.
    iMra = INTEGER(ENTRY(1,c,CHR(1))) NO-ERROR.
    crtrno = CHR(1).
    FOR EACH tCalificacionMora
        WHERE
            tCalificacionMora.DesdeEnDias <= iMra 
        AND tCalificacionMora.HastaEnDias >= iMra:
        entry(1,cRtrno,CHR(1)) = tCalificacionMora.Categoria.
        ENTRY(2,cRtrno,CHR(1)) = IF tCalificacionMora.categoria = "INCUMPLIMIENTO" THEN "SI" ELSE "NO".
        LEAVE.
    END.
    fccalculos("Calificación Mora" + CHR(1) + crtrno + CHR(1) + "NC").
    RETURN cRtrno.
END FUNCTION. /* FUNCTION fCalificacionMora RETURN CHAR(c AS CHAR): */


FUNCTION fPuntaje RETURN DECIMAL(dez AS DECIMAL):
    /*
                                                                     ||||||||||||||||||
                                                                     |       1        |
        Descripcion: Calcula El Valor Puntaje A Partir De La Ecuación| ______________ |
                                                                     |             -z |
                                                                     |  1     +   e   |
                                                                     ||||||||||||||||||
        Parámetro De Entrada = z                                                                     
        log: Creada, 22 de Nov de 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR deRtrno AS DECIMAL NO-UNDO.
    dez = -1 * dez.
    dertrno =  1 / (1 + EXP(E,dez)).
    fccalculos("Puntaje" + CHR(1) + string(dertrno) + CHR(1) + "calificacion"). 
    RETURN dertrno.
END FUNCTION. /* FUNCTION fpuntaje RETURN DECIMAL(dez AS DECIMAL): */


FUNCTION fAMB RETURN INTEGER(c AS CHAR):
    /*
        Descripción: Altura De Mora Actual entre 31 y 60 dias. Devuelve UNO si la mora actual cae en el rango nombrado
        Estructura Del Mensaje De Entrada Con Separador chr(1):
            Entrada 1: Dias De Mora Actual
        log: Creada, 22 de nov de 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR iMraActual AS INTEGER NO-UNDO.
    DEF VAR iRtrno AS INTEGER NO-UNDO.

    iMraActual = INTEGER(ENTRY(1,c,CHR(1))) NO-ERROR.
    IF iMraActual >= 31 AND iMraActual <= 60
    THEN irtrno = 1.
    ELSE irtrno = 0.
    fccalculos("AMB" + CHR(1) + string(irtrno) + CHR(1) + "NC").
    RETURN irtrno.
END FUNCTION. /* FUNCTION fAMB RETURN DECIMAL(): */

FUNCTION fAMC RETURN INTEGER(c AS CHAR):
    /*
        Descripcion: Altura De Mora Actual Entre 61 y 90 dias. Devuelve UNO si la mora actual cae en el rango nombrado
        Estructura Del Mensaje De Entrada Con Separador chr(1):
            Entrada 1: Dias De Mora Actual
        Log: Creada, 22 De Noviembre De 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR iMraActual AS INTEGER NO-UNDO.
    DEF VAR irtrno AS INTEGER NO-UNDO.
    iMraActual = INTEGER(ENTRY(1,c,CHR(1))) NO-ERROR.
    IF iMraActual >= 61 AND iMraActual <= 90
    THEN irtrno = 1.
    ELSE irtrno = 0.
    fccalculos("AMC" + CHR(1) + string(irtrno) + CHR(1) + "NC").
    RETURN irtrno.
END FUNCTION. /* FUNCTION fAMC RETURN DECIMAL(c AS CHAR): */

FUNCTION f-fecha-Menos-N-anos RETURN DATE(dafcha AS DATE,n AS INTEGER):
    /*
        Descripcion: Devuelve Una Fecha n anos atras
        Parámetros De Entrada:
            Parámetro Uno: Fecha De Corte
            Parámetro dos: Número Años A Devolver
    */        
    DEF VAR dafcha1 AS DATE NO-UNDO.
    DEF VAR iano AS INTEGER NO-UNDO.
    DEF VAR imes AS INTEGER NO-UNDO.
    DEF VAR idia AS INTEGER NO-UNDO.
    
    dafcha1 = dafcha + 1.
    iano = YEAR(dafcha1) - n.
    imes = MONTH(dafcha1).
    idia = DAY(dafcha1).
    dafcha1 = DATE(imes,idia,iano) NO-ERROR.

    DO WHILE ERROR-STATUS:ERROR:
        idia = idia + 1.
        IF idia > 31
        THEN DO:
            imes = imes + 1.
            idia = 1.
        END.
        IF imes > 12
        THEN DO:
            imes = 1.
            idia = 1.
            iano = iano + 1.
        END.
        dafcha1 = DATE(imes,idia,iano) NO-ERROR.
    END.
    fccalculos("Fecha Inicial Promedio" + CHR(1) + string(dafcha1) + CHR(1) + "NC").
    fccalculos("Número Años A Promediar" + CHR(1) + string(n) + CHR(1) + "NC").
    RETURN dafcha1.
END FUNCTION. /* FUNCTION f-fecha-Menos-N-anos RETURN DATE(dafcha AS DATE,n AS INTEGER): */

FUNCTION fMoraPromedio RETURN DECIMAL(c AS CHAR):
    /*
        Descripcion: Calcula La Mora Promedio De Los Ultimos N Años
        Estructura Del Mensaje De Entrada Con Separador chr(1)
            Entrada1: Número De Años A Promediar
        log: Creada, 4 de Dic De 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR iNmroAnosPrmdiar AS INTEGER NO-UNDO.
    DEF VAR daFchaIncial AS DATE NO-UNDO.
    DEF VAR iDiasMra AS INTEGER NO-UNDO.
    DEF VAR iNmroPriodos AS INTEGER NO-UNDO.
    DEF VAR deRtrno AS DECIMAL NO-UNDO.
    iDiasMra = 0.
    iNmroAnosPrmdiar    = INTEGER(  ENTRY(1,c,CHR(1))) NO-ERROR.
    daFchaIncial = f-fecha-Menos-N-anos(DAFECHADECORTE,iNmroAnosPrmdiar).
    FOR EACH repositorio NO-LOCK
        WHERE
            repositorio.nit = CNIT
        AND repositorio.int002 = ITIPODECREDITO   /* tipo de credito */
        AND repositorio.dat001 >= daFchaIncial AND repositorio.dat001 <= DAFECHADECORTE: /* fecha desembolso */
        IF repositorio.int007 > 0 /* dias atraso */
        THEN DO:
            iDiasMra = iDiasMra + repositorio.int007.
            iNmroPriodos = iNmroPriodos + 1.
        END.
    END.
    IF iNmroPriodos = 0 
    THEN dertrno = 0.
    ELSE dertrno = iDiasMra / iNmroPriodos.
    fccalculos("Promedio Mora" + CHR(1) + string(dertrno) + CHR(1) + "NC").
    RETURN dertrno.
END FUNCTION. /* FUNCTION fMoraPromedio RETURN DECIMAL(c AS CHAR): */


FUNCTION fMMB RETURN INTEGER(iMraMxma AS INTEGER):
    /*
        Descripción: Máxima Altura De Mora Entre 31 y 60 dias
        log: Creada, 23 De Nov De 2007,Ing. Edilberto Mariño Moya
    */
    DEF VAR iRtrno AS INTEGER NO-UNDO.
    IF iMraMxma >= 31 AND iMraMxma <= 60 
    THEN iRtrno =  1.
    ELSE iRtrno = 0.
    fccalculos("MMB" + CHR(1) + string(irtrno) + CHR(1) + "NC").
    RETURN iRtrno.
END FUNCTION. /* FUNCTION fMMB RETURN INTEGER(c AS CHAR): */


FUNCTION fMMC RETURN INTEGER(iMraMxma AS INTEGER):
    /*
        Descripcion: Máxima Altura De Mora Entre 61 y 90 Dias
        lOG: Creada, 22 De Nov De 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR irtrno AS INTEGER NO-UNDO.
    IF iMraMxma >= 61 AND iMraMxma <= 90
    THEN irtrno = 1.
    ELSE irtrno = 0.
    fccalculos("MMC" + CHR(1) + string(irtrno) + CHR(1) + "NC").
    RETURN irtrno.
END FUNCTION. /* FUNCTION fMMC RETURN INTEGER(c AS CHAR): */



FUNCTION fHndleCmpo RETURN HANDLE(c AS CHAR):
    /*
        Descripcion: Devuelve el handle del campo en el buffer repositorio
        Estructura Del Mensaje De Entrada con separador chr(1)
            Entrada 1: Nombre del campo
        Log: Creada, 23 de nov de 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR h AS HANDLE NO-UNDO.
    DEF VAR h1 AS HANDLE NO-UNDO.
    DEF VAR cCmpo AS CHAR NO-UNDO.
    cCmpo = ENTRY(1,c,CHR(1)) NO-ERROR.
    h = BUFFER repositorio:HANDLE.
    h1 = h:BUFFER-FIELD(cCmpo) NO-ERROR.
    RETURN h1.
END FUNCTION. /* FUNCTION fHndleCmpo RETURN HANDLE(c AS CHAR): */

FUNCTION fCAR RETURN INTEGER():
    /*
        Descripcion: La Variable CAR Se Puede Ignorar Debido A La Falta De Datos De Los Ultimos Tres Años. Por Lo Tanto es Cero 
        Log: Creada, 26 de noviembre de 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR irtrno AS INTEGER NO-UNDO.
    irtrno = 0.
    fccalculos("CAR" + CHR(1) + string(irtrno) + CHR(1) + "NC").
    RETURN irtrno.
END FUNCTION. /* FUNCTION fCAR RETURN INTEGER(): */


FUNCTION fCAM RETURN INTEGER():
    /*
        Descripcion: La Variable CAM Se Puede Ignorar Debido A La Falta De Datos De Los Ultimos Tres Años. Por Lo Tanto Es Cero
        Log: Creada, 26 De Noviembre De 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR irtrno AS INTEGER NO-UNDO.
    irtrno = 0.
    fccalculos("CAM" + CHR(1) + string(irtrno) + CHR(1) + "NC").
    RETURN irtrno.
END FUNCTION. /* FUNCTION fCAM RETURN integr(): */

FUNCTION fProbabilidadIncumplimiento RETURN DECIMAL(dePuntaje AS DECIMAL):
    /*
        Descripcion: Encuentra La Probabilidad De Incumplimiento Con Base En El Puntaje
        log: Creada: 26 de nov de 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR dePI AS DECIMAL NO-UNDO.
    DEF VAR dertrno AS DECIMAL NO-UNDO.
    DEF VAR cClfccionEquivlnte AS CHAR NO-UNDO.
    FOR FIRST tpuntaje 
        WHERE
            tpuntaje.puntaje >= depuntaje
        AND tpuntaje.tpo_crdto = ITIPODECREDITO:
        dePI = tpuntaje.probabilidadIncumplimiento.
        cClfccionEquivlnte = tpuntaje.calificacion.
    END.
    dertrno = dePI / 100.
    fccalculos("Tipo De Crédito" + CHR(1) + string(ITIPODECREDITO) + CHR(1) + "NC").
    fccalculos("Calificación Equivalente" + CHR(1) + cClfccionEquivlnte + CHR(1) + "calificacionequivalente").
    fccalculos("Probabilidad De Incumplimiento" + CHR(1) + string(dertrno) + CHR(1) + "probabilidadincumplimiento").
    RETURN dertrno.
END FUNCTION. /* FUNCTION fProbabilidadIncumplimiento RETURN DECIMAL(dePuntaje AS DECIMAL): */

FUNCTION fPerdidaEsperadaDeValorDelActivo RETURN DECIMAL(iDiasMora AS INTEGER):
    /*
        Descripción: Devuelve El % De Incumplimiento Dados Los Dias De Mora Y El Tipo De Garantía
        Log: Creada, 26 nov de 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR deVRtrno AS DECIMAL NO-UNDO.
    DEF VAR cTpoGrntia AS CHAR NO-UNDO.
    FOR FIRST tpevadi
        WHERE
            tpevadi.DiasDespuesIncumplimiento >= iDiasMora
        AND tpevadi.TipoGarantia = ICODIGOTIPODEGARANTIAMODELO:
        deVRtrno = tpevadi.pdi.
        cTpoGrntia = tpevadi.descripcion.
    END.
    deVrtrno = deVrtrno / 100.
    fccalculos("Tipo De Garantía Modelo" + CHR(1) + cTpoGrntia + CHR(1) + "NC").
    fccalculos("Pérdida Esperada De Valor Del Activo" + CHR(1) + string(devrtrno) + CHR(1) + "perdidaesperada%").
    RETURN deVrtrno.
END FUNCTION. /* FUNCTION fPerdidaEsperadaDeValorDelActivo RETURN DECIMAL(iDiasMora AS INTEGER,iTipoGarantia AS INTEGER): */


FUNCTION fExposicionActivo RETURN DECIMAL():
    /*        
        Descripcion: Devuelve La Exposición Del Activo, Es Decir El Saldo De La Deuda:
            Saldo Capital, Intereses y Cuentas X Cobrar
        log: Creada, 27 De Nov De 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR deVrtrno AS DECIMAL NO-UNDO.
    DEF VAR daFchaI AS DATE NO-UNDO.
    DEF VAR deVlorDeuda AS DECIMAL NO-UNDO.
    daFchai = DATE(MONTH(DAFECHADECORTE),1,YEAR(DAFECHADECORTE)).

    FOR FIRST repositorio NO-LOCK
        WHERE
            repositorio.NumeroCredito = INUMERODECREDITO:
        /* AND repositorio.dat001 >= daFchai AND repositorio.dat001 <= DAFECHADECORTE: /* fecha desembolso */ */
        /*****************************************/
        /* OJO : VERIFICAR LA FORMULA CON NELSON */
        /*****************************************/
        devlordeuda =   repositorio.dec003 + /* Sdo_Capital */
                        repositorio.dec004 + /* Int_Corrientes */
                        repositorio.dec005 + /* Int_Anticipado */
                        repositorio.dec006 + /* Int_MorCobrar */
                        repositorio.dec007 + /* Int_DifCobro */
                        repositorio.dec008 + /* Int_MoraDifCob */
                        repositorio.dec009 + /* Honorarios */
                        repositorio.dec010.  /* Polizas */
    END.
    fccalculos("Exposición Del Activo" + CHR(1) + string(devlordeuda) + CHR(1) + "exposicionactivo").
    RETURN devlordeuda.
END FUNCTION. /* FUNCTION fExpscionActvo RETURN DECIMAL(): */

FUNCTION fmmd RETURN INTEGER(iMraMxma AS INTEGER):
    /*
        Descripcion: Máxima Altura De Mora mayor de 90 Dias
        lOG: Creada, 27 De Nov De 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR irtrno AS INTEGER NO-UNDO.
    IF iMraMxma > 90
    THEN irtrno = 1.
    ELSE irtrno = 0.
    fccalculos("MMD" + CHR(1) + string(irtrno) + CHR(1) + "NC").
    RETURN irtrno.
END FUNCTION.

FUNCTION fgi RETURN INTEGER():
    /*
        Descripcion: Dado El Tipo De Garantía, Devuelve Un Uno o Un cero
        log: Creada 27 de nov de 2007, Ing. Edilberto Mariño Moya
    */
    /* OJO: COMO SE DEFINE UNA GARANTIA IDONEA */
    DEF VAR cTpos AS CHAR NO-UNDO.
    DEF VAR cIdneidad AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR irtrno AS INTEGER NO-UNDO.
    cTpos = "Admisible y CodeudorCodeudorCon Codeudor(es)ConyugeConyuge + CodeudorConyuge + Codeudor + PropHipotecaLibranza EstablecidaPrendaSin CodeudorX".
    cIdneidad = "01111101111".
    i = LOOKUP(CTIPOGARANTIASFG,cTpos,CHR(1)).
    irtrno = 1. /* valor por defecto */
    IF NOT i = 0 
    THEN irtrno =  integer(ENTRY(i,cIdneidad,CHR(1))).
    fccalculos("FGI" + CHR(1) + string(irtrno) + CHR(1) + "NC").
    RETURN irtrno.
END FUNCTION. /*  FUNCTION fgi RETURN INTEGER(iTipoGarantia): */

FUNCTION fcrb RETURN INTEGER():
    /*
        Descripcion: Créditos Acivos. Devuelve 1 si el cliente tiene otros credito de consumo activos diferentes al del segmento.
        Log: Creada 27 de nov de 2007, ing. Edilberto Mariño Moya
        ????? OTROS CREDITOS DE CONSUMO DIFERENTES AL DEL SEGMENTO ?????
    */
    DEF VAR iRtrno AS INTEGER NO-UNDO.
    FOR EACH repositorio NO-LOCK
        WHERE
            repositorio.nit = cnit
        AND repositorio.int002 <> ITIPODECREDITO:
        irtrno = 1.
        LEAVE.
    END.
    fccalculos("CRB" + CHR(1) + string(irtrno) + CHR(1) + "NC").
    RETURN irtrno.
END FUNCTION. /* FUNCTION fcrb RETURN INTEGER(): */

FUNCTION fz RETURN DECIMAL(c as char):
    /*
        Descripcion: Calcula z
        Estructura Del Mensaje Con Separador chr(1).
            Entrada 1: Número De Dias En Mora
            Entrada 2: Máxima Altura De Mora De Los Ultimos Tres Años
        log: Creada, 22 de Nov de 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR cDiasMra AS CHAR NO-UNDO.
    DEF VAR z AS DECIMAL NO-UNDO.
    DEF VAR iMraMxma AS INTEGER NO-UNDO.
    
    cDiasMra =              ENTRY(1,c,CHR(1)).
    iMraMxma = integer(     ENTRY(2,c,CHR(1))).

    z = -2.779 + fAMB(cDiasMra) * 1.855 + fAMC(cDiasMra) * 3.0205 + fMMB(iMraMxma) * 1.668 + fmmc(imramxma) * 1.7234 + 
        fmmd(iMraMxma) * 5.4605 + fgi() * 0.4960 + fcar() * 0.683 + fcam() * 1.5784 +  fcrb() * 0.2505.
    fccalculos("z" + CHR(1) + string(z) + CHR(1) + "NC").
    RETURN z.
END FUNCTION. /* FUNCTION fz RETURN DECIMAL(): */

FUNCTION fPerdidaEsperada RETURN DECIMAL(iDiasMora AS INTEGER):
    /*
        Descripcion: Calcula La Pérdida Esperada
        log: Creada, 27 de Nov de 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR deVRtrno AS DECIMAL NO-UNDO.
    DEF VAR c AS CHAR NO-UNDO.
    c = FILL(CHR(1),1).                     
    ENTRY(1,c,CHR(1)) = STRING(iDiasMora). /* dias en mora */
    ENTRY(2,c,CHR(1)) = string(fMoraPromedio("3")). /* Maxima altura de mora */
    deVRtrno = fProbabilidadIncumplimiento(fPuntaje(fz(c))) *
                fExposicionActivo() * fPerdidaEsperadaDeValorDelActivo(iDiasMora).
    fccalculos("Pérdida Esperada" + CHR(1) + string(deVrtrno) + CHR(1) + "perdidaesperada").
    PUBLISH "pFinalCalculos"(CCALCULOS).
    RETURN deVrtrno.
END FUNCTION. /* FUNCTION fPerdidaEsperada RETURN DECIMAL(): */


PROCEDURE pDatosCredito:
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    DAFECHADECORTE =                    date(       entry(1,c,chr(1))).
    CTIPOGARANTIASFG =                              entry(2,c,chr(1)).
    ICODIGOTIPODEGARANTIAMODELO =       integer(    entry(3,c,chr(1))).
    ITIPODECREDITO =                    integer(    entry(4,c,chr(1))).
    CNIT =                                          entry(5,c,chr(1)).
    INUMERODECREDITO =                  integer(    entry(6,c,chr(1))).
END PROCEDURE.
