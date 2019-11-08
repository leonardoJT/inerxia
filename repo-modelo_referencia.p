/*
    Nombre:         repo-modelo_referencia.p
    Descripción:    Modelo De Referencia, Programado De Acuerdo A Las Especificaciones Entregadas Por Santiago
    Log:            Creado, 22 nov 2007, Ing Edilberto Mariño Moya.
*/    
DEF VAR e AS DECIMAL NO-UNDO INITIAL 2.718281.
DEF VAR DAFECHADECORTE AS DATE NO-UNDO.

DAFECHADECORTE = TODAY.

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



FUNCTION fDiasEnMora RETURN CHAR(c AS CHAR):
    /*
        Descripción: Devuelve El Número De Días En Mora De Un Crédito
        Estructura Del Mensaje De Entrada Con Separador chr(1):
            Entrada 1: Número Del Crédito
            Entrada 2: Fecha Del Proceso, Para Calcular La Mora
        Estructura Del Mensaje De Salida con Separador chr(1):
            Entrada 1: Número Días En Mora
        Log: Creada 22 de nov de 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR dafcha AS DATE NO-UNDO.
    DEF VAR iNmroCrdto AS INTEGER NO-UNDO.
    DEF VAR iDiasAtrzo AS INTEGER NO-UNDO.
    DEF VAR cRtrno AS CHAR NO-UNDO.
    c = c + CHR(1). /* Para Asegurar Por Lo Menos Dos Entradas En El Mensaje */
    dafcha = TODAY.
    dafcha = DATE(ENTRY(1,c,CHR(1))) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN dafcha = TODAY.
    IF dafcha = ? THEN dafcha = TODAY.
    iNmroCrdto = integer(ENTRY(2,c,CHR(1))).
    iDiasAtrzo = 0.
    FOR EACH creditos NO-LOCK
        WHERE
            creditos.num_credito = iNmroCrdto:
        idiasatrzo = creditos.dias_atraso.
        LEAVE.
    END.

    cRtrno = STRING(iDiasAtrzo).
    RETURN crtrno.
END FUNCTION. /* FUNCTION fDiasEnMora RETURN CHAR(c AS CHAR) */

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
    RETURN 1 / (1 + EXP(e,dez)).
END FUNCTION. /* FUNCTION fpuntaje RETURN DECIMAL(dez AS DECIMAL): */


FUNCTION fAMB RETURN INTEGER(c AS CHAR):
    /*
        Descripción: Altura De Mora Actual entre 31 y 60 dias. Devuelve UNO si la mora actual cae en el rango nombrado
        Estructura Del Mensaje De Entrada Con Separador chr(1):
            Entrada 1: Dias De Mora Actual
        log: Creada, 22 de nov de 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR iMraActual AS INTEGER NO-UNDO.
    iMraActual = INTEGER(ENTRY(1,c,CHR(1))) NO-ERROR.
    IF iMraActual >= 31 AND iMraActual <= 60
    THEN RETURN 1.
    ELSE RETURN 0.
END FUNCTION. /* FUNCTION fAMB RETURN DECIMAL(): */

FUNCTION fAMC RETURN INTEGER(c AS CHAR):
    /*
        Descripcion: Altura De Mora Actual Entre 61 y 90 dias. Devuelve UNO si la mora actual cae en el rango nombrado
        Estructura Del Mensaje De Entrada Con Separador chr(1):
        Log: Creada, 22 De Noviembre De 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR iMraActual AS INTEGER NO-UNDO.
    iMraActual = INTEGER(ENTRY(1,c,CHR(1))) NO-ERROR.
    IF iMraActual >= 61 AND iMraActual <= 90
    THEN RETURN 1.
    ELSE RETURN 0.
END FUNCTION. /* FUNCTION fAMC RETURN DECIMAL(c AS CHAR): */


FUNCTION fMMB RETURN INTEGER(c AS CHAR):
    /*
        Descripción: Máxima Altura De Mora Entre 31 y 60 dias
        Estructura Del Mensaje De Entrada Con Separador chr(1)
            Entrada 1: Número Del Crédito
        log: Creada, 23 De Nov De 2007,Ing. Edilberto Mariño Moya
    */
    DEF VAR iMraMxma AS INTEGER NO-UNDO.
    DEF VAR iNmroCrdto AS INTEGER NO-UNDO.
    iNmroCrdto = integer(ENTRY(1,c,CHR(1))).
    DEF VAR iTpoCrdto AS INTEGER NO-UNDO.
    
    /* Determina Tipo Credito */
    FOR EACH creditos FIELD (tip_credito) NO-LOCK
        WHERE
            creditos.num_credito = iNmroCrdto:
        iTpoCrdto = creditos.tip_credito.
    END.

    iMraMxma = 0.
    iMraMxma = INTEGER(ENTRY(1,c,CHR(1))) NO-ERROR.
    IF iMraMxma >= 31 AND iMraMxma <= 60 
    THEN RETURN 1.
    ELSE RETURN 0.
END FUNCTION. /* FUNCTION fMMB RETURN INTEGER(c AS CHAR): */

FUNCTION fMMC RETURN INTEGER(c AS CHAR):
    /*
        Descripcion: Máxima Altura De Mora Entre 61 y 90 Dias
        Estructura Del Mesaje De Entrada Con Separador chr(1)
            Entrada 1: Máxima Altura De Mora
        lOG: Creada, 22 De Nov De 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR iMraMxma AS INTEGER NO-UNDO.
    iMraMxma = INTEGER(ENTRY(1,c,CHR(1))) NO-ERROR.
    IF iMraMxma >= 61 AND iMraMxma <= 90
    THEN RETURN 1.
    ELSE RETURN 0.
END FUNCTION. /* FUNCTION fMMC RETURN INTEGER(c AS CHAR): */

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
    RETURN dafcha1.
END FUNCTION.

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
    RETURN 0.
END FUNCTION. /* FUNCTION fCAR RETURN INTEGER(): */

FUNCTION fCAM RETURN INTEGER():
    /*
        Descripcion: La Variable CAM Se Puede Ignorar Debido A La Falta De Datos De Los Ultimos Tres Años. Por Lo Tanto Es Cero
        Log: Creada, 26 De Noviembre De 2007, Ing. Edilberto Mariño Moya
    */
    RETURN 0.
END FUNCTION. /* FUNCTION fCAM RETURN integr(): */

FUNCTION fProbabilidadIncumplimiento RETURN DECIMAL(dePuntaje AS DECIMAL,TipoCredito AS INTEGER):
    /*
        Descripcion: Encuentra La Probabilidad De Incumplimiento Con Base En El Puntaje
        log: Creada: 26 de nov de 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR dePI AS DECIMAL NO-UNDO.
    FOR FIRST tpuntaje 
        WHERE
            tpuntaje.puntaje >= depuntaje
        AND tpuntaje.tpo_crdto = tipocredito:
        dePI = tpuntaje.probabilidadIncumplimiento.
    END.
    RETURN dePI.
END FUNCTION. /* FUNCTION fProbabilidadIncumplimiento RETURN DECIMAL(dePuntaje AS DECIMAL): */

FUNCTION fPerdidaEsperadaDeValorDelActivo RETURN DECIMAL(iDiasMora AS INTEGER,iTipoGarantia AS INTEGER):
    /*
        Descripción: Devuelve El % De Incumplimiento Dados Los Dias De Mora Y El Tipo De Garantía
        Log: Creada, 26 nov de 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR deVRtrno AS DECIMAL NO-UNDO.
    FOR FIRST tpevadi
        WHERE
            tpevadi.DiasDespuesIncumplimiento >= iDiasMora
        AND tpevadi.TipoGarantia = iTipoGarantia:
        deVRtrno = tpevadi.pdi.
    END.
    RETURN deVrtrno.
END FUNCTION. /* FUNCTION fPerdidaEsperadaDeValorDelActivo RETURN DECIMAL(iDiasMora AS INTEGER,iTipoGarantia AS INTEGER): */


FUNCTION fExposicionActivo RETURN DECIMAL():
    /*
        Descripcion: Devuelve La Exposición Del Activo, Es Decir El Saldo De La Deuda:
            Saldo Capital, Intereses y Cuentas X Cobrar
        log: Creada, 27 De Nov De 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR deVrtrno AS DECIMAL NO-UNDO.
    deVrtrno = 0529. /* LA SUMA SE DEBE AJUSTAR DE ACUERDO CON LAS VARIABLES COMO QUEDEN DEFINIDAS EN EN REPOSITORIO */
    RETURN devrtrno.
END FUNCTION. /* FUNCTION fExpscionActvo RETURN DECIMAL(): */
            
FUNCTION fmmd RETURN INTEGER(c AS CHAR):
    /*
        Descripcion: Máxima Altura De Mora mayor de 90 Dias
        Estructura Del Mesaje De Entrada Con Separador chr(1)
            Entrada 1: Máxima Altura De Mora
        lOG: Creada, 27 De Nov De 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR iMraMxma AS INTEGER NO-UNDO.
    iMraMxma = INTEGER(ENTRY(1,c,CHR(1))) NO-ERROR.
    IF iMraMxma > 90
    THEN RETURN 1.
    ELSE RETURN 0.
END FUNCTION.

FUNCTION fgi RETURN INTEGER(iTipoGarantia AS INTEGER):
    /*
        Descripcion: Dado El Tipo De Garantía, Devuelve Un Uno o Un cero
        log: Creada 27 de nov de 2007, Ing. Edilberto Mariño Moya
    */
    /* OJO: COMO SE DEFINE UNA GARANTIA IDONEA */
    RETURN 0.
END FUNCTION. /*  FUNCTION fgi RETURN INTEGER(iTipoGarantia): */

FUNCTION fcrb RETURN INTEGER():
    /*
        Descripcion: Créditos Acivos. Devuelve 1 si el cliente tiene otros credito de consumo activos diferentes al del segmento.
        Log: Creada 27 de nov de 2007, ing. Edilberto Mariño Moya
    */
    /* OJO FALTA CODIGO */
    RETURN 0.
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
    cDiasMra = ENTRY(1,c,CHR(1)).
    DEF VAR z AS DECIMAL NO-UNDO.
    DEF VAR cMraMxma AS CHAR NO-UNDO.
    cMraMxma = ENTRY(2,c,CHR(1)).
    DEF VAR iTipoGarantia AS INTEGER NO-UNDO.
    iTipoGarantia = integer(ENTRY(3,c,CHR(1))).
    z = -2.779 + fAMB(cDiasMra) * 1.855 + fAMC(cDiasMra) * 3.0205 + fMMB(cMraMxma) * 1.668 + fmmc(STRING(cmramxma)) * 1.7234 + 
        fmmd(cMraMxma) * 5.4605 + fgi(itipogarantia) * 0.4960 + /* fcar() * 0.683 + fcam() * 1.5784 + */ fcrb() * 0.2505.
    RETURN z.
END FUNCTION. /* FUNCTION fz RETURN DECIMAL(): */

FUNCTION fPerdidaEsperada RETURN DECIMAL(iTipoCredito AS INTEGER,iDiasMora AS INTEGER,iTipoGarantia AS INTEGER):
    /*
        Descripcion: Calcula La Pérdida Esperada
        log: Creada, 27 de Nov de 2007, Ing. Edilberto Mariño Moya
    */
    DEF VAR deVRtrno AS DECIMAL NO-UNDO.
    DEF VAR c AS CHAR NO-UNDO.
    c = CHR(1) + CHR(1).
    ENTRY(1,c,CHR(1)) = STRING(iDiasMora). /* dias en mora */
    ENTRY(2,c,CHR(1)) = "0". /* Maxima altura de mora, ultimos tres años. Cero porque o existe historia */
    ENTRY(3,c,CHR(1)) = "1". /* Tipo De Garantia */
    deVRtrno = fProbabilidadIncumplimiento(fPuntaje(fz(c)),iTipoCredito) *
                fExposicionActivo() * fPerdidaEsperadaDeValorDelActivo(iDiasMora,iTipoGarantia).
    RETURN deVrtrno.
END FUNCTION. /* FUNCTION fPerdidaEsperada RETURN DECIMAL(): */


MESSAGE fPerdidaEsperada(1,180,1) VIEW-AS ALERT-BOX.
