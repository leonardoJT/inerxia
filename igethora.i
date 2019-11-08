&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD copia Include 
FUNCTION copia RETURN DECIMAL
    (INPUT f1 AS DATE,
     INPUT h1 AS DECIMAL,
     INPUT f2 AS DATE,
     INPUT h2 AS DECIMAL) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCalcTmpoxFec Include 
FUNCTION getCalcTmpoxFec RETURN DECIMAL
    (INPUT f1 AS DATE,
     INPUT h1 AS DECIMAL,
     INPUT f2 AS DATE,
     INPUT h2 AS DECIMAL) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pasa_hora Include 
FUNCTION pasa_hora RETURN DECIMAL
    (INPUT seg AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pasa_seg Include 
FUNCTION pasa_seg RETURNS INTEGER
  ( INPUT hora AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION copia Include 
FUNCTION copia RETURN DECIMAL
    (INPUT f1 AS DATE,
     INPUT h1 AS DECIMAL,
     INPUT f2 AS DATE,
     INPUT h2 AS DECIMAL):

/*------------------------------------------------------------------------------
  Purpose:  retorna el tiempo real entre dos fechas y dos horas, teniendo en 
            cuenta los horarios de jornada laboral y los dias festivos.
    Notes:  tomado por: 
------------------------------------------------------------------------------*/

DEFINE VARIABLE wdtf AS DATE       NO-UNDO. /*variable fecha*/
DEFINE VARIABLE sumar AS LOGICAL    NO-UNDO. /*si dia es laboral, entonces sumar*/
DEFINE VARIABLE wdtothor AS DECIMAL INITIAL 0   NO-UNDO. /*sumatoria total horas*/
DEFINE VARIABLE vdhi AS DECIMAL    NO-UNDO. /*hora inicio jornada laboral*/
DEFINE VARIABLE vdhf AS DECIMAL    NO-UNDO. /*hora finalización jornada laboral*/

IF f1 EQ f2 THEN
    wdtothor = pasa_hora(pasa_seg(h2) - pasa_seg(h1)). /*si las fechas son las mismas*/
ELSE DO:    
    DO wdtf = f1 TO f2: 
        FIND FIRST xxhact_mstr NO-LOCK NO-ERROR.
        CASE WEEKDAY(wdtf) - 1:
            WHEN 1 THEN
                ASSIGN sumar = IF xxhact_mstr.dia1 THEN TRUE ELSE FALSE
                    vdhi = xxhact_mstr.hidia1
                    vdhf = xxhact_mstr.hfdia1.
            WHEN 2 THEN
                ASSIGN sumar = IF xxhact_mstr.dia2 THEN TRUE ELSE FALSE
                    vdhi = xxhact_mstr.hidia2
                    vdhf = xxhact_mstr.hfdia2.
            WHEN 3 THEN
                ASSIGN sumar = IF xxhact_mstr.dia3 THEN TRUE ELSE FALSE
                    vdhi = xxhact_mstr.hidia3
                    vdhf = xxhact_mstr.hfdia3.
            WHEN 4 THEN
                ASSIGN sumar = IF xxhact_mstr.dia4 THEN TRUE ELSE FALSE
                    vdhi = xxhact_mstr.hidia4
                    vdhf = xxhact_mstr.hfdia4.
            WHEN 5 THEN
                ASSIGN sumar = IF xxhact_mstr.dia5 THEN TRUE ELSE FALSE
                    vdhi = xxhact_mstr.hidia5
                    vdhf = xxhact_mstr.hfdia5.
            WHEN 6 THEN
                ASSIGN sumar = IF xxhact_mstr.dia6 THEN TRUE ELSE FALSE
                    vdhi = xxhact_mstr.hidia6
                    vdhf = xxhact_mstr.hfdia6.
            WHEN 7 THEN
                ASSIGN sumar = IF xxhact_mstr.dia7 THEN TRUE ELSE FALSE
                    vdhi = xxhact_mstr.hidia7
                    vdhf = xxhact_mstr.hfdia7.
        END CASE.
        IF wdtf EQ f1 THEN /*primer día*/
            ASSIGN wdtothor = pasa_hora(pasa_seg(vdhf) - pasa_seg(h1)) /*hora final labores del dia - hora 1*/
                sumar = FALSE.
        IF wdtf EQ f2 THEN /*Ultimo día*/
            ASSIGN wdtothor = wdtothor + pasa_hora(pasa_seg(h2) - pasa_seg(vdhi)) /*hora final labores del dia - hora 1*/
                sumar = FALSE.
        IF sumar THEN DO:
            FIND FIRST xxdfes WHERE xxdfes.cdgo_dfes EQ wdtf NO-LOCK NO-ERROR.
            IF NOT AVAILABLE xxdfes THEN DO:
                ASSIGN wdtothor = wdtothor + pasa_hora(pasa_seg(vdhf) - pasa_seg(vdhi)).
            END.
        END.
        ASSIGN sumar = FALSE.
    END.
END.
RETURN (wdtothor).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCalcTmpoxFec Include 
FUNCTION getCalcTmpoxFec RETURN DECIMAL
    (INPUT f1 AS DATE,
     INPUT h1 AS DECIMAL,
     INPUT f2 AS DATE,
     INPUT h2 AS DECIMAL):

/*------------------------------------------------------------------------------
  Purpose:  retorna el tiempo real entre dos fechas y dos horas, teniendo en 
            cuenta los horarios de jornada laboral y los dias festivos.
    Notes:  tomado por: 
------------------------------------------------------------------------------*/

DEFINE VARIABLE wdtf AS DATE       NO-UNDO. /*variable fecha*/
DEFINE VARIABLE sumar AS LOGICAL    NO-UNDO. /*si dia es laboral, entonces sumar*/
DEFINE VARIABLE wdtothor AS INTEGER INITIAL 0   NO-UNDO. /*sumatoria total horas*/
DEFINE VARIABLE vdhi AS DECIMAL    NO-UNDO. /*hora inicio jornada laboral*/
DEFINE VARIABLE vdhf AS DECIMAL    NO-UNDO. /*hora finalización jornada laboral*/

IF f1 EQ f2 THEN
    wdtothor = (pasa_seg(h2) - pasa_seg(h1)). /*si las fechas son las mismas*/
ELSE DO:    
    DO wdtf = f1 TO f2: 
        FIND FIRST xxhact_mstr NO-LOCK NO-ERROR.
        CASE WEEKDAY(wdtf) - 1:
            WHEN 1 THEN
                ASSIGN sumar = IF xxhact_mstr.dia1 THEN TRUE ELSE FALSE
                    vdhi = xxhact_mstr.hidia1
                    vdhf = xxhact_mstr.hfdia1.
            WHEN 2 THEN
                ASSIGN sumar = IF xxhact_mstr.dia2 THEN TRUE ELSE FALSE
                    vdhi = xxhact_mstr.hidia2
                    vdhf = xxhact_mstr.hfdia2.
            WHEN 3 THEN
                ASSIGN sumar = IF xxhact_mstr.dia3 THEN TRUE ELSE FALSE
                    vdhi = xxhact_mstr.hidia3
                    vdhf = xxhact_mstr.hfdia3.
            WHEN 4 THEN
                ASSIGN sumar = IF xxhact_mstr.dia4 THEN TRUE ELSE FALSE
                    vdhi = xxhact_mstr.hidia4
                    vdhf = xxhact_mstr.hfdia4.
            WHEN 5 THEN
                ASSIGN sumar = IF xxhact_mstr.dia5 THEN TRUE ELSE FALSE
                    vdhi = xxhact_mstr.hidia5
                    vdhf = xxhact_mstr.hfdia5.
            WHEN 6 THEN
                ASSIGN sumar = IF xxhact_mstr.dia6 THEN TRUE ELSE FALSE
                    vdhi = xxhact_mstr.hidia6
                    vdhf = xxhact_mstr.hfdia6.
            WHEN 7 THEN
                ASSIGN sumar = IF xxhact_mstr.dia7 THEN TRUE ELSE FALSE
                    vdhi = xxhact_mstr.hidia7
                    vdhf = xxhact_mstr.hfdia7.
        END CASE.
        IF wdtf EQ f1 THEN /*primer día*/
            ASSIGN wdtothor = (pasa_seg(vdhf) - pasa_seg(h1)) /*hora final labores del dia - hora 1*/
                sumar = FALSE.
        IF wdtf EQ f2 THEN /*Ultimo día*/
            ASSIGN wdtothor = wdtothor + (pasa_seg(h2) - pasa_seg(vdhi)) /*hora final labores del dia - hora 1*/
                sumar = FALSE.
        IF sumar THEN DO:
            FIND FIRST xxdfes WHERE xxdfes.cdgo_dfes EQ wdtf NO-LOCK NO-ERROR.
            IF NOT AVAILABLE xxdfes THEN DO:
                ASSIGN wdtothor = wdtothor + (pasa_seg(vdhf) - pasa_seg(vdhi)).
            END.
        END.
        ASSIGN sumar = FALSE.
    END.
END.
RETURN pasa_hora(wdtothor).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pasa_hora Include 
FUNCTION pasa_hora RETURN DECIMAL
    (INPUT seg AS INTEGER):
    DEFINE VARIABLE horas AS DECIMAL    NO-UNDO.
    ASSIGN 
        horas = TRUNCATE(seg / 3600,0).
    ASSIGN horas = horas + ((seg - (horas * 3600)) / 6000).
    RETURN horas.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pasa_seg Include 
FUNCTION pasa_seg RETURNS INTEGER
  ( INPUT hora AS DECIMAL ) :
    DEFINE VARIABLE tseg AS INTEGER  NO-UNDO.
    DEFINE VARIABLE Wint    AS INTEGER  NO-UNDO.
    DEFINE VARIABLE Wdec    AS INTEGER  NO-UNDO.

    ASSIGN     
        Wint = INTEGER(TRUNCATE(hora,0))
        Wdec = INTEGER((hora - Wint) * 100)
        tseg = Wint * 3600 + Wdec * 60.
    RETURN tseg.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

