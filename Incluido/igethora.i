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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCalcTmpoxFec Include 
FUNCTION getCalcTmpoxFec RETURN DECIMAL
    (INPUT f1 AS DATE,
     INPUT h1 AS INTEGER,
     INPUT f2 AS DATE,
     INPUT h2 AS INTEGER) FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCalcTmpoxFec Include 
FUNCTION getCalcTmpoxFec RETURN DECIMAL
    (INPUT f1 AS DATE,
     INPUT h1 AS INTEGER,
     INPUT f2 AS DATE,
     INPUT h2 AS INTEGER):

/*------------------------------------------------------------------------------
  Purpose:  retorna el tiempo real entre dos fechas y dos horas
    Notes:  tomado por: 
------------------------------------------------------------------------------*/

DEFINE VARIABLE wdtf AS DATE       NO-UNDO. /*variable fecha*/
DEFINE VARIABLE sumar AS LOGICAL    NO-UNDO. /*si dia es laboral, entonces sumar*/
DEFINE VARIABLE wdtothor AS INTEGER INITIAL 0   NO-UNDO. /*sumatoria total horas*/
DEFINE VARIABLE vdhi AS DECIMAL INITIAL 0.01  NO-UNDO. /*hora inicio día*/
DEFINE VARIABLE vdhf AS DECIMAL INITIAL 24 NO-UNDO. /*hora finalización día*/

IF f1 EQ f2 THEN DO:
    wdtothor = h2 - h1. /*si las fechas son las mismas*/
    MESSAGE "1"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
ELSE DO:    
    MESSAGE "2"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    DO wdtf = f1 TO f2: 
        ASSIGN sumar = TRUE.
        IF wdtf EQ f1 THEN /*primer día*/
            ASSIGN wdtothor = vdhf - h1 /*hora final del dia - hora 1*/
                sumar = FALSE.
        IF wdtf EQ f2 THEN /*Ultimo día*/
            ASSIGN wdtothor = wdtothor + (h2 - vdhi) /*hora final del dia - hora 1*/
                sumar = FALSE.
        IF sumar THEN DO:
            ASSIGN wdtothor = wdtothor + (vdhf - vdhi).
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

