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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dia_fin_mes Include 
FUNCTION dia_fin_mes RETURNS DATE
  ( INPUT pdtfecha AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dia_ini_mes Include 
FUNCTION dia_ini_mes RETURNS DATE
  ( INPUT pdtfecha AS DATE )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dia_fin_mes Include 
FUNCTION dia_fin_mes RETURNS DATE
  ( INPUT pdtfecha AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  Devuelve la fecha con el último dia del mes
    Notes:  
------------------------------------------------------------------------------*/

DEFINE VARIABLE vdtfin AS DATE       NO-UNDO.
DEFINE VARIABLE vimesi AS INTEGER  NO-UNDO.
DEFINE VARIABLE vidiai AS INTEGER  NO-UNDO.
DEFINE VARIABLE vianoi AS INTEGER  NO-UNDO.
DEFINE VARIABLE vcdias AS CHARACTER EXTENT 12 FORMAT "XX" INITIAL 
    ["31","28","31","30","31","30","31","31","30","31","30","31"] NO-UNDO.


ASSIGN vimesi = IF (MONTH(pdtfecha) - 1) <= 0 THEN 12 ELSE (MONTH(pdtfecha) - 1).
    vianoi = IF vimesi EQ 12 THEN YEAR(pdtfecha) - 1 ELSE YEAR(pdtfecha).
    vidiai = IF vimesi NE 2 THEN INT(vcdias[vimesi]) 
            ELSE 
                (IF (vianoi MODULO 4) EQ 0 THEN 29 ELSE 28).
    vdtfin = DATE(vimesi,vidiai,vianoi).

  RETURN vdtfin.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dia_ini_mes Include 
FUNCTION dia_ini_mes RETURNS DATE
  ( INPUT pdtfecha AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  Devuelve la fecha con el primer dia del mes
    Notes:  
------------------------------------------------------------------------------*/

DEFINE VARIABLE vdtfin AS DATE       NO-UNDO.
DEFINE VARIABLE vimesi AS INTEGER  NO-UNDO.
DEFINE VARIABLE vidiai AS INTEGER  NO-UNDO.
DEFINE VARIABLE vianoi AS INTEGER  NO-UNDO.
DEFINE VARIABLE vcdias AS CHARACTER EXTENT 12 FORMAT "XX" INITIAL 
    ["31","28","31","30","31","30","31","31","30","31","30","31"] NO-UNDO.


ASSIGN vimesi = IF (MONTH(pdtfecha) - 1) <= 0 THEN 12 ELSE (MONTH(pdtfecha) - 1).
    vianoi = IF vimesi EQ 12 THEN YEAR(pdtfecha) - 1 ELSE YEAR(pdtfecha).
    vidiai = 1.
    vdtfin = DATE(vimesi,vidiai,vianoi).

  RETURN vdtfin.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

