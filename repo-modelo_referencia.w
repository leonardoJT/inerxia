&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}
    DEF VAR hrepo-modelo_refe_auto AS HANDLE NO-UNDO.
    DEF VAR iPriodo AS INTEGER NO-UNDO.
    DEF VAR cTpoGrntia AS CHAR NO-UNDO.
    DEF VAR cCdgoTpoGrntia AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR c AS CHAR NO-UNDO.
    DEF VAR CCALCULOS AS CHAR NO-UNDO.
    DEF VAR dePrddaEsprda AS DECIMAL NO-UNDO.
    DEF VAR cLstaCmpos AS CHAR NO-UNDO.
    DEF VAR cLstaVlres AS CHAR NO-UNDO.
    RUN repo-modelo_refe_auto.p PERSISTENT SET hrepo-modelo_refe_auto.
    SESSION:ADD-SUPER-PROCEDURE(hrepo-modelo_refe_auto).
    /* PARA DETERMINA EL CODIGO DE TIPO DE GARANTIA SEGUN LA TABLA TIPOS DE GARANTIA DEL MODELO DE REFERENCIA */
    ctpogrntia = "Admisible y CodeudorCodeudorCon Codeudor(es)ConyugeConyuge + CodeudorConyuge + Codeudor + PropHipotecaLibranza EstablecidaPrendaSin CodeudorX".
    ccdgotpogrntia = "01111111111".
    /* FIN PARA DETERMINA EL CODIGO DE TIPO DE GARANTIA SEGUN LA TABLA TIPOS DE GARANTIA DEL MODELO DE REFERENCIA */

    {Incluido/Variable.I "SHARED"}
    {Incluido/VARCON.I   "SHARED"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-314 RECT-315 BUTTON-5 BUTTON-1 ~
Btn_Termina 
&Scoped-Define DISPLAYED-OBJECTS daFchaPrcso iAnoCntble iMesCntble 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCCalculos wWin 
FUNCTION fCCalculos RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetRsltdos wWin 
FUNCTION fGetRsltdos RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fMoraPromedio wWin 
FUNCTION fMoraPromedio RETURNS DECIMAL
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fPerdidaEsperada wWin 
FUNCTION fPerdidaEsperada RETURNS DECIMAL
  (iDiasMora AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Termina 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "&Terminar Consulta" 
     SIZE 7 BY 1.62
     FONT 4.

DEFINE BUTTON BUTTON-1 
     LABEL "Procesar" 
     SIZE 10 BY 1.62 TOOLTIP "Continuar - Procesar".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE daFchaPrcso AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha Proceso" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 TOOLTIP "Fecha Aplicación Modelo" NO-UNDO.

DEFINE VARIABLE iAnoCntble AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año Contable" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 TOOLTIP "Año Contable" NO-UNDO.

DEFINE VARIABLE iMesCntble AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Mes Contable" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 TOOLTIP "Mes contable" NO-UNDO.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 6.73.

DEFINE RECTANGLE RECT-315
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29 BY 4.31.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-5 AT ROW 3.69 COL 70.43 WIDGET-ID 24
     daFchaPrcso AT ROW 4.5 COL 36 COLON-ALIGNED WIDGET-ID 30
     BUTTON-1 AT ROW 5.58 COL 70.43 WIDGET-ID 2
     iAnoCntble AT ROW 5.85 COL 36 COLON-ALIGNED WIDGET-ID 32
     iMesCntble AT ROW 7.19 COL 36 COLON-ALIGNED WIDGET-ID 34
     Btn_Termina AT ROW 8.27 COL 72 HELP
          "Termina la consulta del Plan Contable" WIDGET-ID 28
     RECT-314 AT ROW 3.42 COL 69 WIDGET-ID 26
     RECT-315 AT ROW 4.23 COL 23 WIDGET-ID 36
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.57 BY 17
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Aplicación Modelo De Referencia"
         HEIGHT             = 11.69
         WIDTH              = 82.57
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 320
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN daFchaPrcso IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iAnoCntble IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iMesCntble IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Aplicación Modelo De Referencia */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Aplicación Modelo De Referencia */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Termina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Termina wWin
ON CHOOSE OF Btn_Termina IN FRAME fMain /* Terminar Consulta */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Procesar */
DO:
    DEF VAR dafcha1 AS DATE NO-UNDO.
    DEF VAR cPriodos AS CHAR NO-UNDO.
    IF dafchaprcso = ? 
    THEN DO:
        MESSAGE "Indique La Fecha Del Proceso"
            VIEW-AS ALERT-BOX ERROR TITLE "Error".
        APPLY "entry" TO dafchaprcso.
        RETURN NO-APPLY.
    END.
    
    dafcha1 = DATE(MONTH(dafchaprcso),28,YEAR(dafchaprcso)) + 4.
    dafcha1 = DATE(MONTH(dafcha1),1,YEAR(dafcha1)).
    cPriodos = string(YEAR(dafchaprcso) * 100 + MONTH(dafchaprcso)) + "," + string(YEAR(dafcha1) * 100 + MONTH(dafcha1)).
    
    IF ianocntble < YEAR(dafchaprcso) OR ianocntble > YEAR(dafcha1)
    THEN DO:
        MESSAGE "Indique El Año Contable"
            VIEW-AS ALERT-BOX ERROR TITLE "Error".
        APPLY "entry" TO ianocntble.
        RETURN NO-APPLY.
    END.

    IF imescntble < 1 OR imescntble > 12
    THEN DO:
        MESSAGE "Indique El Mes Contable"
            VIEW-AS ALERT-BOX ERROR TITLE "Error".
        APPLY "entry" TO imescntble.
        RETURN NO-APPLY.
    END.

    IF LOOKUP(STRING(iAnoCntble * 100 + iMesCntble),cPriodos,",") = 0
    THEN DO:
        MESSAGE "Indique Período Contable"
            VIEW-AS ALERT-BOX ERROR TITLE "Error".
        APPLY "entry" TO imescntble.
        RETURN NO-APPLY.
    END.

    IF CAN-FIND(FIRST resultados WHERE resultados.fecha = YEAR(dafchaprcso) * 10000 + MONTH(dafchaprcso) * 100 + DAY(dafchaprcso)) 
    THEN DO:
        MESSAGE "Resultados Con Esta Fecha De Proceso, Ya Existen"
            VIEW-AS ALERT-BOX ERROR TITLE "Error".
        RETURN NO-APPLY.
    END.
    PUBLISH "pFechaProceso" (daFchaPrcso).
    /* iPriodo = YEAR(daFchaPrcso) * 100 + MONTH(dafchaprcso). */
    iPriodo = iAnoCntble * 100 + iMesCntble.
    SESSION:SET-WAIT("general").
    DEFINE VAR Listado     AS CHARACTER INITIAL "".
    ASSIGN Listado = W_PathSpl + "-repo-mod-ref" + W_Usuario + ".csv".
    OUTPUT TO value(listado).
    RUN pAplicaModelo.
    OUTPUT CLOSE.
    SESSION:SET-WAIT("").
    MESSAGE "Fin Del Proceso" 
        SKIP(2) "Archivo De Salida: ' " listado "'"
        VIEW-AS ALERT-BOX INFORMATION TITLE "INFORMACION".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 wWin
ON CHOOSE OF BUTTON-5 IN FRAME fMain /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME daFchaPrcso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL daFchaPrcso wWin
ON LEAVE OF daFchaPrcso IN FRAME fMain /* Fecha Proceso */
DO:
    ASSIGN  {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iAnoCntble
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iAnoCntble wWin
ON LEAVE OF iAnoCntble IN FRAME fMain /* Año Contable */
DO:
    ASSIGN  {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iMesCntble
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iMesCntble wWin
ON LEAVE OF iMesCntble IN FRAME fMain /* Mes Contable */
DO:
    ASSIGN  {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY daFchaPrcso iAnoCntble iMesCntble 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-314 RECT-315 BUTTON-5 BUTTON-1 Btn_Termina 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME  {&FRAME-NAME}:
        daFchaPrcso:SCREEN-VALUE = string(TODAY). 
        iAnoCntble:SCREEN-VALUE = string(year(TODAY)). 
        iMesCntble:SCREEN-VALUE = string(MONTH(TODAY)).
        
        daFchaPrcso = TODAY. 
        iAnoCntble = year(TODAY). 
        iMesCntble = MONTH(TODAY).
        SUBSCRIBE "pFinalCalculos" IN hrepo-modelo_refe_auto.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAplicaModelo wWin 
PROCEDURE pAplicaModelo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR iGrntiaMdlo AS INTEGER NO-UNDO.
    aplicamodelo:
    REPEAT TRANSACTION ON ERROR UNDO aplicamodelo, LEAVE aplicamodelo:
        FOR EACH repositorio NO-LOCK
            WHERE
                repositorio.fecha = iPriodo
            AND repositorio.int007 > 0 /* dias atraso */
            /* AND repositorio.numerocredito = 255052 */
            break
                BY repositorio.nit:

            c = FILL(CHR(1),5).
            
            /* FECHA DEL PROCESO */
            entry(1,c,chr(1))       = STRING(daFchaPrcso).                                              
            
            /* GARANTIA SEGUN CODIFICACION SFG */
            entry(2,c,chr(1))       = IF NOT repositorio.char023 = ? THEN repositorio.char023 ELSE "".  
            
            /* CODIGO TIPO DE GARANTIA SEGUN MODELO */
            i = 1.
            IF NOT repositorio.char023 = ? AND NOT trim(repositorio.char023) = ""
            THEN DO:  
                i = lookup(repositorio.char023,ctpogrntia,CHR(1)).
                i = INTEGER(ENTRY(i,ccdgotpogrntia,CHR(1))).
            END.
            iGrntiamdlo = i.
            entry(3,c,chr(1))       = STRING(i).      
    
            /* TIPO DE CREDITO */
            entry(4,c,chr(1))       = IF NOT repositorio.int002 = ? 
                                        THEN STRING(repositorio.int002) 
                                        ELSE "0".                                       
            /* NIT CLINTE */
            ENTRY(5,c,CHR(1))       = IF NOT repositorio.nit = ? 
                                        THEN repositorio.nit 
                                        ELSE "".                                        
            /* NUMERO DEL CREDITO */
            ENTRY(6,c,CHR(1))       = IF NOT repositorio.numerocredito = ? 
                                        THEN STRING(repositorio.numerocredito) 
                                        ELSE "".                                        
            PUBLISH "pDatosCredito"(c).
            CCALCULOS = "".
        /*     DISPLAY repositorio.fecha                                                               */
        /*             repositorio.numerocredito                                                       */
        /*             repositorio.int007                                                              */
        /*             repositorio.int002                                                              */
        /*             fPerdidaEsperada(repositorio.int007 /* dias atraso */) FORMAT ">>>,>>>,>>9.99". */
            dePrddaEsprda = fPerdidaEsperada(repositorio.int007 /* dias atraso */).
            
            IF FIRST(repositorio.nit)
            THEN do:
                PUT UNFORMATTED 
                    "Número Crédito" 
                    ";Nit;Dias Atraso;Tipo De Garantía SFG;Garantía Equivalente Modelo;"
                    REPLACE(ENTRY(1,CCalculos,CHR(2)),CHR(1),";") 
                         SKIP.
                cLstaCmpos = ENTRY(3,CCalculos,CHR(2)).
        /*        PUT UNFORMATTED REPLACE(ENTRY(3,CCalculos,CHR(2)),CHR(1),";") 
                        
                        SKIP. */
            END.
            PUT UNFORMATTED 
                repositorio.numerocredito ";" 
                repositorio.nit ";"
                repositorio.int007 ";"
                repositorio.char023 ";"
                iGrntiamdlo ";"
                REPLACE(ENTRY(2,CCalculos,CHR(2)),CHR(1),";")  
                         SKIP.
            cLstaVlres = ENTRY(2,CCalculos,CHR(2)).
            CREATE resultados.
            resultados.AnoContable                  = iAnoCntble.
            resultados.calificacion                 = fGetRsltdos("calificacion") .
            resultados.CalificacionEquivalente      = fGetRsltdos("calificacionequivalente").
            resultados.ExposicionActivo             = decimal(fGetRsltdos("exposicionactivo")).
            resultados.fecha                        = year(daFchaPrcso) * 10000 + MONTH(dafchaprcso) * 100 + DAY(dafchaprcso).
            resultados.MesContable                  = iMesCntble.
            resultados.Nit                          = repositorio.nit.
            resultados.PerdidaEsperada              = decimal(fGetRsltdos("perdidaesperada")).
            resultados.PerdidaEsperada%             = decimal(fGetRsltdos("perdidaesperada%")).
            resultados.prestamo                     = STRING(repositorio.numerocredito).
            resultados.ProbabilidadIncumplimiento   = decimal(fGetRsltdos("probabilidadincumplimiento")).
        END. /* FOR EACH repositorio NO-LOCK */
        LEAVE aplicamodelo.
    END. /* REPEAT TRANSACTION ON ERROR UNDO aplicamodelo, LEAVE aplicamodelo: */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFinalCalculos wWin 
PROCEDURE pFinalCalculos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    CCALCULOS = c.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCCalculos wWin 
FUNCTION fCCalculos RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER(c).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetRsltdos wWin 
FUNCTION fGetRsltdos RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR i AS INTEGER NO-UNDO.
    i = LOOKUP(c,cLstaCmpos,CHR(1)).
    IF i = 0 THEN RETURN "".
    RETURN ENTRY(i,cLstaVlres,CHR(1)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fMoraPromedio wWin 
FUNCTION fMoraPromedio RETURNS DECIMAL
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER(c).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fPerdidaEsperada wWin 
FUNCTION fPerdidaEsperada RETURNS DECIMAL
  (iDiasMora AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN SUPER(iDiasMora).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

