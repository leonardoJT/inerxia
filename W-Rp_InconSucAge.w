&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
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


 {Incluido/Variable.i "SHARED"}
        
 DEFINE TEMP-TABLE ComAge
     FIELD age LIKE agencias.agencia
     FIELD cue LIKE cuentas.cuenta
     FIELD nit LIKE clientes.nit
     FIELD Ndc LIKE mov_contable.num_documento   
     FIELD doc LIKE mov_contable.doc_referencia
     FIELD fec LIKE mov_contable.fec_contable
     FIELD Com LIKE mov_contable.comentario
     FIELD deb LIKE mov_contable.db
     FIELD cre LIKE mov_contable.cr
     FIELD Usu LIKE Usuarios.Usuario.
DEFINE VAR W_Ok AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FInco

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb_Agencias RCtas FecWk1 FecWk2 BUTTON-1 ~
BtnDone RECT-1 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Agencias RCtas FecWk1 FecWk2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 15 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 1" 
     SIZE 15 BY 1.62.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FecWk1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FecWk2 AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RCtas AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Cuenta 1904", 1904,
"Cuenta 2705", 2705
     SIZE 25 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 1.35.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FInco
     Cmb_Agencias AT ROW 1.54 COL 9 COLON-ALIGNED
     RCtas AT ROW 1.54 COL 53 NO-LABEL
     FecWk1 AT ROW 2.62 COL 37 COLON-ALIGNED
     FecWk2 AT ROW 3.69 COL 37 COLON-ALIGNED
     BUTTON-1 AT ROW 3.15 COL 58
     BtnDone AT ROW 5.04 COL 58
     RECT-1 AT ROW 1.27 COL 52
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 78.29 BY 6.27
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON BtnDone.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Documentos Inconsistentes de Sucursales y Agencias"
         HEIGHT             = 6.27
         WIDTH              = 78.29
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* SETTINGS FOR FRAME FInco
   FRAME-NAME Custom                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Documentos Inconsistentes de Sucursales y Agencias */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Documentos Inconsistentes de Sucursales y Agencias */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME FInco /* Salir */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME FInco /* Button 1 */
DO:
  FOR EACH Comage: DELETE Comage. END.
  ASSIGN FRAME Finco Cmb_Agencias FecWk1 FecWk2 RCtas.
  DEFINE VAR Listado AS CHARACTER INITIAL "".    
  listado = W_PathSpl + "Inconsistencias.Lst".
  {Incluido\Imprimir.I "listado"}
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
  DISPLAY Cmb_Agencias RCtas FecWk1 FecWk2 
      WITH FRAME FInco IN WINDOW wWin.
  ENABLE Cmb_Agencias RCtas FecWk1 FecWk2 BUTTON-1 BtnDone RECT-1 
      WITH FRAME FInco IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FInco}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
MESSAGE "Opción no disponible" VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.
  FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK BREAK BY Agencias.Agencia:
      W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME Finco.
      IF FIRST-OF(Agencias.Agencia) THEN
         Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}

.W_Reporte    = "REPORTE   : INCONSISTENCIAS SUCURSALES Y AGENCIAS - Age: " + TRIM(Cmb_Agencias) + "  : Cuentas " + STRING(RCtas,"9999") + " / " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
W_EncColumna = "Age Nit          Cuenta        Usu   Comentario                             Docto          Fecha              Debito          Credito".

VIEW FRAME F-Encabezado.
VIEW FRAME F-Ftr.

DEFINE VAR x AS DECIMAL FORMAT ">>,>>9".

FOR EACH Mov_Contable WHERE Mov_Contable.Agencia = INTEGER(SUBSTRING(Cmb_Agencias,1,3))
                        AND Mov_Contable.Cuenta BEGINS STRING(RCtas,"9999")
                        AND Mov_Contable.Nit < "011"
                        AND Mov_Contable.Nit <> STRING(Mov_Contable.Agencia,"999")
                        AND Mov_Contable.Fec_Contable >= FecWk1
                        AND Mov_Contable.Fec_Contable <= FecWk2 NO-LOCK BREAK BY Mov_Contable.Fec_Contable:
    FIND Comage WHERE Comage.Age = Mov_Contable.Agencia
                  AND Comage.Cue = Mov_Contable.Cuenta
                  AND Comage.Nit = Mov_Contable.Nit
                  AND Comage.Ndc = Mov_Contable.Num_Documento
                  AND Comage.Doc = Mov_Contable.Doc_Referencia
                  AND Comage.Fec = Mov_Contable.Fec_Contable NO-ERROR.
    IF NOT AVAILABLE Comage THEN DO:
        CREATE Comage NO-ERROR.
        Comage.Age = Mov_Contable.Agencia.
        Comage.Cue = Mov_Contable.Cuenta.
        Comage.Nit = STRING(Mov_Contable.Nit,"999").
        Comage.Ndc = Mov_Contable.Num_Documento.
        Comage.Doc = Mov_Contable.Doc_Referencia.
        Comage.Fec = Mov_Contable.Fec_Contable.
        Comage.Com = Mov_Contable.Comentario.
        Comage.Deb = Mov_Contable.Db.
        Comage.Cre = Mov_Contable.Cr.
        Comage.Usu = Mov_Contable.Usuario.
    END.
END.

FOR EACH Mov_Contable2 WHERE Mov_Contable2.Agencia = INTEGER(SUBSTRING(Cmb_Agencias,1,3))
                         AND Mov_Contable2.Cuenta BEGINS STRING(RCtas,"9999")
                         AND Mov_Contable2.cliente_id < "011"
                         AND Mov_Contable2.cliente_id <> STRING(Mov_Contable.Agencia,"999")
                         AND Mov_Contable2.Fec_Contable >= FecWk1
                         AND Mov_Contable2.Fec_Contable <= FecWk2 NO-LOCK BREAK BY Mov_Contable2.Fec_Contable:
    FIND Comage WHERE Comage.Age = Mov_Contable2.Agencia
                  AND Comage.Cue = Mov_Contable2.Cuenta
                  AND Comage.Nit = Mov_Contable2.cliente_id
                  AND Comage.Ndc = Mov_Contable2.Num_Documento
                  AND Comage.Doc = Mov_Contable2.Doc_Referencia
                  AND Comage.Fec = Mov_Contable2.Fec_Contable NO-ERROR.
    IF NOT AVAILABLE Comage THEN DO:
        CREATE Comage NO-ERROR.
        Comage.Age = Mov_Contable2.Agencia.
        Comage.Cue = Mov_Contable2.Cuenta.
        Comage.Nit = STRING(Mov_Contable2.cliente_id,"999").
        Comage.Ndc = Mov_Contable2.Num_Documento.
        Comage.Doc = Mov_Contable2.Doc_Referencia.
        Comage.Fec = Mov_Contable2.Fec_Contable.
        Comage.Com = Mov_Contable2.Comentario.
        Comage.Deb = Mov_Contable2.Db.
        Comage.Cre = Mov_Contable2.Cr.
        Comage.Usu = Mov_Contable2.Usuario.
    END.
END.

FOR EACH Comage:
    IF Comage.Deb GT 0 THEN 
       FIND FIRST Mov_Contable     WHERE
         Mov_contable.Agencia      EQ INTEGER(STRING(Comage.Nit,"999")) AND
         Mov_contable.Cuenta       EQ Comage.Cue AND
         /*Mov_contable.Fec_contable GE FecWk1 AND
         Mov_contable.Fec_contable LE FecWk2 AND            */
         Mov_contable.Nit          EQ STRING(Comage.Age,"999") AND 
         Mov_contable.Cr           EQ Comage.Deb NO-LOCK NO-ERROR.
    ELSE
       FIND FIRST Mov_Contable     WHERE
         Mov_contable.Agencia      EQ INTEGER(STRING(Comage.Nit,"999")) AND
         Mov_contable.Cuenta       EQ Comage.Cue               AND
         /*Mov_contable.Fec_contable GE FecWk1 AND
         Mov_contable.Fec_contable LE FecWk2 AND            */
         Mov_contable.Nit          EQ STRING(Comage.Age,"999") AND 
         Mov_contable.Db           EQ Comage.Cre NO-LOCK NO-ERROR.

    IF NOT AVAILABLE Mov_Contable THEN DO:
       DISPLAY Comage.Age Comage.Nit Comage.Cue Comage.Usu Comage.Com FORMAT "X(40)" Comage.doc Comage.fec Comage.Deb Comage.Cre
       WITH FRAME FF WIDTH 180 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
    END.
END.
PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

