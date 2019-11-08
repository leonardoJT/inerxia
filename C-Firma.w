&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WIN_Firma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WIN_Firma 
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
DEFINE INPUT PARAMETER Nit_Firma LIKE Clientes.Nit.
DEFINE INPUT PARAMETER Cod_Prod  LIKE Ahorros.Cod_Ahorro.
DEFINE INPUT PARAMETER Cue_Prod  LIKE Ahorros.Cue_Ahorros.
DEFINE INPUT PARAMETER Opcion    AS INTEGER FORMAT "9".

/*DEFINE VARIABLE nit_firma LIKE clientes.nit INITIAL "70383330".*/

/*Opcion: 0 - Muestra solo la firma de Nit_Firma
          1 - Nit_Firma y sus Familiares
          2 - Nit_Firma y sus referencias comerciales
          3 - Nit_Firma y sus tutores
          4 - ...
          7 - Nit_Firma y sus Autorizados de ahorro
          Mayor que cero se busca en la tabla de relaciones. teniendo en cuenta
          la configuracion dada en CF_Varios. */


DEFINE VARIABLE W_Ok   AS LOGICAL.
DEFINE VARIABLE gTexto AS CHARACTER FORMAT "X(80)".

DEFINE VAR Imagen AS CHARACTER FORMAT "X(14)".

DEFINE TEMP-TABLE Tmp
    FIELD Nit LIKE Clientes.Nit
    FIELD Rel LIKE Relaciones.Cod_Relacion
    FIELD Nom AS CHARACTER FORMAT "X(50)"
    FIELD Ced LIKE Clientes.Nit
    FIELD Fec AS DATE
    FIELD Des AS CHARACTER FORMAT "X(30)"
    FIELD Idf AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Firma

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Firma 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WIN_Firma AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-WIN_Firma 
       MENU-ITEM m_Anverso      LABEL "Anverso"       
       MENU-ITEM m_Reverso      LABEL "Reverso"       
       MENU-ITEM m_Salir        LABEL "Salir"         .


/* Definitions of the field level widgets                               */
DEFINE IMAGE Firma
     FILENAME "firmas/0.gif":U
     SIZE 140 BY 17.23.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Firma
     Firma AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 140.86 BY 17.38
         BGCOLOR 17 .


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
  CREATE WINDOW WIN_Firma ASSIGN
         HIDDEN             = YES
         TITLE              = "Firma"
         HEIGHT             = 17.38
         WIDTH              = 140.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         POPUP-MENU         = MENU POPUP-MENU-WIN_Firma:HANDLE
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:POPUP-MENU = MENU POPUP-MENU-WIN_Firma:HANDLE.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT WIN_Firma:LOAD-ICON("imagenes/desktop.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/desktop.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB WIN_Firma 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WIN_Firma
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Firma
                                                                        */
ASSIGN 
       FRAME F_Firma:MOVABLE          = TRUE
       FRAME F_Firma:RESIZABLE        = TRUE.

ASSIGN 
       Firma:RESIZABLE IN FRAME F_Firma        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WIN_Firma)
THEN WIN_Firma:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME WIN_Firma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WIN_Firma WIN_Firma
ON END-ERROR OF WIN_Firma /* Firma */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WIN_Firma WIN_Firma
ON WINDOW-CLOSE OF WIN_Firma /* Firma */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Anverso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Anverso WIN_Firma
ON CHOOSE OF MENU-ITEM m_Anverso /* Anverso */
DO:
/*DO WITH FRAME F_Firma:
  IF Cod_Prod LT 10 THEN
     imagen = STRING(Cod_Prod,"9") + "_" + TRIM(STRING(Cue_Prod)) + "_" + "A" + ".jpg".
  ELSE
     imagen = STRING(Cod_Prod,"99") + "_" + TRIM(STRING(Cue_Prod)) + "_" + "A" + ".jpg".
  gTexto = SEARCH("\\172.28.1.201\d\sicobel\firmas\" + imagen).
  IF gTexto EQ ? THEN
     ASSIGN W_Ok  = Firma:LOAD-IMAGE("imagenes\Fondosincuenta.jpg") NO-ERROR.
  ELSE 
     ASSIGN W_Ok = Firma:LOAD-IMAGE(gTexto) NO-ERROR.
END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Reverso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Reverso WIN_Firma
ON CHOOSE OF MENU-ITEM m_Reverso /* Reverso */
DO:
/*DO WITH FRAME F_Firma:
  IF Cod_Prod LT 10 THEN
     imagen = STRING(Cod_Prod,"9") + "_" + TRIM(STRING(Cue_Prod)) + "_" + "R" + ".jpg".
  ELSE
     imagen = STRING(Cod_Prod,"99") + "_" + TRIM(STRING(Cue_Prod)) + "_" + "R" + ".jpg".
  gTexto = SEARCH("\\172.28.1.201\d\sicobel\firmas\" + imagen).
  IF gTexto EQ ? THEN
     ASSIGN W_Ok                 = Firma:LOAD-IMAGE("imagenes\Fondosincuenta.jpg") NO-ERROR.
  ELSE 
     ASSIGN W_Ok = Firma:LOAD-IMAGE(gTexto) NO-ERROR.
END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Salir WIN_Firma
ON CHOOSE OF MENU-ITEM m_Salir /* Salir */
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WIN_Firma 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects WIN_Firma  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WIN_Firma  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WIN_Firma)
  THEN DELETE WIDGET WIN_Firma.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WIN_Firma  _DEFAULT-ENABLE
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
  ENABLE Firma 
      WITH FRAME F_Firma IN WINDOW WIN_Firma.
  {&OPEN-BROWSERS-IN-QUERY-F_Firma}
  VIEW WIN_Firma.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject WIN_Firma 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject WIN_Firma 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  Win_Firma:TITLE = "Firma del Titular. (Presione el clic derecho para accesar el menú)".
  RUN SUPER.
  
DO WITH FRAME F_Firma:
  /*IF Cod_Prod LT 10 THEN               /* ANTES STRING(Cue_Prod,"99999" */
     imagen = STRING(Cod_Prod,"9") + "_" + TRIM(STRING(Cue_Prod)) + "_" + "A" + ".gif".
  ELSE
     imagen = STRING(Cod_Prod,"99") + "_" + TRIM(STRING(Cue_Prod)) + "_" + "A" + ".gif".*/

  imagen = STRING(Nit_Firma) + ".gif".
  

  gTexto = SEARCH(/*"\\10.1.0.100\objetos\*/"firmas\" + imagen) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN MESSAGE "er1".

  IF gTexto EQ ? THEN DO:
     ASSIGN W_Ok = Firma:LOAD-IMAGE("imagenes\Fondosincuenta.jpg") NO-ERROR.
     IF ERROR-STATUS:ERROR THEN MESSAGE "er2".
  END.
  ELSE DO:
      ASSIGN W_Ok = Firma:LOAD-IMAGE(gTexto) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN MESSAGE "er3".
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

