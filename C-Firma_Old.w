&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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
&Scoped-Define ENABLED-OBJECTS BUTTON-117 Btn_Anterior Btn_Siguiente Firma ~
RECT-227 
&Scoped-Define DISPLAYED-OBJECTS W_Msaje W_Ced F_Nombre F_Descripcion 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WIN_Firma AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Anterior 
     IMAGE-UP FILE "imagenes/fizq.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/fizq.bmp":U
     LABEL "Button 122" 
     SIZE 5 BY 1.08.

DEFINE BUTTON Btn_Siguiente 
     IMAGE-UP FILE "imagenes/fder.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/fder.bmp":U
     LABEL "Button 121" 
     SIZE 5 BY 1.08.

DEFINE BUTTON BUTTON-117 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 117" 
     SIZE 10 BY 2.69.

DEFINE VARIABLE F_Descripcion AS CHARACTER FORMAT "X(30)":U 
     LABEL "Descripción" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE F_Nombre AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_Ced AS CHARACTER FORMAT "X(12)":U 
     LABEL "Cèdula" 
     VIEW-AS FILL-IN 
     SIZE 19.72 BY .96
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Msaje AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE IMAGE Firma
     FILENAME "C:/DESARROLLO/BELEN/Prg/Imagenes/FIRMAS/0.jpg":U
     SIZE 76 BY 12.12.

DEFINE RECTANGLE RECT-227
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46 BY 4.88.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Firma
     W_Msaje AT ROW 13.92 COL 13.14 NO-LABEL
     W_Ced AT ROW 16.08 COL 25 COLON-ALIGNED
     BUTTON-117 AT ROW 17.04 COL 60
     F_Nombre AT ROW 17.15 COL 14 NO-LABEL
     Btn_Anterior AT ROW 17.15 COL 47
     Btn_Siguiente AT ROW 17.15 COL 52
     F_Descripcion AT ROW 18.23 COL 25 COLON-ALIGNED
     Firma AT ROW 1.27 COL 2
     RECT-227 AT ROW 14.85 COL 13
     "Haga clic en las flechas para desplazarse por las firmas asociadas" VIEW-AS TEXT
          SIZE 45 BY .81 AT ROW 15.12 COL 13.57
          FGCOLOR 7 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 77.72 BY 19.23
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
         HEIGHT             = 19.23
         WIDTH              = 77.72
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
       Firma:RESIZABLE IN FRAME F_Firma        = TRUE.

/* SETTINGS FOR FILL-IN F_Descripcion IN FRAME F_Firma
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_Nombre IN FRAME F_Firma
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_Ced IN FRAME F_Firma
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Msaje IN FRAME F_Firma
   NO-ENABLE ALIGN-L                                                    */
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


&Scoped-define SELF-NAME Btn_Anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Anterior WIN_Firma
ON CHOOSE OF Btn_Anterior IN FRAME F_Firma /* Button 122 */
DO:
  FIND PREV Tmp NO-ERROR.
  IF AVAILABLE Tmp THEN DO:
      gTexto = SEARCH("imagenes\firmas\" + Tmp.Nit + ".jpg").
      IF gTexto EQ ? THEN DO:
         ASSIGN W_Msaje:SCREEN-VALUE = "Falta la firma del cliente."
                W_Ok = Firma:LOAD-IMAGE("imagenes\firmas\0.jpg") NO-ERROR.
         ASSIGN F_Nombre:SCREEN-VALUE = Tmp.Nom
                W_Ced:SCREEN-VALUE    = Tmp.Nit.
      END.
      ELSE  DO:
         ASSIGN W_Msaje:SCREEN-VALUE = " "
                W_Ok = Firma:LOAD-IMAGE(gTexto) NO-ERROR.
         ASSIGN F_Nombre:SCREEN-VALUE = Tmp.Nom
                W_Ced:SCREEN-VALUE    = Tmp.Nit.
      END.
      
      IF Nit_Firma EQ Tmp.Nit THEN
         F_Descripcion:SCREEN-VALUE = "Titular Cuenta".
      ELSE
         F_Descripcion:SCREEN-VALUE = Tmp.Des.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Siguiente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Siguiente WIN_Firma
ON CHOOSE OF Btn_Siguiente IN FRAME F_Firma /* Button 121 */
DO:
  FIND NEXT Tmp NO-ERROR.
  IF AVAILABLE Tmp THEN DO:
      gTexto = SEARCH("imagenes\firmas\" + Tmp.Nit + ".jpg").
      IF gTexto EQ ? THEN DO:
         ASSIGN W_Msaje:SCREEN-VALUE = "Falta la firma del cliente."
                W_Ok = Firma:LOAD-IMAGE("imagenes\firmas\0.jpg") NO-ERROR.
         ASSIGN F_Nombre:SCREEN-VALUE = Tmp.Nom
                W_Ced:SCREEN-VALUE    = Tmp.Nit.
      END.
      ELSE  DO:
         ASSIGN W_Msaje:SCREEN-VALUE = " "
                W_Ok = Firma:LOAD-IMAGE(gTexto) NO-ERROR.
         ASSIGN F_Nombre:SCREEN-VALUE = Tmp.Nom
                W_Ced:SCREEN-VALUE    = Tmp.Nit.
      END.
      
      IF Nit_Firma EQ Tmp.Nit THEN
         F_Descripcion:SCREEN-VALUE = "Titular Cuenta".
      ELSE
         F_Descripcion:SCREEN-VALUE = Tmp.Des.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-117
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-117 WIN_Firma
ON CHOOSE OF BUTTON-117 IN FRAME F_Firma /* Button 117 */
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
  DISPLAY W_Msaje W_Ced F_Nombre F_Descripcion 
      WITH FRAME F_Firma IN WINDOW WIN_Firma.
  ENABLE BUTTON-117 Btn_Anterior Btn_Siguiente Firma RECT-227 
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
  IF Opcion EQ 0 THEN
     Win_Firma:TITLE = "Firma del Titular".
  ELSE DO:
     FIND Varios WHERE Varios.Tipo EQ 3 AND 
                       Varios.Codigo EQ Opcion NO-LOCK NO-ERROR.
     IF AVAILABLE Varios THEN
         Win_Firma:TITLE = "Firmas del Titular y sus " + Varios.Descripcion.
  END.    

  RUN SUPER.
  
DO WITH FRAME F_Firma:
  FIND Clientes WHERE Clientes.Nit EQ Nit_Firma NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN DO:
      CREATE Tmp.
      ASSIGN Tmp.Nit = Clientes.Nit
             Tmp.Nom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
             Tmp.Fec = Clientes.Fec_ActFirma
             Tmp.Idf = Clientes.Firma
             Tmp.Des = "Titular Cuenta"
             F_Descripcion:SCREEN-VALUE = "Titular Cuenta".
             
      ASSIGN F_Nombre:SCREEN-VALUE IN FRAME F_Firma = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
             W_Ced:SCREEN-VALUE                     = Clientes.Nit.
  END.
  IF Opcion NE 0 THEN DO:
      FOR EACH Relaciones WHERE Relaciones.Nit EQ Nit_Firma AND 
                                Relaciones.Cod_Relacion EQ Opcion AND
                                Relaciones.Cod_Producto EQ Cod_Prod AND
                                Relaciones.Cuenta       EQ Cue_Prod NO-LOCK:
          FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
          IF AVAILABLE Clientes THEN
          DO:
              CREATE Tmp.
              ASSIGN Tmp.Nit = Clientes.Nit
                     Tmp.Des = Relaciones.Descripcion
                     Tmp.Nom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                     Tmp.Fec = Clientes.Fec_ActFirma
                     Tmp.Idf = Clientes.Firma.
          END.
      END.
  END.
  
  IF Opcion EQ 0 THEN
     DISABLE Btn_Anterior Btn_Siguiente.
  ELSE
     ENABLE  Btn_Anterior Btn_Siguiente.

  /*IF Clientes.Fotografia THEN DO:
      gTexto = SEARCH("imagenes\fotos\" + TRIM(Clientes.Nit:SCREEN-VALUE) + ".jpg").
      IF gTexto EQ ? THEN DO:
         MESSAGE "No ha sido capturada la fotografia" SKIP
                 "del cliente." VIEW-AS ALERT-BOX INFORMATION.
         W_Ok = Foto:LOAD-IMAGE("imagenes\fotos\0.jpg") NO-ERROR.
      END.
      ELSE 
         W_Ok = Foto:LOAD-IMAGE(gTexto) NO-ERROR.
  END.*/
  
  gTexto = SEARCH("imagenes\firmas\" + Nit_Firma + ".jpg").

  IF gTexto EQ ? THEN DO:
     ASSIGN W_Msaje:SCREEN-VALUE = "Falta la firma del cliente."
            W_Ok                 = Firma:LOAD-IMAGE("imagenes\firmas\0.jpg") NO-ERROR.
  END.
  ELSE 
     ASSIGN W_Msaje:SCREEN-VALUE = " "
            W_Ok = Firma:LOAD-IMAGE(gTexto) NO-ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

