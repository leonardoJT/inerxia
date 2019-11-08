&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
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
{incluido\Variable.i "SHARED"}
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE W_RtaFiltro AS INTEGER.   /* Contiene El Valor del Radio Set */
DEFINE VARIABLE W_ArcJPG    AS CHARACTER. /* Contiene El Nombre del Archivo Imagen */    
DEFINE VARIABLE W_Rtaf      AS LOGICAL.   /* Contiene La Respuesta de la Funcion JPG */
DEFINE VARIABLE W_contador AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Admon

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Clientes

/* Definitions for FRAME Admon                                          */
&Scoped-define FIELDS-IN-QUERY-Admon Clientes.Nit Clientes.Firma ~
Clientes.fotografia Clientes.Fec_ActFoto Clientes.Fec_ActFirma ~
Clientes.Ult_UsuImagen 
&Scoped-define QUERY-STRING-Admon FOR EACH Clientes SHARE-LOCK
&Scoped-define OPEN-QUERY-Admon OPEN QUERY Admon FOR EACH Clientes SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Admon Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-Admon Clientes


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CFirma CFOTO RECT-1 RECT-2 RECT-3 RECT-4 ~
RECT-6 RECT-7 RECT-9 B-7 Cuantos B-8 B-9 B-6 BUTTON-3 Nom 
&Scoped-Define DISPLAYED-FIELDS Clientes.Nit Clientes.Firma ~
Clientes.fotografia Clientes.Fec_ActFoto Clientes.Fec_ActFirma ~
Clientes.Ult_UsuImagen 
&Scoped-define DISPLAYED-TABLES Clientes
&Scoped-define FIRST-DISPLAYED-TABLE Clientes
&Scoped-Define DISPLAYED-OBJECTS Cuantos Nom 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-1 
     LABEL "&Salvar" 
     SIZE 9.72 BY 1.54.

DEFINE BUTTON B-2 
     LABEL "&Deshacer" 
     SIZE 9.72 BY 1.54.

DEFINE BUTTON B-3 
     LABEL "&Ingresar" 
     SIZE 9.72 BY 1.54.

DEFINE BUTTON B-4 
     LABEL "&Borrar" 
     SIZE 9.72 BY 1.54.

DEFINE BUTTON B-5 
     LABEL "Ca&ncelar" 
     SIZE 9.72 BY 1.54.

DEFINE BUTTON B-6 DEFAULT 
     LABEL "S&alir" 
     SIZE 9.72 BY 1.54
     BGCOLOR 8 .

DEFINE BUTTON B-7 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 9.72 BY 1.54 TOOLTIP "Información".

DEFINE BUTTON B-8 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 9.72 BY 1.54 TOOLTIP "Imprimir".

DEFINE BUTTON B-9 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 5" 
     SIZE 9.72 BY 1.54 TOOLTIP "Consulta".

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 3" 
     SIZE 5 BY 1.12 TOOLTIP "Ayuda".

DEFINE VARIABLE Nom AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 56 BY .81
     BGCOLOR 18 FONT 4 NO-UNDO.

DEFINE IMAGE CFirma
     FILENAME "adeicon/blank":U
     SIZE 37 BY 6.19.

DEFINE IMAGE CFOTO
     FILENAME "adeicon/blank":U CONVERT-3D-COLORS
     SIZE 37 BY 6.19.

DEFINE VARIABLE Cuantos AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ninguno", 1,
"Uno", 2,
"Todos", 3
     SIZE 31 BY 1.35
     BGCOLOR 17 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11.14 BY 5.12.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11.14 BY 10.54.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 8.62.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 8.62.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 1.88.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 1.35.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 1.35.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Admon FOR 
      Clientes SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Admon
     B-7 AT ROW 1.65 COL 101.86
     Cuantos AT ROW 2.19 COL 34.43 NO-LABEL
     B-8 AT ROW 3.27 COL 101.86
     B-9 AT ROW 4.85 COL 101.86
     Clientes.Nit AT ROW 5.46 COL 10 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.86 BY .81
          BGCOLOR 15 FONT 4
     Clientes.Firma AT ROW 7.85 COL 8
          VIEW-AS TOGGLE-BOX
          SIZE 7.29 BY .73
     Clientes.fotografia AT ROW 7.85 COL 53.86
          LABEL "Fotografia"
          VIEW-AS TOGGLE-BOX
          SIZE 10 BY .77
     B-1 AT ROW 9.38 COL 101.86
     B-2 AT ROW 11.04 COL 101.86
     B-3 AT ROW 12.69 COL 101.86
     B-4 AT ROW 14.35 COL 101.86
     B-5 AT ROW 16 COL 101.86
     B-6 AT ROW 17.69 COL 101.86
     Clientes.Fec_ActFoto AT ROW 19.35 COL 74.43 COLON-ALIGNED
          LABEL "Fecha Actualización Foto"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 FONT 4
     Clientes.Fec_ActFirma AT ROW 19.38 COL 26.29 COLON-ALIGNED
          LABEL "Fecha Actualización Firma"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 FONT 4
     BUTTON-3 AT ROW 20.62 COL 104
     Clientes.Ult_UsuImagen AT ROW 20.92 COL 38.86
          LABEL "Usuario Ultima Modificación"
          VIEW-AS FILL-IN 
          SIZE 18 BY .81
          BGCOLOR 18 FONT 4
     Nom AT ROW 5.5 COL 29.72 COLON-ALIGNED NO-LABEL
     "Foto" VIEW-AS TEXT
          SIZE 3 BY .5 AT ROW 10.04 COL 55.86
          FONT 4
     "Firma" VIEW-AS TEXT
          SIZE 4 BY .5 AT ROW 9.96 COL 7.57
          FONT 4
     CFirma AT ROW 11.54 COL 8.72
     CFOTO AT ROW 11.54 COL 57
     RECT-1 AT ROW 1.46 COL 101
     RECT-2 AT ROW 9.04 COL 101
     RECT-3 AT ROW 10.23 COL 5
     RECT-4 AT ROW 10.27 COL 52.86
     RECT-6 AT ROW 1.92 COL 33.14
     RECT-7 AT ROW 7.5 COL 52.86
     RECT-9 AT ROW 7.5 COL 5.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.29 BY 21.73
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON B-6.


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
         TITLE              = "Administracion De Firmas y Fotos"
         HEIGHT             = 21.73
         WIDTH              = 113.29
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = 17
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
/* SETTINGS FOR FRAME Admon
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON B-1 IN FRAME Admon
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-2 IN FRAME Admon
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-3 IN FRAME Admon
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-4 IN FRAME Admon
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-5 IN FRAME Admon
   NO-ENABLE                                                            */
ASSIGN 
       CFOTO:RESIZABLE IN FRAME Admon        = TRUE.

/* SETTINGS FOR FILL-IN Clientes.Fec_ActFirma IN FRAME Admon
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Clientes.Fec_ActFoto IN FRAME Admon
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX Clientes.Firma IN FRAME Admon
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Clientes.fotografia IN FRAME Admon
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Clientes.Nit IN FRAME Admon
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Ult_UsuImagen IN FRAME Admon
   NO-ENABLE ALIGN-L EXP-LABEL                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Admon
/* Query rebuild information for FRAME Admon
     _TblList          = "bdcentral.Clientes"
     _Query            is OPENED
*/  /* FRAME Admon */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Administracion De Firmas y Fotos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Administracion De Firmas y Fotos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-1 wWin
ON CHOOSE OF B-1 IN FRAME Admon /* Salvar */
DO:
  ASSIGN Cuantos:SCREEN-VALUE = '1'.
  DISABLE ALL WITH FRAME Admon.    
  ENABLE B-6 B-7 B-8 B-9 Cuantos WITH FRAME Admon.  
/*   DISABLE TRIGGERS FOR LOAD OF Clientes. */
  RUN Grabar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-2 wWin
ON CHOOSE OF B-2 IN FRAME Admon /* Deshacer */
DO:
  DISABLE ALL WITH FRAME Admon.    
/*   DISABLE TRIGGERS FOR LOAD OF Clientes. */
  FOR EACH Clientes:
     RUN FN_CargarJPG (INPUT Clientes.Nit, OUTPUT W_rtaf).
     ASSIGN W_contador = W_contador + 1.
     IF W_rtaf = TRUE THEN
      RUN Grabar. 
   END.
  ASSIGN B-2:LABEL = '&Deshacer'.  
  ASSIGN Cuantos:SCREEN-VALUE = '1'.
  ENABLE B-6 B-7 B-8 B-9 Cuantos WITH FRAME Admon.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-6 wWin
ON CHOOSE OF B-6 IN FRAME Admon /* Salir */
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


&Scoped-define SELF-NAME B-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-7 wWin
ON CHOOSE OF B-7 IN FRAME Admon /* Button 1 */
DO:
  RUN w-infdia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-8 wWin
ON CHOOSE OF B-8 IN FRAME Admon /* Button 2 */
DO:
   DEFINE VAR Listado AS CHARACTER INITIAL "".
   
   Listado = W_PathSpl + "LinAct.LST".
  {Incluido\Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-9 wWin
ON CHOOSE OF B-9 IN FRAME Admon /* Button 5 */
DO:
  VIEW FRAME Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cuantos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cuantos wWin
ON VALUE-CHANGED OF Cuantos IN FRAME Admon
DO:
  DISABLE ALL EXCEPT B-6 B-7 B-8 B-9 WITH FRAME Admon.  
  W_RtaFiltro = SELF:INPUT-VALUE.  
  CASE W_RtaFiltro:
      WHEN 1 THEN DO:
        ASSIGN B-2:LABEL = '&Deshacer'.
        ENABLE Clientes.Nit WITH FRAME Admon.
      END.
      WHEN 2 THEN DO:
        ENABLE Clientes.Nit WITH FRAME Admon.
        Clientes.Nit:SCREEN-VALUE IN FRAME Admon = ' '.
        Nom:SCREEN-VALUE = ' '.         
        ASSIGN B-2:LABEL = '&Deshacer'.       
      END.
      WHEN 3 THEN DO:
       ENABLE B-2 WITH FRAME Admon. 
       ASSIGN B-2:LABEL = '&Procesar'.        
      END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Nit wWin
ON LEAVE OF Clientes.Nit IN FRAME Admon /* Nit */
DO:
 IF LASTKEY = KEYCODE("ENTER") THEN
  IF SELF:SCREEN-VALUE NE ' ' THEN DO:
    FIND FIRST Clientes WHERE Clientes.Nit = SELF:SCREEN-VALUE NO-ERROR.
    RUN FN_CargarJPG (INPUT TRIM(SELF:SCREEN-VALUE), OUTPUT W_rtaf).
    IF W_Rtaf = TRUE THEN
      ENABLE B-1 Clientes.Firma Clientes.Fotografia WITH FRAME Admon. 
     ELSE
      MESSAGE 'Nit Sin Firma o Foto...' VIEW-AS ALERT-BOX.  
  END.
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

  {&OPEN-QUERY-Admon}
  GET FIRST Admon.
  DISPLAY Cuantos Nom 
      WITH FRAME Admon IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Nit Clientes.Firma Clientes.fotografia Clientes.Fec_ActFoto 
          Clientes.Fec_ActFirma Clientes.Ult_UsuImagen 
      WITH FRAME Admon IN WINDOW wWin.
  ENABLE CFirma CFOTO RECT-1 RECT-2 RECT-3 RECT-4 RECT-6 RECT-7 RECT-9 B-7 
         Cuantos B-8 B-9 B-6 BUTTON-3 Nom 
      WITH FRAME Admon IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-Admon}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FN_CargarJPG wWin 
PROCEDURE FN_CargarJPG :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER W_Nitf AS CHARACTER.   /* Contiene el nit a buscar */
 DEFINE OUTPUT PARAMETER W_Rtav AS LOGICAL.    /* Retorna verdadero si se cumplio el proceso... */   
 IF AVAILABLE Clientes THEN DO:
   Nom:SCREEN-VALUE IN FRAME Admon = Clientes.Nombre + " -- " + STRING(W_contador). 

   /* Firma JPG  */     
    W_ArcJPG = 'Imagenes\Firmas\' + TRIM(W_Nitf) + '.JPG'.
    W_ArcJPG = SEARCH (W_ArcJPG).
    IF W_ArcJPG NE ? THEN 
      IF CFirma:LOAD-IMAGE(W_ArcJPG) THEN        
        W_Rtav = TRUE.
       ELSE
        W_Rtav = FALSE.  
     ELSE 
      W_Rtav = FALSE.     
  /*  Foto JPG  */          
      W_ArcJPG = 'Imagenes\Fotos\' + TRIM(W_Nitf) + '.JPG'.
      W_ArcJPG = SEARCH (W_ArcJPG).
      IF W_ArcJPG NE ? THEN
        IF CFoto:LOAD-IMAGE(W_ArcJPG) THEN
          W_Rtav = TRUE.
         ELSE
          W_Rtav = FALSE.               
       ELSE
         W_Rtav = FALSE.
   END.
  ELSE
   W_Rtav = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar wWin 
PROCEDURE Grabar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF Firma:SCREEN-VALUE IN FRAME Admon = 'yes' THEN
   ASSIGN Clientes.Firma = TRUE.
  ELSE
   ASSIGN Clientes.Firma = FALSE.
 IF Foto:SCREEN-VALUE  IN FRAME Admon = 'yes' THEN
   ASSIGN Clientes.Foto = TRUE.
  ELSE   
   ASSIGN Clientes.Foto = FALSE.  
 ASSIGN Clientes.Ult_UsuImagen = W_Usuario
        Clientes.Fec_ActFirma = W_Fecha
        Clientes.Fec_ActFoto = W_fecha
        Clientes.Fec_ActFirma:SCREEN-VALUE = STRING(W_fecha,'99/99/9999')    
        Clientes.Fec_ActFoto:SCREEN-VALUE = STRING(W_fecha,'99/99/9999').    
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
  Clientes.Nit:SCREEN-VALUE IN FRAME Admon = ' '.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

