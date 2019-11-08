&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME Reimp_cheque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Reimp_cheque 
/*------------------------------------------------------------------------
  File:        W-Prc_CierreDia.W 
  Description: Proceso de Cierre diario
  Author:      GAER
  Created:     Feb.11/2004
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

   ON RETURN TAB.

   {Incluido/Variable.I "SHARED"}

   {Incluido/VARCON.I   "SHARED"}
        
   DEFINE VAR  C_Benef  AS   CHARACTER FORMAT "X(50)".
   DEFINE VAR  C_Ciudad LIKE Ubicacion.Nombre.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Proc

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-317 Valor nnit tcomen Rs_SiNoCheq ~
W_NomBenCC 
&Scoped-Define DISPLAYED-OBJECTS Valor nnit tcomen Rs_SiNoCheq W_NomBenCC 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR Reimp_cheque AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE nnit AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nit" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE tcomen AS CHARACTER FORMAT "X(256)":U 
     LABEL "comentario" 
     VIEW-AS FILL-IN 
     SIZE 42 BY .81 NO-UNDO.

DEFINE VARIABLE Valor AS CHARACTER FORMAT "X(256)":U 
     LABEL "Valor" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE W_NomBenCC AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 51.86 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_SiNoCheq AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Si", yes,
"No", no
     SIZE 15 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-317
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 9.96.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Proc
     Valor AT ROW 3.65 COL 29 COLON-ALIGNED WIDGET-ID 2
     nnit AT ROW 4.73 COL 29 COLON-ALIGNED WIDGET-ID 4
     tcomen AT ROW 5.81 COL 19.43 COLON-ALIGNED WIDGET-ID 6
     Rs_SiNoCheq AT ROW 7.15 COL 29.43 NO-LABEL WIDGET-ID 8
     W_NomBenCC AT ROW 9.31 COL 11 NO-LABEL WIDGET-ID 16
     "Nombre del Beneficiario del Cheque(Puede Modificarlo)" VIEW-AS TEXT
          SIZE 51.72 BY .81 AT ROW 8.54 COL 11.14 WIDGET-ID 12
          BGCOLOR 7 FGCOLOR 15 
     "Re-Imprimir  Cheque" VIEW-AS TEXT
          SIZE 31 BY 1.08 AT ROW 2.08 COL 21.86 WIDGET-ID 14
          BGCOLOR 7 FGCOLOR 15 
     RECT-317 AT ROW 1.54 COL 6 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.08
         SIZE 75 BY 11.27
         BGCOLOR 17 FGCOLOR 0 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW Reimp_cheque ASSIGN
         HIDDEN             = YES
         TITLE              = "Reimprime Cheques"
         HEIGHT             = 11.38
         WIDTH              = 75
         MAX-HEIGHT         = 26.27
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 26.27
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW Reimp_cheque
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Proc
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN W_NomBenCC IN FRAME F_Proc
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Reimp_cheque)
THEN Reimp_cheque:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Reimp_cheque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Reimp_cheque Reimp_cheque
ON END-ERROR OF Reimp_cheque /* Reimprime Cheques */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Reimp_cheque Reimp_cheque
ON WINDOW-CLOSE OF Reimp_cheque /* Reimprime Cheques */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME nnit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nnit Reimp_cheque
ON LEAVE OF nnit IN FRAME F_Proc /* Nit */
DO:
  ASSIGN NNIT.
 FIND FIRST Clientes WHERE Clientes.Nit EQ NNIT NO-LOCK NO-ERROR.
 ASSIGN C_Benef = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2)
                     WHEN AVAIL(Clientes).
 ASSIGN W_NomBenCC = C_benef
        W_NomBenCC:SCREEN-VALUE = C_benef.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_SiNoCheq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_SiNoCheq Reimp_cheque
ON MOUSE-SELECT-CLICK OF Rs_SiNoCheq IN FRAME F_Proc
DO:
   DEFINE VAR  C_Benef  AS   CHARACTER FORMAT "X(50)".
   DEFINE VAR  C_Ciudad LIKE Ubicacion.Nombre.        
                                                                 
   DEFINE VAR W_Cadena AS CHARACTER FORMAT "X(150)".             
   DEFINE VAR W_Monto1 AS CHARACTER FORMAT "X(70)".              
   DEFINE VAR W_Monto2 AS CHARACTER FORMAT "X(70)".              
   DEFINE VAR W_Monto3 AS CHARACTER FORMAT "X(70)".              
   DEFINE VAR W_Rpta   AS LOGICAL. 
   /* 13-Mayo-2008  Félix Vargas*/
   DEFINE VAR vctipo   AS CHARACTER FORMAT "X(1)".
   DEFINE VAR vcnit    AS CHARACTER FORMAT "X(12)" NO-UNDO.
   DEFINE VAR vinotran AS INTEGER   INITIAL 0 NO-UNDO.
   /*******************/
   ASSIGN Rs_SiNoCheq.
   /*FIND FIRST Clientes WHERE Clientes.Nit EQ NNIT NO-LOCK NO-ERROR.
   ASSIGN C_Benef = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2)
                       WHEN AVAIL(Clientes).*/
   
   ASSIGN W_NomBenCC.

   MESSAGE "Desea Imprimir Ahora el Cheque?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                  TITLE "Impresión de Cheque" UPDATE choice2 AS LOGICAL.
      IF CHOICE2 THEN DO:
         /* 13-Mayo-2008  Félix Vargas*/
         ASSIGN vctipo   = "2"
                vcnit    = ""
                vinotran = 0.
         /*******************************/
         RUN MontoEsc.r (INPUT Valor,INPUT 0,OUTPUT W_Cadena).
         RUN PartirValor IN W_Manija (INPUT W_Cadena,INPUT 60,OUTPUT W_Monto1,OUTPUT W_Monto2,OUTPUT W_Monto3).
   
/*          RUN F-Cheque.r (INPUT W_monto1, INPUT W_monto2, INPUT W_NomBenCC, INPUT W_Ciudad, INPUT 0,                          */
/*                          INPUT " ", INPUT vinotran,  INPUT Mov_Contable.Cr,  INPUT STRING(mov_contable.num_docu))  NO-ERROR. */
/*                                                                                                                              */

         RUN F-Cheque1.p (INPUT vctipo,INPUT vcnit,INPUT vinotran, /* 13-Mayo-2008  Félix Vargas*/
                         INPUT W_Monto1,INPUT W_Monto2,INPUT W_NomBenCC,INPUT W_Ciudad,    /*<----Con el Nom-Benef: vble W_NomBenCC, capturado*/
                         INPUT Valor, INPUT tcomen) NO-ERROR.
      END.
 

 ASSIGN Rs_SiNoCheq:SCREEN-VALUE = "No"
        Rs_SiNoCheq.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Valor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Valor Reimp_cheque
ON LEAVE OF Valor IN FRAME F_Proc /* Valor */
DO:
  ASSIGN VALOR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NomBenCC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NomBenCC Reimp_cheque
ON LEAVE OF W_NomBenCC IN FRAME F_Proc
DO:
  ASSIGN W_NomBenCC.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Reimp_cheque 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   RUN enable_UI.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Reimp_cheque  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Reimp_cheque)
  THEN DELETE WIDGET Reimp_cheque.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Reimp_cheque  _DEFAULT-ENABLE
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
  DISPLAY Valor nnit tcomen Rs_SiNoCheq W_NomBenCC 
      WITH FRAME F_Proc IN WINDOW Reimp_cheque.
  ENABLE RECT-317 Valor nnit tcomen Rs_SiNoCheq W_NomBenCC 
      WITH FRAME F_Proc IN WINDOW Reimp_cheque.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  VIEW Reimp_cheque.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso Reimp_cheque 
PROCEDURE Proceso :
/*------------------------------------------------------------------------------
  Purpose:     
  ------------------------------------------------------------------------------*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcTotales Reimp_cheque 
PROCEDURE ProcTotales :
/*------------------------------------------------------------------------------
  Purpose:  Crea tabla Total_Agencia para Ahorros.
            Invoca a ProcTotCreditos.  
 ------------------------------------------------------------------------------*/
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcTotContab Reimp_cheque 
PROCEDURE ProcTotContab :
/*------------------------------------------------------------------------------
  Purpose:     Totales Contables desde Cuentas.Id_Total
 ------------------------------------------------------------------------------*/
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcTotCreditos Reimp_cheque 
PROCEDURE ProcTotCreditos :
/*------------------------------------------------------------------------------
  Purpose:   Totales de crèditos e Invoca a ProcTotContab.  
------------------------------------------------------------------------------*/
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcTotInver Reimp_cheque 
PROCEDURE ProcTotInver :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

