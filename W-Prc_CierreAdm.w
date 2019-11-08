&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Prc_CierreDia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Prc_CierreDia 
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
        
   DEFI TEMP-TABLE Total_Dia LIKE Total_Agencia
        FIELD TasaXSdo       LIKE Total_Dia.Sdo_Dia
        FIELD TTasa          LIKE Ahorros.Tasa.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Proc

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-120 RECT-314 BUTTON-5 Btn_Procesar ~
Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS W_DiaC W_MesC W_Nmes W_AnoC W_mensaje ~
W_Cont 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 W_MesC W_AnoC 
&Scoped-define List-2 W_mensaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Prc_CierreDia AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.27
     BGCOLOR 8 .

DEFINE BUTTON Btn_Procesar 
     LABEL "&Procesar" 
     SIZE 10 BY 1.46 TOOLTIP "Realiza el Proceso de Cierre".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE W_AnoC AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cont AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.57 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DiaC AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Dia" 
     VIEW-AS FILL-IN 
     SIZE 3.29 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_mensaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33.14 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_MesC AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Mes" 
     VIEW-AS FILL-IN 
     SIZE 3.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Nmes AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-120
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.57 BY 4.08.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.29 BY 5.65.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Proc
     BUTTON-5 AT ROW 1.54 COL 62
     W_DiaC AT ROW 3.42 COL 12.86 COLON-ALIGNED
     W_MesC AT ROW 3.42 COL 20.86 COLON-ALIGNED
     W_Nmes AT ROW 3.42 COL 24.14 COLON-ALIGNED NO-LABEL
     W_AnoC AT ROW 3.42 COL 42.29 COLON-ALIGNED
     Btn_Procesar AT ROW 3.42 COL 62 HELP
          "Permite Realizar el Proceso de Cierre"
     Btn_Done AT ROW 5.31 COL 62 HELP
          "Sale del proceso de Depreciación y Ajustes"
     W_mensaje AT ROW 5.85 COL 5 COLON-ALIGNED NO-LABEL
     W_Cont AT ROW 5.85 COL 39.14 COLON-ALIGNED NO-LABEL
     "Agencia Administrativa" VIEW-AS TEXT
          SIZE 23 BY .81 AT ROW 2.42 COL 8
          FGCOLOR 7 FONT 1
     "RegistrosTotales" VIEW-AS TEXT
          SIZE 11.43 BY .5 AT ROW 5.31 COL 41.14
          BGCOLOR 17 FGCOLOR 7 
     RECT-120 AT ROW 2.85 COL 6.57
     RECT-314 AT ROW 1.27 COL 60
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79 BY 6.31
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
  CREATE WINDOW W-Prc_CierreDia ASSIGN
         HIDDEN             = YES
         TITLE              = "Proceso de Cierre Diario de Agencia Administrativa, Programa W-Prc_CierreADM.W"
         HEIGHT             = 6.31
         WIDTH              = 79
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 85.72
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 85.72
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
/* SETTINGS FOR WINDOW W-Prc_CierreDia
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Proc
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN W_AnoC IN FRAME F_Proc
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_Cont IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_DiaC IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_mensaje IN FRAME F_Proc
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN W_MesC IN FRAME F_Proc
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_Nmes IN FRAME F_Proc
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Prc_CierreDia)
THEN W-Prc_CierreDia:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Prc_CierreDia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Prc_CierreDia W-Prc_CierreDia
ON END-ERROR OF W-Prc_CierreDia /* Proceso de Cierre Diario de Agencia Administrativa, Programa W-Prc_CierreADM.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Prc_CierreDia W-Prc_CierreDia
ON WINDOW-CLOSE OF W-Prc_CierreDia /* Proceso de Cierre Diario de Agencia Administrativa, Programa W-Prc_CierreADM.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Prc_CierreDia
ON CHOOSE OF Btn_Done IN FRAME F_Proc /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Procesar W-Prc_CierreDia
ON CHOOSE OF Btn_Procesar IN FRAME F_Proc /* Procesar */
DO:
  RUN _SetCurs.r("WAIT").
  
  RUN Proceso NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     ASSIGN W_Mensaje:SCREEN-VALUE = "El Proceso Tiene errores...Verifique por favor.".  
     RUN _SetCurs.r("ARROW").  
     RETURN.
  END.
  ELSE
     ASSIGN W_Mensaje:SCREEN-VALUE = "Generado el Proceso de Cierre Diario!...!".

  RUN _SetCurs.r("ARROW").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Prc_CierreDia
ON CHOOSE OF BUTTON-5 IN FRAME F_Proc /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Prc_CierreDia 


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

  
   ASSIGN W_OfiIni                            = W_Agencia
          W_OfiFin                            = W_Agencia
          W_MesC:SCREEN-VALUE IN FRAME F_Proc = STRING(MONTH(W_Fecha))
          W_DiaC:SCREEN-VALUE                 = STRING(DAY(W_Fecha))
          W_AnoC:SCREEN-VALUE                 = STRING(YEAR(W_Fecha)).
   CASE MONTH(W_Fecha):
        WHEN 1 THEN
          W_NMes = "Enero".
        WHEN 2 THEN
          W_NMes = "Febrero".
        WHEN 3 THEN
          W_NMes = "Marzo".
        WHEN 4 THEN
          W_NMes = "Abril".
        WHEN 5 THEN
          W_NMes = "Mayo".
        WHEN 6 THEN
          W_NMes = "Junio".
        WHEN 7 THEN
          W_NMes = "Julio".
        WHEN 8 THEN
          W_NMes = "Agosto".
        WHEN 9 THEN
          W_NMes = "Septiembre".
        WHEN 10 THEN
          W_NMes = "Octubre".
        WHEN 11 THEN
          W_NMes = "Noviembre".
        WHEN 12 THEN
          W_NMes = "Diciembre".
   END CASE. 

   W_NMes:SCREEN-VALUE = W_NMes.

   IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Prc_CierreDia  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Prc_CierreDia)
  THEN DELETE WIDGET W-Prc_CierreDia.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Prc_CierreDia  _DEFAULT-ENABLE
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
  DISPLAY W_DiaC W_MesC W_Nmes W_AnoC W_mensaje W_Cont 
      WITH FRAME F_Proc IN WINDOW W-Prc_CierreDia.
  ENABLE RECT-120 RECT-314 BUTTON-5 Btn_Procesar Btn_Done 
      WITH FRAME F_Proc IN WINDOW W-Prc_CierreDia.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  VIEW W-Prc_CierreDia.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso W-Prc_CierreDia 
PROCEDURE Proceso :
DEFI VAR W_FecCorte LIKE W_Fecha.  
  DEFI VAR K          AS INTEG FORM "9".
  DEFI VAR id         AS LOGICAL INITIAL NO.
  /*Validaciones*/
  FOR EACH Agencias WHERE Agencias.Agencia EQ 10
                      AND Agencias.Estado  NE 2 NO-LOCK:   /*Agencia administrativa*/
      FIND FIRST Usuarios WHERE Usuarios.Agencia    EQ Agencias.Agencia
                            AND Usuarios.Usuario    NE W_Usuario
                            AND Usuarios.Estado     EQ 1
                            AND Usuarios.Id_Entrada NO-LOCK NO-ERROR.
      IF AVAILABLE(Usuarios) THEN DO:                                                                                             
         MESSAGE "El Usuario    : " Usuarios.Usuario "-" Usuarios.Nombre SKIP 
                 "De la Agencia : " Agencias.Agencia "-" Agencias.Nombre SKIP
                 "Està conectado al Aplicativo...        Revise por favor. No se permite la operación." SKIP                                          
               VIEW-AS ALERT-BOX TITLE "Confirmar Proceso".                                              
         RETURN ERROR.                                                                                                               
      END.                                      
        
      FIND ProcDia WHERE ProcDia.Agencia     EQ Agencias.Agencia                                                                     
                     AND ProcDia.Fecha_Proc  EQ W_Fecha                                                                              
                     AND ProcDia.Cod_Proceso EQ 6
                     AND ProcDia.Estado      EQ 1  NO-LOCK NO-ERROR.                                                                 
      IF NOT AVAILABLE(ProcDia) THEN DO:                                                                                             
         MESSAGE "Este proceso Ya Fué Ejecutado para este Día en la Agencia: " Agencias.Agencia SKIP                                 
                 "O no està Matriculado...Revise por favor. No se permite la operación." SKIP                                          
               VIEW-AS ALERT-BOX TITLE "Confirmar Proceso".                                              
         RETURN ERROR.                                                                                                               
      END.                                                                                                                           
  END.     
 /*Fin Validaciones*/
  
  DO TRANSACTION ON ERROR UNDO:
     FOR EACH Agencias WHERE Agencias.Agencia EQ 10
                         AND Agencias.Estado  NE 2: 
         FIND FIRST Calendario WHERE Calendario.Agencia EQ Agencias.Agencia
                                 AND Calendario.Ano     EQ YEAR(W_Fecha)
                                 AND Calendario.Mes     EQ MONTH(W_Fecha)
                                 AND Calendario.Dia     EQ DAY(W_Fecha) NO-ERROR.
         FOR EACH TOTAL_Dia: DELETE TOTAL_Dia. END.
         W_FecCorte        = W_Fecha.
            
         RUN ProcTotales.     /*Genera tabla Temp Total_Agencia, diaria*/
            
         ASSIGN W_Mensaje:SCREEN-VALUE IN FRAME F_Proc = "Generando Total_Agencia...Espere por Favor.".
            
         FOR EACH TOTAL_Dia WHERE TOTAL_Dia.Agencia EQ 10:   /*Pasa la Temporal a la BD*/
             CREATE TOTAL_Agencia.
             BUFFER-COPY TOTAL_Dia TO TOTAL_Agencia.
         END.
            
         FIND ProcDia WHERE ProcDia.Agencia  EQ 10
                      AND ProcDia.Fecha_Proc  EQ W_Fecha                                                                              
                      AND ProcDia.Cod_Proceso EQ 6
                      AND ProcDia.Estado      EQ 1  NO-ERROR.
            
         ASSIGN ProcDia.Estado  = 2               /*Proceso de Cierre Ejecutado*/
                Agencias.Estado = 1.              /*Agencia abierta*/         
     END.      
  END.  /*Fin Tx*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcTotales W-Prc_CierreDia 
PROCEDURE ProcTotales :
/*------------------------------------------------------------------------------
  Purpose:  Crea tabla Total_Agencia para Ahorros.
            Invoca a ProcTotCreditos.  
 ------------------------------------------------------------------------------*/
  FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK:
      CREATE TOTAL_Dia.
      ASSIGN TOTAL_Dia.Agencia        = Agencias.Agencia
             TOTAL_Dia.Fecha          = W_Fecha
             TOTAL_Dia.Clase_Producto = Pro_Ahorros.Tip_Ahorro
             TOTAL_Dia.Codigo         = STRING(Pro_Ahorros.Cod_Ahorro,"999")
             TOTAL_Dia.Tipo_Producto  = 1
             W_Cont                   = W_Cont + 1
             W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont).
        
      FOR EACH Ahorros WHERE Ahorros.Agencia    EQ Agencias.Agencia
                         AND Ahorros.Cod_Ahorro EQ Pro_Ahorros.Cod_Ahorro:

          IF Calendario.Cierre THEN
             ASSIGN Ahorros.Sdo_AnualPerAnt[MONTH(W_Fecha)] = Ahorros.Sdo_Anuales [MONTH(W_Fecha)]
                    Ahorros.Sdo_Anuales [MONTH(W_Fecha)]    = (Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje)
                    Ahorros.Num_DepMes                      = 0
                    Ahorros.Num_RetMes                      = 0
                    Ahorros.Val_DepMes                      = 0
                    Ahorros.Val_RetMes                      = 0.
            
          ASSIGN TOTAL_Dia.Vr_Consignado = TOTAL_Dia.Vr_Consignado + Ahorros.Val_DepDia 
                 TOTAL_Dia.Vr_Retirado   = TOTAL_Dia.Vr_Retirado   + Ahorros.Val_RetDia
                 Ahorros.Val_DepDia      = 0
                 Ahorros.Val_RetDia      = 0
                 Ahorros.Num_DepDia      = 0
                 Ahorros.Num_RetDia      = 0.
                         
          IF (Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje) EQ 0
          AND Ahorros.Fec_Apertura                        NE W_Fecha
          AND Ahorros.Fec_Cancelac                        NE W_Fecha THEN NEXT.
            
          ASSIGN TOTAL_Dia.Cta_totales = TOTAL_Dia.Cta_totales + 1   
                 TOTAL_Dia.Sdo_dia     = TOTAL_Dia.Sdo_dia + (Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje)
                 TOTAL_Dia.TasaXSdo    = TOTAL_Dia.TasaXSdo + 
                                         ((Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje) * Ahorros.Tasa)
                 TOTAL_Dia.TTasa       = TOTAL_Dia.TTasa + Ahorros.Tasa.

          IF Ahorros.Fec_Apertura EQ W_Fecha THEN
             TOTAL_Dia.Cta_nuevas = TOTAL_Dia.Cta_nuevas + 1.
             
          IF Ahorros.Fec_Cancelac EQ W_Fecha THEN
             TOTAL_Dia.Cta_Retiradas = TOTAL_Dia.Cta_Retiradas + 1.
      END.
        
      ASSIGN TOTAL_Dia.Tasa_Ponderada = TOTAL_Dia.TasaXSdo / TOTAL_Dia.Sdo_dia
             TOTAL_Dia.Tasa_promedio  = TOTAL_Dia.TTasa    / TOTAL_Dia.Cta_totales.
            
     /* FOR EACH Taquilla WHERE Taquilla.Agencia   EQ Agencias.Agencia
                         AND  Taquilla.Fec_Trans EQ W_Fecha NO-LOCK:
          IF  Taquilla.Tip_Prod EQ 1
          AND Taquilla.Cod_Prod EQ Pro_Ahorros.Cod_Ahorro THEN DO:
              IF Taquilla.Naturaleza EQ "CR" THEN
                 ASSIGN TOTAL_Dia.Vr_Consignado = TOTAL_Dia.Vr_Consignado +
                                                  (Taquilla.Val_Cheque + Taquilla.Val_Efectivo).
              ELSE 
                 ASSIGN TOTAL_Dia.Vr_Retirado   = TOTAL_Dia.Vr_Retirado +
                                                  (Taquilla.Val_Cheque + Taquilla.Val_Efectivo).
          END.
      END.*/
                
  END.
    
  RUN ProcTotCreditos. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcTotContab W-Prc_CierreDia 
PROCEDURE ProcTotContab :
/*------------------------------------------------------------------------------
  Purpose:     Totales Contables desde Cuentas.Id_Total
 ------------------------------------------------------------------------------*/
 DEFI VAR K AS INTEG FORM "99".
 
 FOR EACH Cuentas WHERE Cuentas.Estado EQ 1
                    AND Cuentas.Tipo   EQ 2
                    AND Cuentas.Id_Total    NO-LOCK:
     CREATE TOTAL_Dia.
     ASSIGN TOTAL_Dia.Agencia        = Agencias.Agencia      
            TOTAL_Dia.Fecha          = W_Fecha                
            TOTAL_Dia.Clase_Producto = 3 
            TOTAL_Dia.Codigo         = Cuentas.Cuenta 
            TOTAL_Dia.Tipo_Producto  = 3
            W_Cont                   = W_Cont + 1
            W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont). 
        
     FIND FIRST Sal_Cuenta WHERE Sal_Cuenta.Cuenta  EQ Cuentas.Cuenta
                             AND Sal_Cuenta.Ano     EQ YEAR(W_Fecha) 
                             AND Sal_Cuenta.Agencia EQ Agencias.Agencia NO-LOCK NO-ERROR.
     IF AVAIL(Sal_Cuenta) THEN DO:
        ASSIGN TOTAL_Dia.Sdo_Dia = Sal_Cuenta.Sal_Inicial. 
             
        DO K = 1 TO MONTH(W_Fecha):
           IF Cuentas.Natur EQ "DB" THEN
              ASSIGN TOTAL_Dia.Sdo_Dia = TOTAL_Dia.Sdo_Dia + (Sal_Cuenta.Db[K] - Sal_Cuenta.Cr[K]).
           ELSE 
              ASSIGN TOTAL_Dia.Sdo_Dia = TOTAL_Dia.Sdo_Dia + (Sal_Cuenta.Cr[K] - Sal_Cuenta.Db[K]).    
        END.
     END.
        
     FOR EACH Mov_Contable WHERE Mov_Contable.Agencia    EQ Agencias.Agencia
                            AND  Mov_Contable.Cuenta     EQ Cuentas.Cuenta
                            AND  Mov_Contable.Fec_Contab EQ W_Fecha NO-LOCK:
          
         ASSIGN TOTAL_Dia.Vr_Consignado = TOTAL_Dia.Vr_Consignado + Mov_Contable.Db
                TOTAL_Dia.Vr_Retirado   = TOTAL_Dia.Vr_Retirado   + Mov_Contable.Cr.
     END.      
 END.

 RUN ProcTotInver.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcTotCreditos W-Prc_CierreDia 
PROCEDURE ProcTotCreditos :
/*------------------------------------------------------------------------------
  Purpose:   Totales de crèditos e Invoca a ProcTotContab.  
------------------------------------------------------------------------------*/
  FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ 1 NO-LOCK:
      CREATE TOTAL_Dia.
      ASSIGN TOTAL_Dia.Agencia        = Agencias.Agencia
             TOTAL_Dia.Fecha          = W_Fecha
             TOTAL_Dia.Clase_Producto = Pro_Creditos.Tip_Credito
             TOTAL_Dia.Codigo         = STRING(Pro_Creditos.Cod_Credito,"999")
             TOTAL_Dia.Tipo_Producto  = 2
             W_Cont                   = W_Cont + 1
             W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont).
        
      FOR EACH Creditos WHERE Creditos.Agencia    EQ Agencias.Agencia
                         AND Creditos.Cod_Credito EQ Pro_Creditos.Cod_Credito:
          IF Calendario.Cierre THEN                 
             Creditos.Sdo_Anuales [MONTH(W_Fecha)] = Creditos.Sdo_Capital.
          
          IF  Creditos.Sdo_Capital   EQ 0
          AND Creditos.Fec_Desemb    NE W_Fecha
          AND Creditos.Fec_CanceTot  NE W_Fecha THEN NEXT.
            
          ASSIGN TOTAL_Dia.Cta_totales = TOTAL_Dia.Cta_totales + 1   
                 TOTAL_Dia.Sdo_dia     = TOTAL_Dia.Sdo_dia + Creditos.Sdo_Capital
                 TOTAL_Dia.TasaXSdo    = TOTAL_Dia.TasaXSdo + 
                                         (Creditos.Sdo_Capital * Creditos.Tasa)
                 TOTAL_Dia.TTasa       = TOTAL_Dia.TTasa + Creditos.Tasa
                 Total_Dia.Val_Atraso  = Total_Dia.Val_Atraso + Creditos.Val_Atraso.

          IF Creditos.Fec_Desemb EQ W_Fecha THEN
             TOTAL_Dia.Cta_nuevas = TOTAL_Dia.Cta_nuevas + 1.
             
          IF Creditos.Fec_CanceTot EQ W_Fecha THEN
             TOTAL_Dia.Cta_Retiradas = TOTAL_Dia.Cta_Retiradas + 1.
      END.
        
      ASSIGN TOTAL_Dia.Tasa_Ponderada = TOTAL_Dia.TasaXSdo / TOTAL_Dia.Sdo_dia
             TOTAL_Dia.Tasa_promedio  = TOTAL_Dia.TTasa    / TOTAL_Dia.Cta_totales.
            
      FOR EACH Taquilla WHERE Taquilla.Agencia   EQ Agencias.Agencia
                         AND  Taquilla.Fec_Trans EQ W_Fecha NO-LOCK:
          IF  Taquilla.Tip_Prod EQ 2
          AND Taquilla.Cod_Prod EQ Pro_Creditos.Cod_Credito THEN DO:
              IF Taquilla.Naturaleza EQ "CR" THEN
                 ASSIGN TOTAL_Dia.Vr_Consignado = TOTAL_Dia.Vr_Consignado +
                                                  (Taquilla.Val_Cheque + Taquilla.Val_Efectivo).
              ELSE 
                 ASSIGN TOTAL_Dia.Vr_Retirado   = TOTAL_Dia.Vr_Retirado +
                                                  (Taquilla.Val_Cheque + Taquilla.Val_Efectivo).
          END.
      END.
                
  END.

  RUN ProcTotContab.     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcTotInver W-Prc_CierreDia 
PROCEDURE ProcTotInver :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
 FOR EACH Pro_Inversiones NO-LOCK BY Pro_Inversiones.Categoria
                                  BY Pro_Inversiones.Cod_Producto:
     CREATE TOTAL_Dia.
     ASSIGN TOTAL_Dia.Agencia        = Agencias.Agencia      
            TOTAL_Dia.Fecha          = W_Fecha                
            TOTAL_Dia.Clase_Producto = Pro_Inversiones.Categoria
            TOTAL_Dia.Codigo         = STRING(Pro_Inversiones.Cod_Producto,"999")
            TOTAL_Dia.Tipo_Producto  = 4
            W_Cont                   = W_Cont + 1
            W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont). 
              
     FOR EACH Inversion_Sdos WHERE Inversion_Sdos.Agencia      EQ Agencias.Agencia
                               AND Inversion_Sdos.Cod_Producto EQ Pro_Inversiones.Cod_Producto
                                   NO-LOCK :
         IF  Inversion_Sdos.Sdo_Actual   EQ 0
         AND Inversion_Sdos.Fec_Apertura NE W_Fecha
         AND Inversion_Sdos.Fec_Canc     NE W_Fecha THEN NEXT.
            
         ASSIGN TOTAL_Dia.Cta_Totales = TOTAL_Dia.Cta_Totales + 1   
                TOTAL_Dia.Sdo_dia     = TOTAL_Dia.Sdo_dia + Inversion_Sdos.Sdo_Actual
                TOTAL_Dia.TasaXSdo    = TOTAL_Dia.TasaXSdo + 
                                        Inversion_Sdos.Sdo_Actual * Inversion_Sdos.Tasa_NomiAnual
                TOTAL_Dia.TTasa       = TOTAL_Dia.TTasa + Inversion_Sdos.Tasa_NomiAnual.

         IF Inversion_Sdos.Fec_Apertura EQ W_Fecha THEN
            TOTAL_Dia.Cta_nuevas = TOTAL_Dia.Cta_nuevas + 1.
             
         IF Inversion_Sdos.Fec_Canc EQ W_Fecha THEN
            TOTAL_Dia.Cta_Retiradas = TOTAL_Dia.Cta_Retiradas + 1.
     END.
          
     ASSIGN TOTAL_Dia.Tasa_Ponderada = TOTAL_Dia.TasaXSdo / TOTAL_Dia.Sdo_dia
            TOTAL_Dia.Tasa_promedio  = TOTAL_Dia.TTasa    / TOTAL_Dia.Cta_totales.

     FOR EACH Mov_Inversion WHERE Mov_Inversion.Agencia    EQ Agencias.Agencia
                              AND Mov_Inversion.Cod_Produc EQ Pro_Inversiones.Cod_Producto
                              AND Mov_Inversion.Fecha      EQ W_Fecha NO-LOCK:
         ASSIGN TOTAL_Dia.Vr_Consignado = TOTAL_Dia.Vr_Consignado + Mov_Inversion.Vr_Consig
                TOTAL_Dia.Vr_Retirado   = TOTAL_Dia.Vr_Retirado   + Mov_Inversion.Vr_Retiro.
     END.    
 END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

