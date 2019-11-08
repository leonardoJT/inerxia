  /*Programa Ptlla_Imprimir.P....Desde Pantalla letra(Fond) = -1(Grande)
  ---------------------------------------------------------------*/
  DEFINE INPUT PARAMETER archivo AS CHARACTER.
  DEFINE BUTTON b-ok IMAGE-UP FILE "imagenes/Volver" LABEL "&Salir del Reporte".   
  DEFINE VARIABLE K_Bs AS CHARACTER FORMAT "X(15)" BGCOL 15 VIEW-AS FILL-IN LABEL "Buscar:".   
  DEFINE VARIABLE W_Ok AS LOGICAL.
  DEFINE BUTTON Impresora IMAGE-UP FILE "imagenes/Impresora2" LABEL "Button 3" SIZE 6 BY 1.5 TOOLTIP "Salida a Impresora".
  DEFINE VAR tecla AS LOGICAL INITIAL TRUE.
  DEFINE VAR Rep-Editor AS CHARACTER VIEW-AS EDITOR INNER-CHARS 107 INNER-LINES 35 
         LARGE SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL FONT 2.

  DEFINE MENU POPUP-MENU-Rep-Editor
         MENU-ITEM m_Buscar       LABEL "Buscar      CTRL-F"
         MENU-ITEM m_Siguiente    LABEL "Siguiente   F9"  
         MENU-ITEM m_Anterior     LABEL "Anterior    F8"
         RULE
         MENU-ITEM m_Imprimir     LABEL "Imprimir    CTRL-P".

  DEFINE FRAME dialogo1
     b-ok       AT ROW 1.05 COL 01
     Impresora  AT ROW 1.05 COL 7 HELP "Permite Enviar la Consulta de Información a la Impresora"
     " Ingrese en la casilla en blanco la cadena de busqueda y luego la tecla tab " 
              AT ROW 1.08 COL 14 FGCOLOR 15 FONT 4 BGCOLOR 18
     K_Bs     AT ROW 1.05 COL 67 
     Rep-editor AT ROW 2.50 COL 1 SKIP
  WITH NO-LABELS NO-UNDERLINE THREE-D NO-BOX AT COL 1 ROW 1 SIZE 114.29 BY 23.08 BGCOLOR 17 FONT 5
       VIEW-AS DIALOG-BOX SCROLLABLE.  

  ENABLE ALL WITH FRAME Dialogo1.
  ASSIGN 
         Rep-Editor:POPUP-MENU IN FRAME Dialogo1 = MENU POPUP-MENU-Rep-Editor:HANDLE
         Rep-Editor:READ-ONLY IN FRAME Dialogo1 = YES
         Rep-Editor:SENSITIVE IN FRAME Dialogo1 = YES
         FRAME Dialogo1:TITLE = "Listado por Monitor"
         tecla = Rep-Editor:READ-FILE(Archivo) IN FRAME dialogo1.

  ON LEAVE OF K_Bs
  DO:
    ASSIGN FRAME Dialogo1 K_Bs.
    W_Ok = Rep-Editor:SEARCH(K_Bs,33) IN FRAME Dialogo1.
  END.

  ON CTRL-F OF Rep-Editor IN FRAME Dialogo1
  OR CHOOSE OF MENU-ITEM m_Buscar /* Buscar    CTRL-F */
  DO:
    APPLY "entry" TO K_Bs.
    RETURN NO-APPLY.
  END. 
  ON F9 OF Rep-Editor IN FRAME Dialogo1
  OR CHOOSE OF MENU-ITEM m_Siguiente /* Siguiente F9 */
  DO:
    W_Ok = Rep-Editor:SEARCH(K_Bs,33) IN FRAME Dialogo1.
  END.
  ON F8 OF Rep-Editor IN FRAME Dialogo1
  OR CHOOSE OF MENU-ITEM m_Anterior /* Anterior    F8 */
  DO:
      W_Ok = Rep-Editor:SEARCH(K_Bs,34) IN FRAME Dialogo1.
  END.
  ON CTRL-P OF Rep-Editor IN FRAME Dialogo1
  OR CHOOSE OF MENU-ITEM m_Imprimir
  OR CHOOSE OF Impresora /* Imprimir    CTRL-P    -1 = Parametro Fond*/
  DO:
      RUN adecomm/_osprint.r ( INPUT  ?, INPUT Archivo,INPUT -1,INPUT  1,INPUT  1,
                               INPUT  99999,OUTPUT tecla).
   END.

  ON WINDOW-CLOSE OF FRAME Dialogo1
   DO:
     APPLY "CHOOSE":U TO B-Ok.
     RETURN NO-APPLY.
   END.

  WAIT-FOR CHOOSE OF B-ok FOCUS B-ok.   
  DISABLE ALL WITH FRAME Dialogo1.  
  HIDE FRAME Dialogo1 NO-PAUSE.

