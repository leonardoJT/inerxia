   
  {Incluido\Clientes.i "NEW GLOBAL SHARED "}
  {Incluido\variable.i "NEW GLOBAL SHARED "}
  {Incluido\VarCon.i   "NEW GLOBAL SHARED "}
  {Incluido\Menu.i}
  
  DEFINE VARIABLE W_Dia AS INTEGER FORMAT 9.
  DEFINE VAR P_Ok AS LOGICAL INITIAL YES.
  DEFINE VARIABLE W_ValHora AS INTEGER FORMAT 99999.
  DEFINE VARIABLE W_MensajeHorario0 AS CHARACTER FORMAT "X(30)".
  DEFINE VARIABLE W_MensajeHorario1 AS CHARACTER FORMAT "X(30)".
  DEFINE VARIABLE W_MensajeHorario2 AS CHARACTER FORMAT "X(30)".

  ASSIGN W_Fecha = DATE(11,01,2004).
         TODAY   = W_Fecha.
  
  RUN W-Validar_Usuario.W (OUTPUT W_Permiso,INPUT 3, OUTPUT W_Grupo, OUTPUT Agencia_Cnt) NO-ERROR.
  RUN c:\SICOBEL\confg.p (OUTPUT W_Estacion).

  RUN Inicia.
  VIEW W_Ventana.

  ON ESC STOP.   
  ON RETURN RETURN.
  QUIT.

PROCEDURE  P_ComienzoSesion:
  RUN Arma_Menu_Grupo.
  IF NOT W_Bandera THEN DO:
     CREATE SUB-MENU W_SubMenu[1]
            ASSIGN PARENT = W_MenuBarra
                   LABEL  = "Sin Opciones".

     CREATE MENU-ITEM W_MenuItem
            ASSIGN SUBTYPE = "RULE"
                   PARENT  = W_SubMenu[1].

     CREATE MENU-ITEM W_OpcionSalir
            ASSIGN PARENT = W_SubMenu[1]
                   LABEL  = "Salir del Sistema"
            TRIGGERS:
               ON CHOOSE PERSISTENT RUN EjecutarSalir IN THIS-PROCEDURE.
            END TRIGGERS. 
     ASSIGN W_Bandera = YES.
  END.

  CREATE SUB-MENU W_SubMenu[1]
         ASSIGN PARENT = W_MenuBarra
                LABEL  = "Ayudas".
                
  CREATE MENU-ITEM W_MenuItem
         ASSIGN PARENT = W_SubMenu[1]
                LABEL  = "Contenido"
         TRIGGERS:
            ON CHOOSE APPLY "HELP" TO W_Ventana.

         END TRIGGERS.
         ON CHOOSE OF BT_Ocultar IN FRAME F_MenuPpal DO:
            IF Bt_Ocultar:LABEL EQ "Ocultar Mensaje" THEN
             DO:
               HIDE Mensaje_Entidad Mensaje_Agencia IN FRAME F_MenuPpal.
               ASSIGN Bt_Ocultar:LABEL = "Mostrar Mensaje".
                      BT_Ocultar:TOOLTIP IN FRAME F_MenuPpal = "Muestra los mensajes de Entidad y de Oficina".
                      W_Metodo = BT_Ocultar:LOAD-IMAGE-UP("imagenes\ViewMensaje") 
                                 IN FRAME F_MenuPpal.
             END.
            ELSE
             DO:
               VIEW Mensaje_Entidad Mensaje_Agencia IN FRAME F_MenuPpal.
               ASSIGN Bt_Ocultar:LABEL = "Ocultar Mensaje".
                      BT_Ocultar:TOOLTIP IN FRAME F_MenuPpal = "Esconde los mensajes de Entidad y de Oficina".
                      W_Metodo = BT_Ocultar:LOAD-IMAGE-UP("imagenes\HideMensaje") 
                                 IN FRAME F_MenuPpal.
             END. 
         END.
         ON CHOOSE OF BT_Informacion IN FRAME F_MenuPpal DO:
            RUN W-InfDia.r.
         END.
         ON CHOOSE OF BT_Calculadora IN FRAME F_MenuPpal DO:
           RUN CALC.R(5,5).
         END.
                 
  
  ON F5 ANYWHERE
     RUN CALC.R(5,5).

  Mensaje_Agencia:READ-ONLY IN FRAME F_MenuPpal        = TRUE.
  Mensaje_Entidad:READ-ONLY IN FRAME F_MenuPpal        = TRUE.
  ASSIGN CURRENT-WINDOW    = W_Ventana
         W_Ventana:MENUBAR = W_MenuBarra.
         
  ENABLE ALL EXcEPT Inf_General WITH FRAME F_MenuPpal IN WINDOW W_Ventana.      
  WAIT-FOR CHOOSE OF W_OpcionSalir FOCUS W_Ventana.

  ON CURSOR-DOWN CURSOR-DOWN.
  ON RETURN RETURN.
  
  DELETE WIDGET W_ventana.
  DELETE WIDGET W_MenuBarra.
END PROCEDURE.

 {Incluido\Cnf_Menu.i}

PROCEDURE EjecutarVentana:
   DEFINE VARIABLE W_NomArc   AS CHARACTER FORMAT "X(60)" INITIAL "".
   DEFINE VARIABLE W_ClaTem LIKE Usuarios.Clave.
  
  DEFINE IMAGE Llave
     FILENAME "imagenes\seguridad":U
     SIZE 7 BY 1.88.

  DEFINE BUTTON Btn_Ayuda
     IMAGE-UP FILE "imagenes\Interrogacion"
     SIZE 8 BY 1.08 FONT 4.

   DEFINE BUTTON Btn_Aceptar  LABEL "&Entrar " SIZE 10 BY 1.08 AUTO-GO.
   DEFINE BUTTON Btn_Cancelar LABEL "&Cancelar" SIZE 10 BY 1.08 AUTO-ENDKEY. 
   DEFINE RECTANGLE W_Cuadro2 EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL SIZE 32 BY 1.7.
   DEFINE RECTANGLE W_Cuadro3 EDGE-PIXELS 4 GRAPHIC-EDGE NO-FILL SIZE 47 BY 5.
   DEFINE VARIABLE W_Prg AS CHARACTER FORMAT "X(20)".
   
   DEFINE FRAME F_Validacion
      Llave            AT ROW 1.25 COL 1.5
      Btn_Aceptar      AT ROW 1.25 COL 29 
      Btn_Cancelar     AT ROW 2.35 COL 29 
      W_ClaTem BLANK LABEL "Password " AT ROW 1.70 COL 9 BGCOLOR 15  
               VIEW-AS FILL-IN SIZE 8 BY 0.8
    WITH SIDE-LABELS THREE-D KEEP-TAB-ORDER FONT 5 TITLE "Ingrese la Clave de Acceso"
         BGCOLOR 17 FGCOLOR 7 VIEW-AS DIALOG-BOX OVERLAY NO-UNDERLINE SCROLLABLE.

   ON CHOOSE OF Btn_Aceptar
   DO:
     IF W_Clave NE W_ClaTem THEN DO:
        RUN P-GraLog IN W_Manija (INPUT "Trato de Entrar con clave errada al programa: " + STRING(INTEGER(SELF:PRIVATE-DATA)) + " Pero su tiempo de trabajo expiro" + STRING(TIME,"HH:MM:SS AM")).
        RUN MostrarMensaje IN W_Manija (INPUT 37, OUTPUT W_Eleccion).
     END.
   END.
      
   FIND Programas WHERE Programas.Programa EQ INTEGER(SELF:PRIVATE-DATA) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Programas THEN DO:
      RUN MostrarMensaje IN W_Manija (INPUT 41, OUTPUT W_Eleccion).
      ON RETURN RETURN.
      RETURN.
   END.  

   ASSIGN W_NomArc = W_Path + "" + TRIM(Programas.Ejecutable).
   IF SEARCH(W_NomArc) EQ ? THEN DO:
      RUN MostrarMensaje IN W_Manija (INPUT 41, OUTPUT W_Eleccion).
      ON RETURN RETURN.
      RETURN.
   END.

   ASSIGN W_ClaTem = "". 
   IF NOT Programas.Id_Clave THEN DO:                
      ON RETURN TAB.  
      ASSIGN W_Programa = SESSION:FIRST-PROCEDURE.
             W_Encontro = NO.

      DO WHILE VALID-HANDLE(W_Programa):
         IF W_Programa:FILE-NAME EQ TRIM(W_NomArc) THEN DO:
            ASSIGN Wk_Ventana = W_Programa:CURRENT-WINDOW.
            IF NOT VALID-HANDLE(Wk_Ventana) THEN
               LEAVE.

            ASSIGN W_Encontro = YES.
            IF Wk_Ventana:WINDOW-STATE NE 3 THEN
               ASSIGN Wk_Ventana:WINDOW-STATE = 3.
            ASSIGN W_Status = Wk_Ventana:MOVE-TO-TOP().
            APPLY "ENTRY" TO Wk_Ventana.
            LEAVE.
         END.
         ASSIGN W_Programa = W_Programa:NEXT-SIBLING.
      END.        
      IF NOT W_Encontro THEN DO:
         DO TRANSACTIO:
             FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario 
                             /*AND Usuarios.Agencia = W_Agencia*/  NO-ERROR.
             IF AVAILABLE(Usuarios) THEN DO: 
                ASSIGN Usuarios.proactual = Programas.Opcion
                       Usuarios.Hentrada  = time.
                
                FIND CURRENT Usuarios NO-LOCK NO-ERROR.
                       
                RUN Verificar_HoraJornada(OUTPUT P_Ok).
             END.
             FIND CURRENT Usuarios NO-LOCK NO-ERROR.  /*Nueva*/          
         END.
         
         IF NOT P_Ok OR Usuarios.Id_Entrada EQ NO  THEN DO:
           IF Id_Entrada EQ NO THEN DO:
              RUN P-GraLog IN W_Manija (INPUT "Trato de Entrar al Sistema aunque esta desactivado por el Admdor " + STRING(TIME,"HH:MM:SS AM")).
              MESSAGE "NO PUEDE INICIAR LA SESION" skip
                      "Ha sido desactivado por el Administrador"
              VIEW-AS ALERT-BOX WARNING TITLE "Desactivado del Sistema".
           END.
           ELSE DO:
             RUN P-GraLog IN W_Manija (INPUT "Trato de Entrar a programa: " + STRING(W_NomArc) + " Pero su tiempo de trabajo expiro" + STRING(TIME,"HH:MM:SS AM")).
             MESSAGE "NO PUEDE INICIAR LA SESION" skip
                      W_MensajeHorario0 skip(2)
                      W_MensajeHorario1 W_MensajeHorario2 
             VIEW-AS ALERT-BOX WARNING TITLE "Tiempo de Trabajo ha Expirado".
             FIND CURRENT Usuarios NO-LOCK NO-ERROR.
           END.
         END.
         ELSE DO:
            FIND CURRENT Usuarios NO-LOCK NO-ERROR.
            RELEASE Usuarios NO-ERROR.           
/*IF LOCKED usuarios  THEN 
   MESSAGE "no liberò usuario".
ELSE MESSAGE "Si liberò usuario".   */
           RUN VALUE(TRIM(W_NomArc)) PERSISTENT SET W_Programa NO-ERROR.
           IF VALID-HANDLE(W_Programa) THEN DO:
              RUN dispatch IN W_Programa ('initialize') NO-ERROR.
           END.
           IF ERROR-STATUS:ERROR THEN DO:
               RUN InitializeObject IN W_Programa NO-ERROR.
           END.
         END.
      END.
      VIEW W_ventana.
      RETURN.
   END.
  
   ON RETURN TAB.
   ENABLE ALL WITH FRAME F_Validacion IN WINDOW W_Ventana.

   ON LEAVE OF W_ClaTem
   DO:
     ON RETURN RETURN.  
     ASSIGN W_ClaTem.
   END.  

   ON CHOOSE OF Btn_Cancelar
   DO:
      HIDE FRAME F_Validacion.
      ASSIGN W_ClaTem = W_Clave + "1".
   END.

   WAIT-FOR CHOOSE OF Btn_Cancelar OR CHOOSE OF Btn_Aceptar FOCUS W_ClaTem.

   HIDE FRAME F_Validacion.
   IF W_Clave = W_ClaTem THEN DO:
      ON RETURN TAB. 
      ASSIGN W_Programa = SESSION:FIRST-PROCEDURE.
             W_Encontro = NO.

      DO WHILE VALID-HANDLE(W_Programa):
         IF W_Programa:FILE-NAME EQ TRIM(W_NomArc) THEN DO:
            ASSIGN Wk_Ventana = W_Programa:CURRENT-WINDOW.
            IF NOT VALID-HANDLE(Wk_Ventana) THEN
               LEAVE.

            ASSIGN W_Encontro = YES.           
            IF Wk_Ventana:WINDOW-STATE NE 3 THEN
               ASSIGN Wk_Ventana:WINDOW-STATE = 3.
            ASSIGN W_Status = Wk_Ventana:MOVE-TO-TOP().
            APPLY "ENTRY" TO Wk_Ventana.
            LEAVE.
         END.
         ASSIGN W_Programa = W_Programa:NEXT-SIBLING.
      END. 
             
      IF NOT W_Encontro THEN DO: /*SI NO ESTA*/
         RELEASE Usuarios NO-ERROR. 
         
/*IF LOCKED usuarios  THEN 
   MESSAGE "no liberò usuario".
ELSE MESSAGE "Si liberò usuario".          */
                
         FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
         IF AVAILABLE(Usuarios) THEN DO: 
           /* ASSIGN Usuarios.proactual = Programas.Opcion
                   Usuarios.Hentrada  = time.*/
            
            FIND CURRENT Usuarios NO-LOCK NO-ERROR.       
                   
            RUN Verificar_HoraJornada(OUTPUT P_Ok). 
            
            FIND CURRENT Usuarios NO-LOCK NO-ERROR. /*Nueva*/
         END. 
         IF NOT P_Ok OR Usuarios.Id_Entrada EQ NO  THEN DO:
           IF Id_Entrada EQ NO THEN DO:
             RUN P-GraLog IN W_Manija (INPUT "Trato de Entrar al Sistema aunque esta desactivado por el Admdor " + STRING(TIME,"HH:MM:SS AM")).
             MESSAGE "NO PUEDE INICIAR LA SESION" skip
                     "Ha sido desactivado por el Administrador"
             VIEW-AS ALERT-BOX WARNING TITLE "Desactivado del Sistema".
           END.
           ELSE DO:
             RUN P-GraLog IN W_Manija (INPUT "Trato de Entrar a programa: " + STRING(W_NomArc) + " Pero su tiempo de trabajo expiro" + STRING(TIME,"HH:MM:SS AM")).
             MESSAGE "NO PUEDE INICIAR LA SESION" skip
                      W_MensajeHorario0 skip(2)
                      W_MensajeHorario1 W_MensajeHorario2 
             VIEW-AS ALERT-BOX WARNING TITLE "Tiempo de Trabajo ha Expirado".
             RELEASE Usuarios NO-ERROR.
           END.
           FIND CURRENT Usuarios NO-LOCK NO-ERROR.   /*nueva*/
         END.
         ELSE DO:
            FIND CURRENT Usuarios NO-LOCK NO-ERROR.
            RUN VALUE(TRIM(W_NomArc)) PERSISTENT SET W_Programa NO-ERROR.
            IF VALID-HANDLE(W_Programa) THEN DO:
              RUN dispatch IN W_Programa ('initialize') NO-ERROR.
            END.
            IF ERROR-STATUS:ERROR THEN DO:
               RUN InitializeObject IN W_Programa NO-ERROR.
            END.
         END.
      END.
      
      VIEW W_ventana.
      
   END.
END PROCEDURE.


PROCEDURE EjecutarSalir:
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Ejecuta el Trigger de la opcion Salir
-------------------------------------------------------------*/

   RUN MostrarMensaje IN W_Manija (INPUT 40, OUTPUT W_Eleccion).                         
   IF NOT W_Eleccion THEN RETURN NO-APPLY.
  
   ON ESC STOP.   
   ON RETURN RETURN.

   RUN P-GraLog IN W_Manija (INPUT "SALIO DEL SISTEMA " + STRING(TIME,"HH:MM:SS AM")).
   FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario 
                   AND Usuarios.Agencia = W_Agencia NO-ERROR.
   IF AVAILABLE(Usuarios) THEN DO: 
      ASSIGN Usuarios.Id_Entrada = FALSE.
      RELEASE Usuarios NO-ERROR.
   END.  
   
   FIND Estaciones WHERE Estaciones.Estacion EQ W_Estacion 
                     AND Estaciones.Estado EQ 1 NO-ERROR.
   IF AVAILABLE(Estaciones) THEN
      ASSIGN Estaciones.Fec_UltSalida  = W_Fecha
             Estaciones.Hora_UltSalida = TIME.
   RELEASE Estaciones.
   APPLY "CLOSE":U TO W_Ventana.
   ON CURSOR-DOWN CURSOR-DOWN.  

   DELETE WIDGET W_MenuBarra.
   DELETE WIDGET W_ventana.

   QUIT.
END PROCEDURE.

PROCEDURE Inicia:
  FIND Agencias WHERE Agencias.Agencia EQ Agencia_Cnt NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Agencias THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 35, OUTPUT W_Eleccion).
     ON ESC STOP.   
     ON RETURN RETURN.
     QUIT.
    END.

  ASSIGN W_Agencia     = Agencias.Agencia
         W_Nom_Agencia = Agencias.Nombre
         W_Ciudad      = Agencias.Ciudad
         W_UbiDatos    = Agencias.Tip_Agencia
         Mensaje_Agencia = Mensaje_Agencia + " " + Agencia.Mensaje
         Inf_General   = Agencias.Nombre + " / Fecha: " + STRING(TODAY) 
                         + " / Hora: " + STRING(TIME, "HH:MM AM").
            
  FIND Entidad WHERE Entidad.Entidad EQ Agencia.Entidad NO-LOCK NO-ERROR.
  IF AVAILABLE(Entidad) THEN DO:
     ASSIGN W_Path          = Entidad.Dir_Programas
            W_PathSpl       = Entidad.Dir_Spl
            W_Entidad       = Entidad.Entidad
            W_Nom_Entidad   = Entidad.Nombre
            W_CenCosGral    = 999
            Mensaje_Entidad = Mensaje_Entidad + " " + Entidad.Mensaje.
     IF Entidad.Ind_SMLV NE 0 THEN DO:
        FIND Indicadores WHERE Indicadores.Indicador EQ Entidad.Ind_SMLV NO-LOCK NO-ERROR.
        IF AVAILABLE Indicadores THEN
           W_SMLV = Indicadores.Valor.
     END.
  END.
  ELSE          
     ASSIGN W_Path       = "C:\"
            W_PathSpl    = "C:\"
            W_CenCosGral = 999.


  IF  W_Permiso EQ 0 
  AND LASTKEY   NE KEYCODE("ESC") THEN DO:
      RUN P-GraLog IN W_Manija (INPUT "ENTRAR AL SISTEMA " + STRING(TIME,"HH:MM:SS AM")).  
      DISPLAY Mensaje_Entidad Mensaje_Agencia Inf_general  WITH NO-LABEL FRAME F_MenuPpal IN WINDOW W_Ventana.
      VIEW W_Ventana.
      RUN P_ComienzoSesion.
  END.

END PROCEDURE.

PROCEDURE Verificar_HoraJornada:
DEFINE OUTPUT PARAMETER P_Ok AS LOGICAL.
DEFINE VAR W_ValHora AS INTEGER.
 ASSIGN P_Ok      = YES 
        W_ValHora = TIME.
 IF WEEKDAY(TODAY) = 1 THEN W_Dia = 7.
 ELSE W_Dia = WEEKDAY(TODAY) - 1.
 FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
 IF Usuarios.IdHorario THEN DO:
    IF Usuarios.Tip_Menu EQ 1 THEN
    DO:
       IF W_ValHora LT Usuarios.HIniMan[1] OR W_ValHora GT Usuarios.HFinMan[1] THEN
       DO:
          IF W_ValHora LT Usuarios.HIniTar[1] OR W_ValHora GT Usuarios.HFinTar[1] THEN
          DO:
             P_Ok = NO.
             W_MensajeHorario0 = "Usted puede entrar al sistema todos los dias de:".
             W_MensajeHorario1 = STRING(Usuarios.HIniMan[1],"hh:mm am") + " a " +
                                 STRING(Usuarios.HFinMan[1],"hh:mm am") + " o de ".
             W_MensajeHorario2 = STRING(Usuarios.HIniTar[1],"hh:mm am") + " a " +
                                 STRING(Usuarios.HFinTar[1],"hh:mm am").
          END.
       END.
    END.
    ELSE
    DO:
       IF NOT Usuarios.Dia_Permiso[W_Dia] THEN
       DO:
          P_Ok = NO.
          W_MensajeHorario0 = "Usted no tiene permiso de trabajo para este dia".
       END.
       ELSE
       DO:
         IF W_ValHora LT Usuarios.HIniMan[W_Dia] OR W_ValHora GT Usuarios.HFinMan[W_Dia] THEN
         DO:
             IF W_ValHora LT Usuarios.HIniTar[W_Dia] OR W_ValHora GT Usuarios.HFinTar[W_Dia] THEN
             DO:
                P_Ok = NO.
                W_MensajeHorario0 = "Su horario de trabajo para este dia es:".
                W_MensajeHorario1 = STRING(Usuarios.HIniMan[W_Dia],"hh:mm am") + " a " +
                                    STRING(Usuarios.HFinMan[W_Dia],"hh:mm am") + " o de ".
                W_MensajeHorario2 = STRING(Usuarios.HIniTar[W_Dia],"hh:mm am") + " a " +
                                    STRING(Usuarios.HFinTar[W_Dia],"hh:mm am").
             END.
         END.
       END.
    END.
 END.
END PROCEDURE.
