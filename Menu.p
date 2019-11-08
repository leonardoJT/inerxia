{Incluido\Clientes.i "NEW GLOBAL SHARED"}
{Incluido\variable.i "NEW GLOBAL SHARED"}
{Incluido\VarCon.i "NEW GLOBAL SHARED"}
{Incluido\Menu.i}

DEFINE VAR W_Dia AS INTEGER FORMAT 9.
DEFINE VAR P_Ok AS LOGICAL INITIAL YES.
DEFINE VAR W_MensajeHorario0 AS CHARACTER FORMAT "X(30)".
DEFINE VAR W_MensajeHorario1 AS CHARACTER FORMAT "X(30)".
DEFINE VAR W_MensajeHorario2 AS CHARACTER FORMAT "X(30)".
DEFINE VAR pAgenciaUser AS INTEGER.

RUN W-Validar_Usuario.R(OUTPUT W_Permiso,
                        OUTPUT W_Grupo,
                        OUTPUT pAgenciaUser) NO-ERROR.

RUN Inicia.

VIEW W_Ventana.

ON ESC STOP.
ON RETURN RETURN.
QUIT.

/* ----------------------- */

PROCEDURE Inicia:
    FIND FIRST Agencias WHERE Agencias.Agencia = pAgenciaUser NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Agencias THEN DO:
        MESSAGE "La agencia en la que se encuentra matriculado el usuario no se encuentra." SKIP
                "Por favor, comuníqueses con el Administrador del Sistema."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        ON ESC STOP.
        ON RETURN RETURN.
        QUIT.
    END.

    W_Agencia = Agencias.Agencia.
    W_Nom_Agencia = Agencias.Nombre.
    W_Ciudad = Agencias.Ciudad.
    W_UbiDatos = Agencias.Tip_Agencia.
    Inf_General = Agencias.Nombre + " / Fecha de ingreso: " + STRING(TODAY) + " - " + STRING(TIME, "HH:MM AM").

    FIND FIRST Entidad WHERE Entidad.Entidad = Agencias.Entidad NO-LOCK NO-ERROR.
    IF AVAILABLE(Entidad) THEN DO:
        W_Path = Entidad.Dir_Programas.
        W_PathSpl = Entidad.Dir_Spl.
        W_Entidad = Entidad.Entidad.
        W_Nom_Entidad = Entidad.Nombre.
        
        IF Entidad.Ind_SMLV <> 0 THEN DO:
            FIND FIRST Indicadores WHERE Indicadores.Indicador = Entidad.Ind_SMLV NO-LOCK NO-ERROR.
            IF AVAILABLE Indicadores THEN
                W_SMLV = Indicadores.Valor.
        END.
    END.

    W_CenCosGral = 999.

    IF W_Permiso = 0 AND LASTKEY <> KEYCODE("ESC") THEN DO:
        DISPLAY Inf_general WITH NO-LABEL FRAME F_MenuPpal IN WINDOW W_Ventana.

        VIEW W_Ventana.
        RUN P_ComienzoSesion.
    END.

END PROCEDURE.


PROCEDURE P_ComienzoSesion:
    RUN Arma_Menu_Grupo.

    IF NOT W_Bandera THEN DO:
        CREATE SUB-MENU W_SubMenu[1]
            ASSIGN PARENT = W_MenuBarra
                   LABEL = "Sin Opciones".

        CREATE MENU-ITEM W_MenuItem
            ASSIGN SUBTYPE = "RULE"
                   PARENT = W_SubMenu[1].

        CREATE MENU-ITEM W_OpcionSalir
            ASSIGN PARENT = W_SubMenu[1]
                   LABEL = "Salir del Sistema"
                   TRIGGERS:
                       ON CHOOSE PERSISTENT RUN EjecutarSalir IN THIS-PROCEDURE.
                   END TRIGGERS.

        W_Bandera = YES.
    END.

    CREATE SUB-MENU W_SubMenu[1]
        ASSIGN PARENT = W_MenuBarra
               LABEL = "Ayudas".

    CREATE MENU-ITEM W_MenuItem
        ASSIGN PARENT = W_SubMenu[1]
               LABEL = "Contenido"
               TRIGGERS:
                   ON CHOOSE APPLY "HELP" TO W_Ventana.
               END TRIGGERS.

    ASSIGN CURRENT-WINDOW = W_Ventana
           W_Ventana:MENUBAR = W_MenuBarra.

    ENABLE ALL EXCEPT Inf_General WITH FRAME F_MenuPpal IN WINDOW W_Ventana.
    WAIT-FOR CHOOSE OF W_OpcionSalir FOCUS W_Ventana.

    ON CURSOR-DOWN CURSOR-DOWN.
    ON RETURN RETURN.

    DELETE WIDGET W_ventana.
    DELETE WIDGET W_MenuBarra.

END PROCEDURE.

{Incluido\Cnf_Menu.i}

PROCEDURE EjecutarVentana:
    DEFINE VARIABLE W_NomArc AS CHARACTER FORMAT "X(60)".
    DEFINE VARIABLE W_ClaTem LIKE Usuarios.Clave.

    DEFINE IMAGE Llave
        FILENAME "imagenes\seguridad":U
        SIZE 7 BY 1.88.

    DEFINE BUTTON Btn_Ayuda
        IMAGE-UP FILE "imagenes\Interrogacion" SIZE 8 BY 1.08 FONT 4.

    DEFINE BUTTON Btn_Aceptar LABEL "&Entrar " SIZE 10 BY 1.08 AUTO-GO.
    DEFINE BUTTON Btn_Cancelar LABEL "&Cancelar" SIZE 10 BY 1.08 AUTO-ENDKEY.
    DEFINE RECTANGLE W_Cuadro2 EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL SIZE 32 BY 1.7.
    DEFINE RECTANGLE W_Cuadro3 EDGE-PIXELS 4 GRAPHIC-EDGE NO-FILL SIZE 47 BY 5.
    DEFINE VARIABLE W_Prg AS CHARACTER FORMAT "X(20)".

    DEFINE FRAME F_Validacion
        Llave AT ROW 1.25 COL 1.5
        Btn_Aceptar AT ROW 1.25 COL 29
        Btn_Cancelar AT ROW 2.35 COL 29
        W_ClaTem BLANK LABEL "Password " AT ROW 1.70 COL 9 BGCOLOR 15 VIEW-AS FILL-IN SIZE 8 BY 0.8
        WITH SIDE-LABELS THREE-D KEEP-TAB-ORDER FONT 5 TITLE "Ingrese la Clave de Acceso" BGCOLOR 17 FGCOLOR 7 VIEW-AS DIALOG-BOX OVERLAY NO-UNDERLINE SCROLLABLE.

    ON CHOOSE OF Btn_Aceptar DO:
        IF W_Clave NE W_ClaTem THEN DO:
            RUN P-GraLog IN W_Manija (INPUT "Trato de Entrar con clave errada al programa: " + STRING(INTEGER(SELF:PRIVATE-DATA)) + " Pero su tiempo de trabajo expiro" + STRING(TIME,"HH:MM:SS AM")).
            RUN MostrarMensaje IN W_Manija (INPUT 37, OUTPUT W_Eleccion).
        END.
    END.

    FIND FIRST Programas WHERE Programas.Programa EQ INTEGER(SELF:PRIVATE-DATA) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Programas THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 41, OUTPUT W_Eleccion).
        ON RETURN RETURN.
        RETURN.
    END.

    W_NomArc = W_Path + "" + TRIM(Programas.Ejecutable).

    IF SEARCH(W_NomArc) EQ ? THEN DO:
        RUN MostrarMensaje IN W_Manija (INPUT 41, OUTPUT W_Eleccion).
        ON RETURN RETURN.
        RETURN.
    END.

    W_ClaTem = "".

    IF NOT Programas.Id_Clave THEN DO:
        ON RETURN TAB.

        W_Programa = SESSION:FIRST-PROCEDURE.
        W_Encontro = NO.

        DO WHILE VALID-HANDLE(W_Programa):
            IF W_Programa:FILE-NAME EQ TRIM(W_NomArc) THEN DO:
                ASSIGN Wk_Ventana = W_Programa:CURRENT-WINDOW.

                IF NOT VALID-HANDLE(Wk_Ventana) THEN
                    LEAVE.

                W_Encontro = YES.

                IF Wk_Ventana:WINDOW-STATE NE 3 THEN
                    ASSIGN Wk_Ventana:WINDOW-STATE = 3.

                ASSIGN W_Status = Wk_Ventana:MOVE-TO-TOP().
                APPLY "ENTRY" TO Wk_Ventana.
                LEAVE.
            END.

            ASSIGN W_Programa = W_Programa:NEXT-SIBLING.
        END.

        IF NOT W_Encontro THEN DO:
            DO TRANSACTION:
                FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-ERROR.
                IF AVAILABLE(Usuarios) THEN DO:
                    ASSIGN Usuarios.proactual = Programas.Opcion
                           Usuarios.Hentrada  = time.

                    FIND CURRENT Usuarios NO-LOCK NO-ERROR. /* oakley */
                END.

                FIND CURRENT Usuarios NO-LOCK NO-ERROR.
            END.

            FIND CURRENT Usuarios NO-LOCK NO-ERROR.
            RELEASE Usuarios NO-ERROR.

            RUN VALUE(TRIM(W_NomArc)) PERSISTENT SET W_Programa NO-ERROR.
            IF VALID-HANDLE(W_Programa) THEN DO:
                RUN escribirLog IN w_manija  (INPUT w_usuario,
                                              INPUT 'Ingresa a ' + programas.opcion) NO-ERROR.

                RUN dispatch IN W_Programa ('initialize') NO-ERROR.
            END.

            IF ERROR-STATUS:ERROR THEN
                RUN InitializeObject IN W_Programa NO-ERROR.
        END.

        VIEW W_ventana.
        RETURN.
    END.

    ON RETURN TAB.
    ENABLE ALL WITH FRAME F_Validacion IN WINDOW W_Ventana.

    ON LEAVE OF W_ClaTem DO:
        ON RETURN RETURN.  
        ASSIGN W_ClaTem.
    END.

    ON CHOOSE OF Btn_Cancelar DO:
        HIDE FRAME F_Validacion.
        W_ClaTem = W_Clave + "1".
    END.

    WAIT-FOR CHOOSE OF Btn_Cancelar OR CHOOSE OF Btn_Aceptar FOCUS W_ClaTem.

    HIDE FRAME F_Validacion.

    IF W_Clave = W_ClaTem THEN DO:
        ON RETURN TAB.

        ASSIGN W_Programa = SESSION:FIRST-PROCEDURE.

        w_Encontro = NO.

        DO WHILE VALID-HANDLE(W_Programa):
            IF W_Programa:FILE-NAME EQ TRIM(W_NomArc) THEN DO:
                ASSIGN Wk_Ventana = W_Programa:CURRENT-WINDOW.

                IF NOT VALID-HANDLE(Wk_Ventana) THEN
                    LEAVE.

                W_Encontro = YES.

                IF Wk_Ventana:WINDOW-STATE NE 3 THEN
                    ASSIGN Wk_Ventana:WINDOW-STATE = 3.

                ASSIGN W_Status = Wk_Ventana:MOVE-TO-TOP().

                APPLY "ENTRY" TO Wk_Ventana.
                LEAVE.
            END.

            ASSIGN W_Programa = W_Programa:NEXT-SIBLING.
        END.

        IF NOT W_Encontro THEN DO:
            RELEASE Usuarios NO-ERROR.

            FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
            
            RUN VALUE(TRIM(W_NomArc)) PERSISTENT SET W_Programa NO-ERROR.

            IF VALID-HANDLE(W_Programa) THEN DO:
                RUN escribirLog IN w_manija  (INPUT w_usuario,
                                              INPUT 'Ingresa a ' + programas.opcion) NO-ERROR.

                RUN dispatch IN W_Programa ('initialize') NO-ERROR.
            END.

            IF ERROR-STATUS:ERROR THEN
                RUN InitializeObject IN W_Programa NO-ERROR.
        END.

        VIEW W_ventana.
    END.

END PROCEDURE.


PROCEDURE EjecutarSalir:
    MESSAGE "Está seguro que desea salir del Sistema?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "    CONFIRMAR SALIR" UPDATE W_SiSalir AS LOGICAL.

    IF NOT W_Sisalir THEN
        RETURN NO-APPLY.

    ON ESC STOP.
    ON RETURN RETURN.

    RUN escribirLog IN W_Manija (INPUT w_usuario,
                                 INPUT "Sale del Sistema") NO-ERROR.
    
    FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario AND Usuarios.Agencia = W_Agencia NO-ERROR.
    IF AVAILABLE(Usuarios) THEN DO:
        Usuarios.Id_Entrada = FALSE.

        RELEASE Usuarios NO-ERROR.
    END.

    APPLY "CLOSE":U TO W_Ventana.
    ON CURSOR-DOWN CURSOR-DOWN.

    DELETE WIDGET W_MenuBarra.
    DELETE WIDGET W_ventana.
    
    QUIT.

END PROCEDURE.
