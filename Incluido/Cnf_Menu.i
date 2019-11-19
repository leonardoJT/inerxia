PROCEDURE  Arma_Menu_Grupo:
    W_Bandera = NO.

    CREATE MENU W_MenuBarra.

    FOR EACH Pro_Grupo WHERE Pro_Grupo.Grupo EQ W_Grupo
                         AND Pro_Grupo.Nivel EQ 1 NO-LOCK BY Pro_Grupo.Orden:
        FIND FIRST Programas WHERE Programas.Programa EQ Pro_Grupo.Programa NO-LOCK NO-ERROR.
        IF AVAILABLE Programas THEN
            W_Ejecutable = Programas.Ejecutable.

        RUN Arma_Menu (BUFFER Pro_Grupo).

        RUN Recursivo (INPUT Pro_Grupo.Nivel + 1,
                       INPUT Pro_Grupo.Orden,
                       INPUT Pro_Grupo.Padre,
                       INPUT Pro_Grupo.Orden).

        IF W_Bandera THEN
            NEXT.

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

END PROCEDURE.


PROCEDURE Arma_Menu:
    DEFINE PARAMETER BUFFER Arbol FOR Pro_Grupo.
    DEFINE VARIABLE W_Origen_Rama LIKE Pro_Grupo.Origen_Rama.

    IF Arbol.Nivel EQ 1 THEN
        W_Origen_Rama = Arbol.Orden.
    ELSE
        W_Origen_Rama = Arbol.Origen_Rama.

    IF Arbol.Raya THEN DO:
        CREATE MENU-ITEM W_MenuItem
               ASSIGN SUBTYPE = "RULE"
                      PARENT = IF Arbol.Nivel EQ 1 THEN W_MenuBarra ELSE W_SubMenu[Arbol.Nivel - 1].

        RETURN.
    END.

    FIND FIRST Programas WHERE Programas.Programa EQ Arbol.Programa NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Programas THEN
        NEXT.

    FIND FIRST Hijo_Pro_Grupo WHERE Hijo_Pro_Grupo.Grupo EQ W_Grupo
                                AND Hijo_Pro_Grupo.Nivel EQ (Arbol.Nivel + 1)
                                AND Hijo_Pro_Grupo.Origen EQ W_Origen_Rama
                                AND Hijo_Pro_Grupo.Abuelo EQ Arbol.Padre
                                AND Hijo_Pro_Grupo.Padre  EQ Arbol.Orden NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Hijo_Pro_Grupo AND Programa.Tipo THEN DO:
        IF Arbol.Nivel EQ 1 THEN DO:
            CREATE MENU-ITEM W_MenuItem
                ASSIGN PARENT = IF Arbol.Nivel EQ 1 THEN W_MenuBarra ELSE W_SubMenu[Arbol.Nivel - 1]
                       LABEL = Programas.Opcion
                       PRIVATE-DATA = STRING(Programa.Programa)

                    TRIGGERS:
                        ON CHOOSE PERSISTENT RUN EjecutarVentana IN THIS-PROCEDURE.
                    END TRIGGERS.
        END.
        ELSE DO:
            IF VALID-HANDLE(W_SubMenu[Arbol.Nivel - 1]) THEN DO:
                CREATE MENU-ITEM W_MenuItem
                    ASSIGN PARENT = IF Arbol.Nivel EQ 1 THEN W_MenuBarra ELSE W_SubMenu[Arbol.Nivel - 1]
                           LABEL = Programas.Opcion
                           PRIVATE-DATA = STRING(Programa.Programa)

                        TRIGGERS:
                            ON CHOOSE PERSISTENT RUN EjecutarVentana IN THIS-PROCEDURE.
                        END TRIGGERS.
            END.
        END.
    END.
    ELSE DO:
        IF W_Ejecutable EQ "w-taquil.r" OR W_Ejecutable EQ "w-sercli.r" THEN DO:
            CREATE MENU-ITEM W_MenuItem
                ASSIGN PARENT = IF Arbol.Nivel EQ 1 THEN W_MenuBarra ELSE W_SubMenu[Arbol.Nivel - 1]
                       LABEL  = Programas.Opcion
                       PRIVATE-DATA = STRING(Programa.Programa)

                    TRIGGERS:
                        ON CHOOSE PERSISTENT RUN EjecutarVentana IN THIS-PROCEDURE.
                    END TRIGGERS.
        END.
        ELSE DO:
            CREATE SUB-MENU W_SubMenu[Arbol.Nivel]
                   ASSIGN PARENT = IF Arbol.Nivel EQ 1 THEN W_MenuBarra ELSE W_SubMenu[Arbol.Nivel - 1]
                          LABEL = Programas.Opcion.
        END.
    END.

END PROCEDURE.


PROCEDURE Recursivo :
    DEFINE INPUT PARAMETER P_Nivel LIKE Pro_Grupo.Nivel.
    DEFINE INPUT PARAMETER P_Origen_Rama LIKE Pro_Grupo.Origen_Rama.
    DEFINE INPUT PARAMETER P_Abuelo LIKE Pro_Grupo.Abuelo.
    DEFINE INPUT PARAMETER P_Padre LIKE Pro_Grupo.Padre.

    FOR EACH Tmp_Pro_Grupo WHERE Tmp_Pro_Grupo.Grupo EQ W_Grupo
                             AND Tmp_Pro_Grupo.Nivel EQ P_Nivel
                             AND Tmp_Pro_Grupo.Origen_Rama EQ P_Origen_Rama
                             AND Tmp_Pro_Grupo.Abuelo EQ P_Abuelo
                             AND Tmp_Pro_Grupo.Padre EQ P_Padre NO-LOCK BY Tmp_Pro_Grupo.Orden:
        FIND FIRST Programas WHERE Programas.Programa EQ Tmp_Pro_Grupo.Programa NO-LOCK NO-ERROR.
        IF AVAILABLE Programas THEN
            W_Ejecutable = Programas.Ejecutable.

        RUN Arma_Menu(BUFFER Tmp_Pro_Grupo).
        
        RUN Recursivo (INPUT Tmp_Pro_Grupo.Nivel + 1,
                       INPUT Tmp_Pro_Grupo.Origen_Rama,
                       INPUT Tmp_Pro_Grupo.Padre,
                       INPUT Tmp_Pro_Grupo.Orden).
    END.

END PROCEDURE.

