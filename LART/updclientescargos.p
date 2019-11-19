
FOR EACH clientes :
/*     UPDATE Tipo_Actividad = "Empleado Público" */
/*             Clientes.cod_actividad = "474"     */
/*             Clientes.Cod_Profesion = 154.      */

    FIND FIRST anexos_clientes WHERE anexos_clientes.nit EQ clientes.nit NO-ERROR.
    IF AVAILABLE anexos_clientes THEN
        UPDATE  Anexos_Clientes.Acti_Economica_Emp = "8050".

END.
