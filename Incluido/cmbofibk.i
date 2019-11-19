/* ************************************************************************
 * ARCHIVO: CmbOfi.I                                                      *
 * DESCRIPCION: Activa o desactiva el combo de Agencias, por el manejo de *
 *              superusuario.                                             *
 * AUTHOR: MDLCB                                                          *
 * FECHA: Mayo 4 de 1998                                                  * 
 * PROPIEDAD: REDECOOP                                                    *
 * CONSIDERACIONES:                                                       *
 *    se deben definir las siguientes vbles en definitions del programa   *
 *    DEFINE VAR W_Ofitbajo LIKE Agencias.Agencia.                        *
 *    DEFINE VAR W_OfStr     AS CHARACTER FORMAT "X(20)".                 * 
 *    DEFINE VAR W_SuperUsu  AS  LOGICAL.                                 *
 *    además, las vbles de uib de Agencia son:                            *
 *    para el combo w_cmbOfi, formato x(20), display = true,              *
 *                            enabled = true no-undo = true               *
 *    fill-in       W_Oficinap, activado unicamente display               *
 **************************************************************************/

  DEFINE VAR W_Metodo AS LOGICAL.
  W_OfiTbajo = W_Agencia.
  FOR EACH Agencias WHERE Agencias.Estado = 1 NO-LOCK:
      ASSIGN W_OfStr = STRING(Agencias.Agencia,"999") + 
             "-" + STRING(Agencias.Nombre,"X(16)")
             W_Metodo = W_CmbOfi:ADD-LAST(W_OfStr)   IN FRAME {&frame-name}.    
      IF Agencias.Agencia EQ W_Agencia THEN
         ASSIGN W_OficinaP = W_OfStr
                W_CmbOfi:SCREEN-VALUE IN FRAME {&frame-name} = W_OfStr.
  END.
  RUN SuperUsuario IN W_Manija (INPUT W_Agencia, INPUT W_Usuario, OUTPUT W_SuperUsu).  
  IF NOT W_SuperUsu THEN
/*     ASSIGN W_Oficinap:HIDDEN IN FRAME {&frame-name} = TRUE.
  ELSE*/
     ASSIGN W_Cmbofi:HIDDEN IN FRAME {&frame-name} = TRUE
            W_Oficinap:HIDDEN IN FRAME {&frame-name} = FALSE.
