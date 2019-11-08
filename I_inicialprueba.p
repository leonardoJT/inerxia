DISABLE TRIGGERS FOR LOAD OF solicitud.
FOR EACH solicitud:
    DELETE solicitud.
END.
DISABLE TRIGGERS FOR LOAD OF creditos.
FOR EACH creditos:
    DELETE creditos.
END.
DISABLE TRIGGERS FOR LOAD OF planpagos.
FOR EACH  planpagos:
    DELETE  planpagos.
END.
DISABLE TRIGGERS FOR LOAD OF mov_creditos.
FOR EACH  mov_creditos:
    DELETE  mov_creditos.
END.
DISABLE TRIGGERS FOR LOAD OF garantias.
FOR EACH  garantias:
    DELETE  garantias.
END.
DISABLE TRIGGERS FOR LOAD OF ahorros.
FOR EACH  ahorros:
    DELETE  ahorros.
END.
DISABLE TRIGGERS FOR LOAD OF mov_ahorros.
FOR EACH  mov_ahorros:
    DELETE  mov_ahorros.
END.
DISABLE TRIGGERS FOR LOAD OF lib_chequera.
FOR EACH  lib_chequera:
    DELETE  lib_chequera.
END.
DISABLE TRIGGERS FOR LOAD OF relaciones.
FOR EACH  relaciones.
    DELETE  relaciones.
END. 
DISABLE TRIGGERS FOR LOAD OF mov_contable.
FOR EACH  mov_contable:
    DELETE  mov_contable.
END.
DISABLE TRIGGERS FOR LOAD OF sal_cuenta.
FOR EACH  sal_cuenta:
    DELETE  sal_cuenta.
END.
DISABLE TRIGGERS FOR LOAD OF anexos.
FOR EACH  anexos:
    DELETE  anexos.
END.
DISABLE TRIGGERS FOR LOAD OF detalle.
FOR EACH  detalle:
    DELETE  detalle.
END.
DISABLE TRIGGERS FOR LOAD OF clientes.
FOR EACH clientes:
    DELETE clientes.
END.
DISABLE TRIGGERS FOR LOAD OF Hoja_vida.
FOR EACH  Hoja_vida:
    DELETE  Hoja_vida.
END.
DISABLE TRIGGERS FOR LOAD OF mov_instancia.
FOR EACH  mov_instancia:
    DELETE  mov_instancia.
END.
DISABLE TRIGGERS FOR LOAD OF LOGs.
FOR EACH  LOGs:
    DELETE  LOGs.
END.
DISABLE TRIGGERS FOR LOAD OF taquilla.
FOR EACH  taquilla:
    DELETE  taquilla.
END.
DISABLE TRIGGERS FOR LOAD OF scoring.
FOR EACH  scoring:
    DELETE  scoring.
END.


FOR EACH mov_gmf:
    DELETE mov_GMF.
END.

FOR EACH CHE_TRANSITO:
    DELETE CHE_TRANSITO.
END.

FOR EACH INVERSION_SDOs:
    DELETE INVERSION_SDOs.
END.

FOR EACH LISTANEGRA:
    DELETE LISTANEGRA.
END.

FOR EACH MOV_INVERSION:
    DELETE MOV_INVERSION.
END.

FOR EACH TOTAL_AGENCIA:
    DELETE TOTAL_agencia.
END.

/* configuraciones*/
/*DISABLE TRIGGERS FOR LOAD OF pro_ahorros.
FOR EACH pro_ahorros.
    DELETE pro_ahorros.
END.
DISABLE TRIGGERS FOR LOAD OF pro_creditos.
FOR EACH pro_creditos:
    DELETE pro_creditos.
END.
DISABLE TRIGGERS FOR LOAD OF liqui_int.
FOR EACH liqui_int:
    DELETE liqui_int.
END.
DISABLE TRIGGERS FOR LOAD OF carteravencida.
FOR EACH carteravencida:
    DELETE carteravencida.
END.
DISABLE TRIGGERS FOR LOAD OF cortolargo.
FOR EACH CORTOLARGO:
    DELETE cortolargo.
END.
DISABLE TRIGGERS FOR LOAD OF operacion.
FOR EACH operacion:
    DELETE operacion.
END.
DISABLE TRIGGERS FOR LOAD OF cuentas.
FOR EACH cuentas.
    DELETE CUENTAS.
END.
DISABLE TRIGGERS FOR LOAD OF comprobantes.
FOR EACH COMPROBANTES:
    DELETE COMPROBANTES.
END.*/
