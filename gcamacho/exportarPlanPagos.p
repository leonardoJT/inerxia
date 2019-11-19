


OUTPUT TO "C:\info_fodun\credito-32434942.d".
FOR EACH creditos WHERE nit EQ "32434942" AND num_credito EQ 10442 :
    EXPORT creditos.
END.
OUTPUT CLOSE.

OUTPUT TO "C:\info_fodun\CONTROL_pago-32434942.d".
FOR EACH CONTROL_pago WHERE CONTROL_pago.nit EQ "32434942" AND 
                     CONTROL_pago.num_credito = 10442:
    EXPORT CONTROL_pago.
END.
OUTPUT CLOSE.

OUTPUT TO "C:\info_fodun\planpagos-32434942.d".
FOR EACH planpagos WHERE planPagos.nit = "32434942"
                     AND planPagos.num_credito = 10442:
    EXPORT planPagos.
END.
OUTPUT CLOSE.
