CONNECT -db desarrollo -H localhost -S 7000.

    FOR EACH ahorros WHERE estado = 1 NO-LOCK:
        DISPLAY ahorros WITH WIDTH 200 1 COL.
    END.

DISCONNECT desarrollo.
