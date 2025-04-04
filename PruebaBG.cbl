      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  BANDERA-SALIR           PIC X VALUE "N".
       01  OPCION                  PIC X.
       01  IDEMPLEADO              PIC 9 VALUE 1.
       01  IDEMPLEADOTEMP          PIC 9 VALUE 1.
       01  CEDULABUSCA             PIC X(10).

       01  EMPLEADO OCCURS 10 TIMES.
           05 CEDULA               PIC X(10).
           05 Nombre               PIC X(10).
           05 Salario              PIC 9999V99.
           05 DeduccionImpuestos   PIC 9999V99.
           05 DeduccionSeguro      PIC 9999V99.
           05 SalarioNeto          PIC 9999V99.
           05 DatosSocioeconomicos PIC X(10).

       01  DeduccionesGeneral      PIC 9999V99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL BANDERA-SALIR = "S"
            DISPLAY "[1] Ingresar nuevo empleado."
            DISPLAY "[2] Calcular salario neto."
            DISPLAY "[3] Mostrar información de empleado."
            DISPLAY "[4] Filtrar"
               DISPLAY "[4.1] Salario neto"
               DISPLAY "[4.2] Nivel educativo"
               DISPLAY "[4.3] Salario o nivel educativo"
            DISPLAY "[5] Salir."
            ACCEPT OPCION
               EVALUATE OPCION
                   WHEN '1'
                       PERFORM INGRESAR-EMPLEADO
                   WHEN '2'
                       PERFORM CALCULAR-NETO
                   WHEN '3'
                       PERFORM MOSTRAR-INFO
                   WHEN '5'
                       MOVE "S" TO BANDERA-SALIR
                   WHEN OTHER
                       DISPLAY "Opción inválida. Intente de nuevo."
            END-PERFORM.


       INGRESAR-EMPLEADO.

           DISPLAY "Ingrese ID del empleado:".
           ACCEPT CEDULA(IDEMPLEADO).

           DISPLAY "Ingrese Nombre del empleado:"
           ACCEPT Nombre(IDEMPLEADO).

           DISPLAY "Ingrese Salario bruto (número decimal)."
           ACCEPT Salario(IDEMPLEADO).
           DISPLAY "Ingrese Deducción por impuestos (número decimal)."
           ACCEPT DeduccionImpuestos(IDEMPLEADO).
           DISPLAY "Ingrese Deducción por seguro (número decimal)."
           ACCEPT DeduccionSeguro(IDEMPLEADO).
           DISPLAY "Ingrese DatosSocioeconómicos (número decimal)."
           ACCEPT DatosSocioeconomicos(IDEMPLEADO).

           ADD 1 TO IDEMPLEADO.

       CALCULAR-NETO.

           DISPLAY "INGRESE LA CEDULA DEL USUARIO A CALCULAR"
           ACCEPT CEDULABUSCA.

           PERFORM VARYING IDEMPLEADOTEMP FROM 1 BY 1 UNTIL
           IDEMPLEADOTEMP > 10
               IF CEDULA(IDEMPLEADOTEMP) = CEDULABUSCA
                   MOVE IDEMPLEADOTEMP TO IDEMPLEADO
               END-IF
               ADD 1 TO IDEMPLEADOTEMP
           END-PERFORM.

           ADD DeduccionSeguro(IDEMPLEADO)
           TO DeduccionImpuestos(IDEMPLEADO)
           GIVING DeduccionesGeneral.
           SUBTRACT Salario(IDEMPLEADO) FROM DeduccionesGeneral
           GIVING SalarioNeto(IDEMPLEADO).
           DISPLAY "El salario neto del usuario es",
           CEDULA(IDEMPLEADO), ":",SalarioNeto(IDEMPLEADO).


       MOSTRAR-INFO.
           DISPLAY "INGRESE LA CEDULA DEL USUARIO A CONSULTAR"
           ACCEPT IDEMPLEADO.

           DISPLAY "ID del empleado:",CEDULA(IDEMPLEADO).

           DISPLAY "Nombre del empleado:",Nombre(IDEMPLEADO).

           DISPLAY "Ingrese Salario bruto (número decimal)."
           ,Salario(IDEMPLEADO).
           DISPLAY "Deducción por impuestos (número decimal)."
           ,DeduccionImpuestos(IDEMPLEADO).
           DISPLAY "Deducción por seguro (número decimal)."
           ,DeduccionSeguro(IDEMPLEADO).
           DISPLAY "DatosSocioeconómicos (número decimal)."
           ,DatosSocioeconomicos(IDEMPLEADO).

           STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
