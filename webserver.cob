       IDENTIFICATION DIVISION.
       PROGRAM-ID. webserver.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-SOCKFD PIC 9(4).
       01 WS-CLIENT-SOCKFD PIC 9(4).
       01 WS-RESULT PIC 9(8).
       01 WS-SIGACTION.
           05 SIG-IGN PIC 9(4) BINARY VALUE 256.
       01 WS-SOCKADDR-IN.
      * AF_INET, Port 8080, 0.0.0.0
           05 SIN-FAMILY PIC 9(4) BINARY VALUE 512.
           05 SIN-PORT PIC 9(4) BINARY VALUE 8080.
           05 SIN-ADDR PIC 9(8) BINARY VALUE ZEROS.
           05 FILLER PIC X(8) VALUE ZEROS.
       01 WS-ADDRLEN PIC 9(4) VALUE 16.
       01 WS-SOCKCADDR-IN.
           05 SIN-FAMILY PIC 9(4) USAGE BINARY.
           05 SIN-PORT PIC 9(4) USAGE BINARY.
           05 SIN-ADDR PIC 9(8) USAGE BINARY.
           05 FILLER PIC X(8).
       01 WS-CADDRLEN PIC 9(4) VALUE 16.
       01 WS-BUFFER PIC X(100). 
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM SETUP-IGNORE-SIGPIPE.
           PERFORM SETUP-SOCKET.
       HANDLE-CLIENT-LOOP.
           PERFORM HANDLE-CLIENT.
           GO TO HANDLE-CLIENT-LOOP.
      * EXIT PROGRAM.
       SETUP-IGNORE-SIGPIPE.
      * IGNORE SIGPIPE signal
           CALL "sigaction"
           USING BY VALUE 13,
           BY REFERENCE WS-SIGACTION,
           BY REFERENCE NULL
           RETURNING WS-RESULT
           END-CALL
           IF WS-RESULT IS NOT ZERO
           THEN
               DISPLAY "sigaction call failed: ", WS-RESULT
               END-DISPLAY
               GOBACK
           END-IF.
       SETUP-SOCKET.
      * AF_INET, SOCK_STREAM, default prot
           CALL "socket" 
           USING BY VALUE 2, 1, 0
           RETURNING WS-SOCKFD
           END-CALL.
           IF WS-SOCKFD = -1
           THEN
               DISPLAY "socket call failed: ", WS-SOCKFD
               END-DISPLAY
               GOBACK
           END-IF.
           CALL "bind"
           USING BY VALUE WS-SOCKFD, 
           BY REFERENCE WS-SOCKADDR-IN,
           BY VALUE WS-ADDRLEN 
           RETURNING WS-RESULT
           END-CALL.
           IF WS-RESULT IS NOT ZERO
           THEN
               DISPLAY "bind call failed: ", WS-RESULT
               END-DISPLAY
               GOBACK
           END-IF.
           CALL "listen" 
           USING BY VALUE WS-SOCKFD, 50
           RETURNING WS-RESULT
           END-CALL.
           IF WS-RESULT IS NOT ZERO
           THEN
               DISPLAY "listen call failed: ", WS-RESULT
               END-DISPLAY
               GOBACK
           END-IF.
           DISPLAY "Listening on 0.0.0.0:8080"
           END-DISPLAY.
       HANDLE-CLIENT.
           MOVE 16 TO WS-CADDRLEN.
           CALL "accept" 
           USING BY VALUE WS-SOCKFD, 
           BY REFERENCE WS-SOCKCADDR-IN, 
           BY REFERENCE WS-CADDRLEN
           RETURNING WS-CLIENT-SOCKFD
           END-CALL.
           IF WS-CLIENT-SOCKFD = -1
           THEN
               DISPLAY "accept call failed: ", WS-CLIENT-SOCKFD
               END-DISPLAY
               GOBACK
           END-IF.

           MOVE "HTTP/1.1 200 OK" TO WS-BUFFER.
           CALL "write" 
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY REFERENCE WS-BUFFER,
           BY VALUE 15
           RETURNING WS-RESULT
           END-CALL.
           
           MOVE X"0D0A" TO WS-BUFFER.
           CALL "write" 
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY REFERENCE WS-BUFFER,
           BY VALUE 2
           RETURNING WS-RESULT
           END-CALL.
 
           MOVE "Content-Length: 12" TO WS-BUFFER.
           CALL "write" 
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY REFERENCE WS-BUFFER,
           BY VALUE 18
           RETURNING WS-RESULT
           END-CALL.
 
           MOVE X"0D0A0D0A" TO WS-BUFFER.
           CALL "write" 
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY REFERENCE WS-BUFFER,
           BY VALUE 4
           RETURNING WS-RESULT
           END-CALL.

           MOVE "Hello world!" TO WS-BUFFER.
           CALL "write" 
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY REFERENCE WS-BUFFER,
           BY VALUE 12
           RETURNING WS-RESULT
           END-CALL.

      * SHUT_RD
           CALL "shutdown"
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY VALUE 2
           END-CALL.

           CALL "close"
           USING BY VALUE WS-CLIENT-SOCKFD
           RETURNING WS-RESULT
           END-CALL.
       END PROGRAM webserver.
