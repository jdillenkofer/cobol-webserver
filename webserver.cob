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
       01 WS-STATUS PIC 9(3).
       01 WS-STATUSTEXT PIC X(32).
       01 WS-FILEFD PIC 9(4).
       01 WS-FILENAME PIC X(32).
       01 WS-FILESIZE PIC 9(8).
       01 WS-FILESIZE2 PIC Z(7)9.
       01 WS-BUFFER PIC X(4096).
       01 WS-BUFFER-LEN PIC 9(8).
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

           MOVE SPACES TO WS-FILENAME.
           STRING "index.html" X"00" DELIMITED BY SIZE
           INTO WS-FILENAME
           END-STRING.

           MOVE 200 TO WS-STATUS.
           PERFORM COMPUTE-STATUSTEXT-FROM-STATUS.
           PERFORM SEND-FILE-AS-HTTP-RESPONSE.

       COMPUTE-STATUSTEXT-FROM-STATUS.
      * TODO: Compute WS-STATUSTEXT from WS-STATUS
           MOVE "OK" TO WS-STATUSTEXT.
       COMPUTE-FILE-SIZE.
      * fseek to SEEK_END
           CALL "lseek"
           USING BY VALUE WS-FILEFD,
           BY VALUE 0,
           BY VALUE 2
           RETURNING WS-FILESIZE
           END-CALL.

           CALL "lseek"
           USING BY VALUE WS-FILEFD,
           BY VALUE 0,
           BY VALUE 0
           END-CALL.

       SEND-FILE-AS-HTTP-RESPONSE.
           MOVE SPACES TO WS-BUFFER.
           STRING "HTTP/1.1 " WS-STATUS " " DELIMITED BY SIZE
           WS-STATUSTEXT DELIMITED BY SPACE
           INTO WS-BUFFER
           END-STRING.
           PERFORM WRITE-TO-CLIENT-SOCKET.

           MOVE X"0D0A" TO WS-BUFFER.
           PERFORM WRITE-TO-CLIENT-SOCKET.

           MOVE "Content-Type: text/html" TO WS-BUFFER.
           PERFORM WRITE-TO-CLIENT-SOCKET.

           MOVE X"0D0A" TO WS-BUFFER.
           PERFORM WRITE-TO-CLIENT-SOCKET.

           CALL "open" 
           USING BY REFERENCE WS-FILENAME,
           BY VALUE 0
           RETURNING WS-FILEFD
           END-CALL.

           PERFORM COMPUTE-FILE-SIZE.

           MOVE WS-FILESIZE TO WS-FILESIZE2.
           MOVE SPACES TO WS-BUFFER.
           STRING "Content-Length: " 
           FUNCTION TRIM(WS-FILESIZE2, LEADING) DELIMITED BY SIZE
           INTO WS-BUFFER
           END-STRING.

           PERFORM WRITE-TO-CLIENT-SOCKET.
           
           MOVE X"0D0A0D0A" TO WS-BUFFER.
           PERFORM WRITE-TO-CLIENT-SOCKET.

      *    CALL "read"
      *    USING BY VALUE WS-FILEFD,
      *     BY REFERENCE WS-BUFFER,
      *     BY VALUE LENGTH OF WS-BUFFER 
      *     RETURNING WS-RESULT
      *     END-CALL.


      *    PERFORM WRITE-TO-CLIENT-SOCKET.
           PERFORM SEND-FILE-TO-CLIENT-SOCKET.

           CALL "close"
           USING BY VALUE WS-FILEFD
           RETURNING WS-RESULT
           END-CALL.

           PERFORM CLOSE-CLIENT-SOCKET.

       COMPUTE-BUFFER-LEN.
           MOVE ZERO TO  WS-BUFFER-LEN.
           INSPECT FUNCTION REVERSE (WS-BUFFER)
           TALLYING WS-BUFFER-LEN FOR LEADING SPACES.
           COMPUTE WS-BUFFER-LEN = LENGTH OF WS-BUFFER - WS-BUFFER-LEN
           END-COMPUTE.
       SEND-FILE-TO-CLIENT-SOCKET.
           CALL "sendfile"
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY VALUE WS-FILEFD,
           BY REFERENCE NULL,
           BY VALUE WS-FILESIZE
           RETURNING WS-RESULT
           END-CALL.
           IF WS-RESULT NOT = WS-FILESIZE
           THEN
               DISPLAY "sendfile call failed: ", WS-RESULT
               END-DISPLAY
               GOBACK
           END-IF.
       WRITE-TO-CLIENT-SOCKET.
           PERFORM COMPUTE-BUFFER-LEN.
           CALL "write" 
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY REFERENCE WS-BUFFER,
           BY VALUE WS-BUFFER-LEN
           RETURNING WS-RESULT
           END-CALL.
           IF WS-RESULT NOT = WS-BUFFER-LEN
           THEN
               DISPLAY "write call failed: ", WS-RESULT
               END-DISPLAY
               GOBACK
           END-IF.
       CLOSE-CLIENT-SOCKET.
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
