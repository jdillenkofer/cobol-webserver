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
       01 WS-FILESIZE PIC 9(16).
       01 WS-FILESIZE2 PIC Z(15)9.
       01 WS-BUFFER PIC X(4096).
       01 WS-BUFFER2 PIC X(4096).
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
      * Compute WS-STATUSTEXT from WS-STATUS
           EVALUATE WS-STATUS
               WHEN 100
                   MOVE "Continue" TO WS-STATUSTEXT
               WHEN 101
                   MOVE "Switching Protocols" TO WS-STATUSTEXT
               WHEN 102
                   MOVE "Processing" TO WS-STATUSTEXT
               WHEN 103
                   MOVE "Early Hints" TO WS-STATUSTEXT
               WHEN 200
                   MOVE "OK" TO WS-STATUSTEXT
               WHEN 201
                   MOVE "Created" TO WS-STATUSTEXT
               WHEN 202
                   MOVE "Accepted" TO WS-STATUSTEXT
               WHEN 203
                   MOVE "Non-Authoritative Information" TO WS-STATUSTEXT
               WHEN 204
                   MOVE "No Content" TO WS-STATUSTEXT
               WHEN 205
                   MOVE "Reset Content" TO WS-STATUSTEXT
               WHEN 206
                   MOVE "Partial Content" TO WS-STATUSTEXT
               WHEN 207
                   MOVE "Multi-Status" TO WS-STATUSTEXT
               WHEN 208
                   MOVE "Already Reported" TO WS-STATUSTEXT
               WHEN 226
                   MOVE "IM Used" TO WS-STATUSTEXT
               WHEN 300
                   MOVE "Multiple Choices" TO WS-STATUSTEXT
               WHEN 301
                   MOVE "Moved Permanently" TO WS-STATUSTEXT
               WHEN 302
                   MOVE "Found" TO WS-STATUSTEXT
               WHEN 303
                   MOVE "See Other" TO WS-STATUSTEXT
               WHEN 304
                   MOVE "Not Modified" TO WS-STATUSTEXT
               WHEN 305
                   MOVE "Use Proxy" TO WS-STATUSTEXT
               WHEN 306
                   MOVE "Switch Proxy" TO WS-STATUSTEXT
               WHEN 307
                   MOVE "Temporary Redirect" TO WS-STATUSTEXT
               WHEN 308
                   MOVE "Permanent Redirect" TO WS-STATUSTEXT
               WHEN 400
                   MOVE "Bad Request" TO WS-STATUSTEXT
               WHEN 401
                   MOVE "Unauthorized" TO WS-STATUSTEXT
               WHEN 402
                   MOVE "Payment Required" TO WS-STATUSTEXT
               WHEN 403
                   MOVE "Forbidden" TO WS-STATUSTEXT
               WHEN 404
                   MOVE "Not Found" TO WS-STATUSTEXT
               WHEN 405
                   MOVE "Method Not Allowed" TO WS-STATUSTEXT
               WHEN 406
                   MOVE "Not Acceptable" TO WS-STATUSTEXT
               WHEN 407
                   MOVE "Proxy Authentication Required" TO WS-STATUSTEXT
               WHEN 408
                   MOVE "Request Timeout" TO WS-STATUSTEXT
               WHEN 409
                   MOVE "Conflict" TO WS-STATUSTEXT
               WHEN 410
                   MOVE "Gone" TO WS-STATUSTEXT
               WHEN 411
                   MOVE "Length Required" TO WS-STATUSTEXT
               WHEN 412
                   MOVE "Precondition Failed" TO WS-STATUSTEXT
               WHEN 413
                   MOVE "Payload Too Large" TO WS-STATUSTEXT
               WHEN 414
                   MOVE "URI Too Long" TO WS-STATUSTEXT
               WHEN 415
                   MOVE "Unsupported Media Type" TO WS-STATUSTEXT
               WHEN 416
                   MOVE "Range Not Satisfiable" TO WS-STATUSTEXT
               WHEN 417
                   MOVE "Expectation Failed" TO WS-STATUSTEXT
               WHEN 418
                   MOVE "I'm a teapot" TO WS-STATUSTEXT
               WHEN 421
                   MOVE "Misdirected Request" TO WS-STATUSTEXT
               WHEN 422
                   MOVE "Unprocessable Entity" TO WS-STATUSTEXT
               WHEN 423
                   MOVE "Locked" TO WS-STATUSTEXT
               WHEN 424
                   MOVE "Failed Dependency" TO WS-STATUSTEXT
               WHEN 425
                   MOVE "Too Early" TO WS-STATUSTEXT
               WHEN 426
                   MOVE "Upgrade Required" TO WS-STATUSTEXT
               WHEN 428
                   MOVE "Precondition Required" TO WS-STATUSTEXT
               WHEN 429
                   MOVE "Too Many Requests" TO WS-STATUSTEXT
               WHEN 431
                   MOVE "Request Header Fields Too Large" 
                   TO WS-STATUSTEXT
               WHEN 451
                   MOVE "Unavailable For Legal Reasons" TO WS-STATUSTEXT
               WHEN 500
                   MOVE "Internal Server Error" TO WS-STATUSTEXT
               WHEN 501
                   MOVE "Not Implemented" TO WS-STATUSTEXT
               WHEN 502
                   MOVE "Bad Gateway" TO WS-STATUSTEXT
               WHEN 503
                   MOVE "Service Unavailable" TO WS-STATUSTEXT
               WHEN 504
                   MOVE "Gateway Timeout" TO WS-STATUSTEXT
               WHEN 505
                   MOVE "HTTP Version Not Supported" TO WS-STATUSTEXT
               WHEN 506
                   MOVE "Variant Also Negotiates" TO WS-STATUSTEXT
               WHEN 507
                   MOVE "Insufficient Storage" TO WS-STATUSTEXT
               WHEN 508
                   MOVE "Loop Detected" TO WS-STATUSTEXT
               WHEN 510
                   MOVE "Not Extended" TO WS-STATUSTEXT
               WHEN 511
                   MOVE "Network Authentication Required"
                   TO WS-STATUSTEXT
               WHEN OTHER
                   MOVE "Unknown" TO WS-STATUSTEXT
           END-EVALUATE.
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
           WS-STATUSTEXT DELIMITED BY SIZE
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
      * TODO: Handle case if sendfile does not send the entire file
           CALL "sendfile64"
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY VALUE WS-FILEFD,
           BY REFERENCE NULL,
           BY VALUE WS-FILESIZE
           RETURNING WS-RESULT
           END-CALL.
           IF WS-RESULT = -1
           THEN
               DISPLAY "sendfile call failed: ", WS-RESULT
               END-DISPLAY
               GOBACK
           END-IF.
       WRITE-TO-CLIENT-SOCKET.
           PERFORM COMPUTE-BUFFER-LEN.
           PERFORM WRITE-TO-CLIENT-SOCKET-LOOP.
       WRITE-TO-CLIENT-SOCKET-LOOP.
           CALL "write" 
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY REFERENCE WS-BUFFER,
           BY VALUE WS-BUFFER-LEN
           RETURNING WS-RESULT
           END-CALL.
           IF WS-RESULT = -1
           THEN
               DISPLAY "write call failed: ", WS-RESULT
               END-DISPLAY
               GOBACK
           END-IF.
           IF WS-RESULT < WS-BUFFER-LEN
           THEN
               DISPLAY "write call returned before writing all bytes."
               END-DISPLAY
               MOVE WS-BUFFER(WS-RESULT:) TO WS-BUFFER2
               MOVE WS-BUFFER2 TO WS-BUFFER
               COMPUTE WS-BUFFER-LEN = WS-BUFFER-LEN - WS-RESULT
               END-COMPUTE
               GO TO WRITE-TO-CLIENT-SOCKET-LOOP
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
