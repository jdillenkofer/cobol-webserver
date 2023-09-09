       IDENTIFICATION DIVISION.
       PROGRAM-ID. cobol-webserver.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-SOCKFD PIC 9(4).
       01 WS-CLIENT-SOCKFD PIC 9(4).
       01 WS-RESULT PIC S9(32).
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
       01 WS-SENDFILE-OFFSET PIC 9(16) BINARY.
       01 WS-HTTP-LINE PIC X(4096).
       01 WS-HTTP-LINE-LEN PIC 9(8).
       01 WS-HTTP-LINE-SIZE PIC 9(8) VALUE 4096.
       01 WS-HTTP-REQUEST.
           05 HTTP-METHOD PIC X(8).
           05 PATH   PIC X(2083).
           05 PROTOCOL PIC X(16).
           05 HEADERS-LEN PIC 9(8).
           05 HEADERS-SIZE PIC 9(8) VALUE 512.
           05 HEADERS OCCURS 512 TIMES.
               06 HEADER-KEY PIC X(256).
               06 HEADER-VALUE PIC X(256).
       01 WS-HTTP-RESPONSE.
           05 HTTP-STATUS PIC 9(3).
           05 HTTP-STATUSTEXT PIC X(32).
       01 WS-FILEFD PIC S9(4).
       01 WS-FILENAME PIC X(32).
       01 WS-FILESIZE PIC 9(32).
       01 WS-FILESIZE-WITHOUT-LEADING-ZEROS PIC Z(31)9.
       01 WS-BUFFER PIC X(8192).
       01 WS-BUFFER2 PIC X(8192).
       01 WS-BUFFER-LEN PIC 9(8).
       01 WS-BUFFER-SIZE PIC 9(8) VALUE 8192.
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM SETUP-IGNORE-SIGCHLD.
           PERFORM SETUP-IGNORE-SIGPIPE.
           PERFORM SETUP-SOCKET.
           PERFORM FOREVER
               PERFORM HANDLE-CLIENT
           END-PERFORM.

       SETUP-IGNORE-SIGCHLD.
      * IGNORE SIGCHLD signal
           CALL "sigaction"
           USING BY VALUE 17,
           BY REFERENCE WS-SIGACTION,
           BY REFERENCE NULL
           RETURNING WS-RESULT
           END-CALL
           IF WS-RESULT NOT = ZERO
           THEN
               DISPLAY "sigaction call failed: ", WS-RESULT
               END-DISPLAY
               GOBACK
           END-IF.

       SETUP-IGNORE-SIGPIPE.
      * IGNORE SIGPIPE signal
           CALL "sigaction"
           USING BY VALUE 13,
           BY REFERENCE WS-SIGACTION,
           BY REFERENCE NULL
           RETURNING WS-RESULT
           END-CALL
           IF WS-RESULT NOT = ZERO
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
           IF WS-RESULT NOT = ZERO
           THEN
               DISPLAY "bind call failed: ", WS-RESULT
               END-DISPLAY
               GOBACK
           END-IF.
           CALL "listen" 
           USING BY VALUE WS-SOCKFD, 50
           RETURNING WS-RESULT
           END-CALL.
           IF WS-RESULT NOT = ZERO
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

           CALL "fork"
           RETURNING WS-RESULT
           END-CALL.
           IF WS-RESULT = -1
           THEN
               DISPLAY "fork call failed: ", WS-RESULT
               END-DISPLAY
               CALL "close"
               USING BY VALUE WS-CLIENT-SOCKFD
               RETURNING WS-RESULT
               END-CALL
               EXIT PARAGRAPH
           END-IF.
           IF WS-RESULT > 0
               EXIT PARAGRAPH
           END-IF.

      * Read incoming http request
           MOVE SPACES TO WS-BUFFER.
           CALL "read"
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY REFERENCE WS-BUFFER,
           BY VALUE WS-BUFFER-SIZE
           RETURNING WS-BUFFER-LEN
           END-CALL.

           PERFORM READ-HTTP-LINE.

           UNSTRING WS-HTTP-LINE
           DELIMITED BY ALL SPACES
           INTO HTTP-METHOD OF WS-HTTP-REQUEST,
           PATH OF WS-HTTP-REQUEST,
           PROTOCOL OF WS-HTTP-REQUEST
           END-UNSTRING.

           MOVE ZERO TO HEADERS-LEN OF WS-HTTP-REQUEST.
           PERFORM READ-HTTP-LINE
           PERFORM UNTIL WS-BUFFER-LEN = 0 OR WS-BUFFER(1:2) = X"0D0A"
                   OR HEADERS-LEN = HEADERS-SIZE
               COMPUTE
               HEADERS-LEN = HEADERS-LEN + 1
               END-COMPUTE

               UNSTRING WS-HTTP-LINE
               DELIMITED BY ": "
               INTO HEADER-KEY OF HEADERS(HEADERS-LEN),
               HEADER-VALUE OF HEADERS(HEADERS-LEN)
               END-UNSTRING
               PERFORM READ-HTTP-LINE
           END-PERFORM.

           IF WS-BUFFER(1:2) NOT = X"0D0A"
              AND HEADERS-LEN = HEADERS-SIZE
           THEN
      * We read all headers we could read, but there are still more...
      * Return status code 413 "Payload Too Large"
               MOVE 413 TO HTTP-STATUS OF WS-HTTP-RESPONSE
               PERFORM COMPUTE-STATUSTEXT-FROM-STATUS
               PERFORM SEND-STATUSCODE-AS-HTTP-RESPONSE
               GOBACK
           END-IF.
      * Consumes the last \r\n
           PERFORM READ-HTTP-LINE.

      * TODO: Consume body

           IF HTTP-METHOD OF WS-HTTP-REQUEST NOT = "GET"
           THEN
               MOVE 405 TO HTTP-STATUS OF WS-HTTP-RESPONSE
               PERFORM COMPUTE-STATUSTEXT-FROM-STATUS
               PERFORM SEND-STATUSCODE-AS-HTTP-RESPONSE
               GOBACK
           END-IF.

           IF  PROTOCOL OF WS-HTTP-REQUEST NOT = "HTTP/1.0"
           AND PROTOCOL OF WS-HTTP-REQUEST NOT = "HTTP/1.1"
           THEN
               MOVE "HTTP/1.1" TO PROTOCOL OF WS-HTTP-REQUEST
               MOVE 505 TO HTTP-STATUS OF WS-HTTP-RESPONSE
               PERFORM COMPUTE-STATUSTEXT-FROM-STATUS
               PERFORM SEND-STATUSCODE-AS-HTTP-RESPONSE
               GOBACK
           END-IF.

      * FIXME: This is vulnerable to path traversal
           IF PATH OF WS-HTTP-REQUEST = "/"
           THEN
               MOVE "index.html" TO WS-FILENAME
           ELSE
               MOVE SPACES TO WS-FILENAME
               UNSTRING PATH OF WS-HTTP-REQUEST
               DELIMITED BY "/"
               INTO WS-BUFFER, WS-FILENAME
               END-UNSTRING
           END-IF.

           STRING WS-FILENAME DELIMITED BY SPACE
           X"00" DELIMITED BY SIZE
           INTO WS-FILENAME
           END-STRING

           CALL "open" 
           USING BY REFERENCE WS-FILENAME,
           BY VALUE 0
           RETURNING WS-FILEFD
           END-CALL.

           IF WS-FILEFD = -1
           THEN
               MOVE 404 TO HTTP-STATUS OF WS-HTTP-RESPONSE
               PERFORM COMPUTE-STATUSTEXT-FROM-STATUS
               PERFORM SEND-STATUSCODE-AS-HTTP-RESPONSE
               GOBACK
           END-IF.
 
           MOVE 200 TO HTTP-STATUS OF WS-HTTP-RESPONSE.
           PERFORM COMPUTE-STATUSTEXT-FROM-STATUS.
           PERFORM SEND-FILE-AS-HTTP-RESPONSE.

           CALL "close"
           USING BY VALUE WS-FILEFD
           RETURNING WS-RESULT
           END-CALL.

           GOBACK.

       READ-HTTP-LINE.
           MOVE SPACES TO WS-HTTP-LINE.

           UNSTRING WS-BUFFER(1:WS-BUFFER-LEN)
           DELIMITED BY X"0D0A"
           INTO WS-HTTP-LINE
           COUNT IN WS-HTTP-LINE-LEN
           END-UNSTRING.
 
           COMPUTE
           WS-HTTP-LINE-LEN = WS-HTTP-LINE-LEN + 3
           END-COMPUTE.

           COMPUTE
           WS-BUFFER-LEN = WS-BUFFER-LEN - WS-HTTP-LINE-LEN + 1
           END-COMPUTE.

           MOVE WS-BUFFER(WS-HTTP-LINE-LEN:) TO WS-BUFFER2.
           COMPUTE
           WS-HTTP-LINE-LEN = WS-HTTP-LINE-LEN - 1
           END-COMPUTE.
           MOVE WS-BUFFER2 TO WS-BUFFER.

       COMPUTE-STATUSTEXT-FROM-STATUS.
      * Compute WS-STATUSTEXT from WS-STATUS
           EVALUATE HTTP-STATUS OF WS-HTTP-RESPONSE
               WHEN 100
                   MOVE "Continue"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 101
                   MOVE "Switching Protocols"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 102
                   MOVE "Processing"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 103
                   MOVE "Early Hints"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 200
                   MOVE "OK"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 201
                   MOVE "Created"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 202
                   MOVE "Accepted"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 203
                   MOVE "Non-Authoritative Information"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 204
                   MOVE "No Content"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 205
                   MOVE "Reset Content"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 206
                   MOVE "Partial Content"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 207
                   MOVE "Multi-Status"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 208
                   MOVE "Already Reported"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 226
                   MOVE "IM Used"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 300
                   MOVE "Multiple Choices"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 301
                   MOVE "Moved Permanently"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 302
                   MOVE "Found"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 303
                   MOVE "See Other"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 304
                   MOVE "Not Modified"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 305
                   MOVE "Use Proxy"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 306
                   MOVE "Switch Proxy"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 307
                   MOVE "Temporary Redirect"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 308
                   MOVE "Permanent Redirect"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 400
                   MOVE "Bad Request"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 401
                   MOVE "Unauthorized"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 402
                   MOVE "Payment Required"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 403
                   MOVE "Forbidden"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 404
                   MOVE "Not Found"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 405
                   MOVE "Method Not Allowed"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 406
                   MOVE "Not Acceptable"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 407
                   MOVE "Proxy Authentication Required"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 408
                   MOVE "Request Timeout"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 409
                   MOVE "Conflict"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 410
                   MOVE "Gone"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 411
                   MOVE "Length Required"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 412
                   MOVE "Precondition Failed"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 413
                   MOVE "Payload Too Large"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 414
                   MOVE "URI Too Long"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 415
                   MOVE "Unsupported Media Type"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 416
                   MOVE "Range Not Satisfiable"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 417
                   MOVE "Expectation Failed"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 418
                   MOVE "I'm a teapot"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 421
                   MOVE "Misdirected Request"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 422
                   MOVE "Unprocessable Entity"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 423
                   MOVE "Locked"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 424
                   MOVE "Failed Dependency"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 425
                   MOVE "Too Early"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 426
                   MOVE "Upgrade Required"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 428
                   MOVE "Precondition Required"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 429
                   MOVE "Too Many Requests"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 431
                   MOVE "Request Header Fields Too Large"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 451
                   MOVE "Unavailable For Legal Reasons"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 500
                   MOVE "Internal Server Error"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 501
                   MOVE "Not Implemented"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 502
                   MOVE "Bad Gateway"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 503
                   MOVE "Service Unavailable"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 504
                   MOVE "Gateway Timeout"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 505
                   MOVE "HTTP Version Not Supported"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 506
                   MOVE "Variant Also Negotiates"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 507
                   MOVE "Insufficient Storage"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 508
                   MOVE "Loop Detected"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 510
                   MOVE "Not Extended"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN 511
                   MOVE "Network Authentication Required"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
               WHEN OTHER
                   MOVE "Unknown"
                   TO HTTP-STATUSTEXT OF WS-HTTP-RESPONSE
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

       SEND-STATUSCODE-AS-HTTP-RESPONSE.
           MOVE SPACES TO WS-BUFFER.
           STRING PROTOCOL OF WS-HTTP-REQUEST " "
           HTTP-STATUS OF WS-HTTP-RESPONSE " " DELIMITED BY SIZE
           HTTP-STATUSTEXT OF WS-HTTP-RESPONSE DELIMITED BY SIZE
           INTO WS-BUFFER
           END-STRING.
           PERFORM WRITE-TO-CLIENT-SOCKET.

           MOVE X"0D0A" TO WS-BUFFER.
           PERFORM WRITE-TO-CLIENT-SOCKET.

           MOVE "Content-Length: 0" TO WS-BUFFER.

           PERFORM WRITE-TO-CLIENT-SOCKET.

           MOVE X"0D0A0D0A" TO WS-BUFFER.
           PERFORM WRITE-TO-CLIENT-SOCKET.

           PERFORM CLOSE-CLIENT-SOCKET.

       SEND-FILE-AS-HTTP-RESPONSE.
           MOVE SPACES TO WS-BUFFER.
           STRING PROTOCOL OF WS-HTTP-REQUEST " "
           HTTP-STATUS OF WS-HTTP-RESPONSE " " DELIMITED BY SIZE
           HTTP-STATUSTEXT OF WS-HTTP-RESPONSE DELIMITED BY SIZE
           INTO WS-BUFFER
           END-STRING.
           PERFORM WRITE-TO-CLIENT-SOCKET.

           MOVE X"0D0A" TO WS-BUFFER.
           PERFORM WRITE-TO-CLIENT-SOCKET.

           MOVE "Content-Type: text/html" TO WS-BUFFER.
           PERFORM WRITE-TO-CLIENT-SOCKET.

           MOVE X"0D0A" TO WS-BUFFER.
           PERFORM WRITE-TO-CLIENT-SOCKET.

           PERFORM COMPUTE-FILE-SIZE.

           MOVE WS-FILESIZE TO WS-FILESIZE-WITHOUT-LEADING-ZEROS.
           MOVE SPACES TO WS-BUFFER.
           STRING "Content-Length: " 
           FUNCTION TRIM(WS-FILESIZE-WITHOUT-LEADING-ZEROS LEADING)
           DELIMITED BY SIZE
           INTO WS-BUFFER
           END-STRING.

           PERFORM WRITE-TO-CLIENT-SOCKET.
           
           MOVE X"0D0A0D0A" TO WS-BUFFER.
           PERFORM WRITE-TO-CLIENT-SOCKET.

           PERFORM SEND-FILE-TO-CLIENT-SOCKET.

           PERFORM CLOSE-CLIENT-SOCKET.

       COMPUTE-BUFFER-LEN.
           MOVE ZERO TO  WS-BUFFER-LEN.
           INSPECT FUNCTION REVERSE (WS-BUFFER)
           TALLYING WS-BUFFER-LEN FOR LEADING SPACES.
           COMPUTE WS-BUFFER-LEN = LENGTH OF WS-BUFFER - WS-BUFFER-LEN
           END-COMPUTE.

       SEND-FILE-TO-CLIENT-SOCKET.
           MOVE ZERO TO WS-SENDFILE-OFFSET.
           MOVE ZERO TO WS-RESULT.
           PERFORM SEND-FILE-TO-CLIENT-SOCKET-LOOP 
           UNTIL WS-FILESIZE = 0 OR WS-RESULT = -1.

       SEND-FILE-TO-CLIENT-SOCKET-LOOP.
           CALL "sendfile64"
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY VALUE WS-FILEFD,
           BY REFERENCE WS-SENDFILE-OFFSET,
           BY VALUE WS-FILESIZE
           RETURNING WS-RESULT
           END-CALL.
           IF WS-RESULT = -1
           THEN
               DISPLAY "sendfile call failed: ", WS-RESULT
               END-DISPLAY
           ELSE
               COMPUTE WS-FILESIZE = WS-FILESIZE - WS-RESULT
               END-COMPUTE
           END-IF.

       WRITE-TO-CLIENT-SOCKET.
           PERFORM COMPUTE-BUFFER-LEN.
           MOVE ZERO TO WS-RESULT.
           PERFORM WRITE-TO-CLIENT-SOCKET-LOOP 
           UNTIL WS-BUFFER-LEN = 0 OR WS-RESULT = -1.

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
           END-IF.
           IF WS-RESULT > 0
           THEN
               MOVE SPACES TO WS-BUFFER2
               MOVE WS-BUFFER(WS-RESULT:) TO WS-BUFFER2
               MOVE WS-BUFFER2 TO WS-BUFFER
               COMPUTE WS-BUFFER-LEN = WS-BUFFER-LEN - WS-RESULT
               END-COMPUTE
           END-IF.

       CLOSE-CLIENT-SOCKET.
      * SHUT_WR
           CALL "shutdown"
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY VALUE 1
           END-CALL.

           CALL "close"
           USING BY VALUE WS-CLIENT-SOCKFD
           RETURNING WS-RESULT
           END-CALL.

       END PROGRAM cobol-webserver.
