       IDENTIFICATION DIVISION.
       PROGRAM-ID. cobol-webserver.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-KEEP-RUNNING PIC X EXTERNAL.
       01 WS-ALRM-WAS-RAISED PIC X EXTERNAL.
       01 WS-SOCKFD PIC 9(4).
       01 WS-CLIENT-SOCKFD PIC 9(4).
       01 WS-TEMP PIC S9(32).
       01 WS-TEMP2 PIC S9(32).
       01 WS-TEMP3 PIC S9(32).
       01 WS-I PIC 9(32).
       01 WS-NUM-TRAILING-SPACES PIC 9(20).
       01 WS-NUM-HEX-DIGITS PIC 9(20).
       01 WS-TEXT PIC X(16).
       01 WS-HEX-STRING PIC X(16).
       01 FILLER PIC 9 VALUE 0.
       01 WS-NUMERIC-VALUE PIC 9(20).
       01 WS-HEX-CHAR PIC X.
       01 WS-HEX-DIGITS CONSTANT "0123456789ABCDEF".
       01 WS-HEX-VALUE PIC 9(2).
       01 WS-SIGACTION-IGNORE.
           05 SIG-IGN PIC 9(4) BINARY VALUE 256.
       01 WS-SIGACTION-STRUCT.
           05 SA-HANDLER USAGE PROGRAM-POINTER.
           05 SA-SIGACTION USAGE PROGRAM-POINTER.
           05 SA-MASK    PIC X(128) VALUE ZEROS.
           05 SA-FLAGS   PIC 9(8) BINARY VALUE ZEROS.
           05 FILLER PIC X(12) VALUE ZEROS.
       01 WS-HANDLE-SIGINT-PROCNAME PIC X(20) VALUE "sigint-handler".
       01 WS-HANDLE-SIGALRM-PROCNAME PIC X(20)
           VALUE "sigalrm-handler".
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
       01 WS-HTTP-REQUEST-COUNTER PIC 9(8) VALUE 0.
       01 WS-HTTP-REQUEST.
           05 HTTP-METHOD PIC X(8).
           05 PATH   PIC X(2083).
           05 PROTOCOL PIC X(16).
           05 HEADERS-LEN PIC 9(8).
           05 HEADERS-SIZE PIC 9(8) VALUE 512.
           05 HEADERS OCCURS 512 TIMES.
               06 HEADER-KEY PIC X(256).
               06 HEADER-VALUE PIC X(256).
           05 IS-TRANSFER-ENCODING-CHUNKED PIC X VALUE 'N'.
           05 CONTENT-LENGTH PIC 9(8).
           05 CHUNK-LENGTH PIC 9(20).
           05 REMAINING-CONTENT-LENGTH PIC 9(8).
       01 WS-HTTP-RESPONSE.
           05 HTTP-STATUS PIC 9(3).
           05 HTTP-STATUSTEXT PIC X(32).
           05 HEADERS-LEN PIC 9(8).
           05 HEADERS-SIZE PIC 9(8) VALUE 512.
           05 HEADERS OCCURS 512 TIMES.
               06 HEADER-KEY PIC X(256).
               06 HEADER-VALUE PIC X(256).
       01 WS-FILEFD PIC S9(4).
       01 WS-FILENAME PIC X(256).
       01 WS-FILENAME-NULLTERMINATED PIC X(256).
       01 WS-FILESIZE PIC 9(32).
       01 WS-FILESIZE-WITHOUT-LEADING-ZEROS PIC Z(31)9.
       01 WS-FILESUFFIX PIC X(256).
       01 WS-CONTENT-TYPE PIC X(256).
       01 WS-BUFFER PIC X(8192).
       01 WS-TEMP-BUFFER PIC X(8192).
       01 WS-BUFFER-LEN PIC 9(8).
       01 WS-BUFFER-SIZE PIC 9(8) VALUE 8192.
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM SETUP-IGNORE-SIGCHLD.
           PERFORM SETUP-IGNORE-SIGPIPE.
           PERFORM SETUP-HANDLE-SIGINT.
           PERFORM SETUP-SOCKET.
           MOVE 'Y' TO WS-KEEP-RUNNING.
           PERFORM UNTIL WS-KEEP-RUNNING = 'N'
               PERFORM HANDLE-CLIENT
           END-PERFORM.
           PERFORM CLEANUP-SOCKET.
           STOP RUN.

       SETUP-IGNORE-SIGCHLD.
      * IGNORE SIGCHLD signal
           CALL "sigaction"
           USING BY VALUE 17,
           BY REFERENCE WS-SIGACTION-IGNORE,
           BY REFERENCE NULL
           RETURNING WS-TEMP
           END-CALL
           IF WS-TEMP NOT = ZERO
           THEN
               DISPLAY "sigaction call failed: ", WS-TEMP
               END-DISPLAY
               GOBACK
           END-IF.

       SETUP-IGNORE-SIGPIPE.
      * IGNORE SIGPIPE signal
           CALL "sigaction"
           USING BY VALUE 13,
           BY REFERENCE WS-SIGACTION-IGNORE,
           BY REFERENCE NULL
           RETURNING WS-TEMP
           END-CALL
           IF WS-TEMP NOT = ZERO
           THEN
               DISPLAY "sigaction call failed: ", WS-TEMP
               END-DISPLAY
               GOBACK
           END-IF.

       SETUP-HANDLE-SIGINT.
      * Handle SIGINT signal
           SET SA-HANDLER OF WS-SIGACTION-STRUCT
           TO ENTRY WS-HANDLE-SIGINT-PROCNAME.

           SET SA-SIGACTION OF WS-SIGACTION-STRUCT
           TO NULL.

           CALL "sigfillset"
           USING BY REFERENCE WS-TEMP-BUFFER
           END-CALL.
           MOVE WS-TEMP-BUFFER(1:128) TO SA-MASK OF WS-SIGACTION-STRUCT.
           MOVE ZEROS TO SA-FLAGS OF WS-SIGACTION-STRUCT.

           CALL "sigaction"
           USING BY VALUE 2,
           BY REFERENCE WS-SIGACTION-STRUCT,
           BY REFERENCE NULL
           RETURNING WS-TEMP
           END-CALL

           IF WS-TEMP NOT = ZERO
           THEN
               DISPLAY "sigaction call failed: ", WS-TEMP
               END-DISPLAY
               GOBACK
           END-IF.

       SETUP-HANDLE-SIGALRM.
      * Handle SIGALRM signal
           SET SA-HANDLER OF WS-SIGACTION-STRUCT
           TO ENTRY WS-HANDLE-SIGALRM-PROCNAME.

           SET SA-SIGACTION OF WS-SIGACTION-STRUCT
           TO NULL.

           CALL "sigfillset"
           USING BY REFERENCE WS-TEMP-BUFFER
           END-CALL.
           MOVE WS-TEMP-BUFFER(1:128) TO SA-MASK OF WS-SIGACTION-STRUCT.
           MOVE ZEROS TO SA-FLAGS OF WS-SIGACTION-STRUCT.

           CALL "sigaction"
           USING BY VALUE 14,
           BY REFERENCE WS-SIGACTION-STRUCT,
           BY REFERENCE NULL
           RETURNING WS-TEMP
           END-CALL

           IF WS-TEMP NOT = ZERO
           THEN
               DISPLAY "sigaction call failed: ", WS-TEMP
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
           RETURNING WS-TEMP
           END-CALL.
           IF WS-TEMP NOT = ZERO
           THEN
               DISPLAY "bind call failed: ", WS-TEMP
               END-DISPLAY
               GOBACK
           END-IF.
           CALL "listen" 
           USING BY VALUE WS-SOCKFD, 50
           RETURNING WS-TEMP
           END-CALL.
           IF WS-TEMP NOT = ZERO
           THEN
               DISPLAY "listen call failed: ", WS-TEMP
               END-DISPLAY
               GOBACK
           END-IF.
           DISPLAY "Listening on 0.0.0.0:8080"
           END-DISPLAY.

       CLEANUP-SOCKET.
           CALL "close"
           USING BY VALUE WS-SOCKFD
           RETURNING WS-TEMP
           END-CALL.

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
               EXIT PARAGRAPH
           END-IF.

           IF WS-KEEP-RUNNING = 'N'
           THEN
             EXIT PARAGRAPH
           END-IF.

           COMPUTE
           WS-HTTP-REQUEST-COUNTER = WS-HTTP-REQUEST-COUNTER + 1
           END-COMPUTE.

           CALL "fork"
           RETURNING WS-TEMP
           END-CALL.
           IF WS-TEMP = -1
           THEN
               DISPLAY "fork call failed: ", WS-TEMP
               END-DISPLAY
               CALL "close"
               USING BY VALUE WS-CLIENT-SOCKFD
               RETURNING WS-TEMP
               END-CALL
               EXIT PARAGRAPH
           END-IF.
           IF WS-TEMP > 0
               EXIT PARAGRAPH
           END-IF.

      * We set an alarm for 10 sec in case the
      * requestor never sends us the entire http request
           MOVE "N" TO WS-ALRM-WAS-RAISED.
           PERFORM SETUP-HANDLE-SIGALRM.
           CALL "alarm"
           USING BY VALUE 10
           RETURNING WS-TEMP
           END-CALL.

      * Read incoming http request
           MOVE SPACES TO WS-BUFFER.
           MOVE ZERO TO WS-BUFFER-LEN.
           PERFORM READ-FROM-SOCKET-AND-FILL-WS-BUFFER-WITH-TIMEOUT.

           PERFORM READ-HTTP-LINE.

           UNSTRING WS-HTTP-LINE
           DELIMITED BY ALL SPACES
           INTO HTTP-METHOD OF WS-HTTP-REQUEST,
           PATH OF WS-HTTP-REQUEST,
           PROTOCOL OF WS-HTTP-REQUEST
           END-UNSTRING.

           PERFORM PROCESS-HTTP-HEADERS.

           IF WS-BUFFER(1:2) NOT = X"0D0A"
              AND HEADERS-LEN OF WS-HTTP-REQUEST = HEADERS-SIZE OF
              WS-HTTP-REQUEST
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

           DISPLAY ">Method: '",
           FUNCTION TRIM(HTTP-METHOD OF WS-HTTP-REQUEST TRAILING),
           "' Path: '",
           FUNCTION TRIM(PATH OF WS-HTTP-REQUEST TRAILING),
           "' Protocol: '",
           FUNCTION TRIM(PROTOCOL OF WS-HTTP-REQUEST TRAILING),
           "' RequestCounter: '",
           WS-HTTP-REQUEST-COUNTER,
           "'"
           END-DISPLAY.

      * Consume remaining body by checking Content-Length header
           PERFORM PARSE-TRANSFER-ENCODING-CHUNKED-FROM-REQUEST-HEADERS.
           IF IS-TRANSFER-ENCODING-CHUNKED = 'N'
           THEN
               PERFORM READ-BODY-USING-CONTENT-LENGTH
           ELSE
               PERFORM READ-BODY-USING-CHUNK-ENCODING
           END-IF.

      * Now we read the entire request
      * it is time to disable the alarm
           CALL "alarm"
           USING BY VALUE 0
           RETURNING WS-TEMP
           END-CALL.

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
               MOVE
               FUNCTION TRIM (PATH OF WS-HTTP-REQUEST TRAILING)
               TO WS-TEMP-BUFFER
               MOVE WS-TEMP-BUFFER(2:256) TO WS-FILENAME
           END-IF.

           MOVE SPACES TO WS-FILENAME-NULLTERMINATED.
           STRING WS-FILENAME DELIMITED BY SPACE
           X"00" DELIMITED BY SIZE
           INTO WS-FILENAME-NULLTERMINATED
           END-STRING

           CALL "open" 
           USING BY REFERENCE WS-FILENAME-NULLTERMINATED,
           BY VALUE 0
           RETURNING WS-FILEFD
           END-CALL.

      * If we can't open the file, return 404
           IF WS-FILEFD = -1
           THEN
               MOVE 404 TO HTTP-STATUS OF WS-HTTP-RESPONSE
               PERFORM COMPUTE-STATUSTEXT-FROM-STATUS
               PERFORM SEND-STATUSCODE-AS-HTTP-RESPONSE
               GOBACK
           END-IF.

           CALL "is_filepath_subpath_of_cwd"
           USING BY REFERENCE WS-FILENAME-NULLTERMINATED
           RETURNING WS-TEMP
           END-CALL.

           IF WS-TEMP = 0
           THEN
               MOVE 404 TO HTTP-STATUS OF WS-HTTP-RESPONSE
               PERFORM COMPUTE-STATUSTEXT-FROM-STATUS
               PERFORM SEND-STATUSCODE-AS-HTTP-RESPONSE

               CALL "close"
               USING BY VALUE WS-FILEFD
               RETURNING WS-TEMP
               END-CALL

               GOBACK
           END-IF.

           CALL "is_directory"
           USING BY VALUE WS-FILEFD
           RETURNING WS-TEMP
           END-CALL.

      * If the file is a directory, return 404 and close the file
           IF WS-TEMP NOT = 0
           THEN
               MOVE 404 TO HTTP-STATUS OF WS-HTTP-RESPONSE
               PERFORM COMPUTE-STATUSTEXT-FROM-STATUS
               PERFORM SEND-STATUSCODE-AS-HTTP-RESPONSE

               CALL "close"
               USING BY VALUE WS-FILEFD
               RETURNING WS-TEMP
               END-CALL

               GOBACK
           END-IF.

           MOVE 200 TO HTTP-STATUS OF WS-HTTP-RESPONSE.
           PERFORM COMPUTE-STATUSTEXT-FROM-STATUS.
           PERFORM SEND-FILE-AS-HTTP-RESPONSE.

           CALL "close"
           USING BY VALUE WS-FILEFD
           RETURNING WS-TEMP
           END-CALL.

           GOBACK.

       PROCESS-HTTP-HEADERS.
           MOVE ZERO TO HEADERS-LEN OF WS-HTTP-REQUEST.

           PERFORM UNTIL WS-BUFFER-LEN = 0 OR WS-BUFFER(1:2) = X"0D0A"
                   OR HEADERS-LEN OF WS-HTTP-REQUEST = HEADERS-SIZE OF
                   WS-HTTP-REQUEST

      *    We need to ensure that the last \r\n\r\n is inside
      *    WS-BUFFER with further reads
               MOVE ZERO TO WS-TEMP
               INSPECT WS-BUFFER(1:WS-BUFFER-LEN)
               TALLYING WS-TEMP FOR ALL X"0D0A0D0A"
               IF WS-TEMP = 0 AND WS-BUFFER-LEN NOT = WS-BUFFER-SIZE
               THEN
                   PERFORM
                       READ-FROM-SOCKET-AND-FILL-WS-BUFFER-WITH-TIMEOUT
               END-IF

               PERFORM READ-HTTP-LINE

               COMPUTE
               HEADERS-LEN OF WS-HTTP-REQUEST = HEADERS-LEN OF
               WS-HTTP-REQUEST + 1
               END-COMPUTE

               UNSTRING WS-HTTP-LINE
               DELIMITED BY ": "
               INTO HEADER-KEY 
               OF HEADERS OF
               WS-HTTP-REQUEST (HEADERS-LEN OF WS-HTTP-REQUEST),
               HEADER-VALUE OF HEADERS OF WS-HTTP-REQUEST
               (HEADERS-LEN OF WS-HTTP-REQUEST)

               END-UNSTRING
           END-PERFORM.

       PARSE-CONTENT-LENGTH-FROM-REQUEST-HEADERS.
           PERFORM VARYING WS-TEMP FROM 1 BY 1
           UNTIL WS-TEMP > HEADERS-LEN OF WS-HTTP-REQUEST
           IF HEADER-KEY OF HEADERS OF WS-HTTP-REQUEST (WS-TEMP)
                   = "Content-Length"
           THEN
               COMPUTE
               CONTENT-LENGTH = FUNCTION NUMVAL(HEADER-VALUE OF
               HEADERS OF WS-HTTP-REQUEST(WS-TEMP))
               END-COMPUTE
           END-IF
           END-PERFORM.

       PARSE-TRANSFER-ENCODING-CHUNKED-FROM-REQUEST-HEADERS.
           MOVE 'N' TO IS-TRANSFER-ENCODING-CHUNKED.
           PERFORM VARYING WS-TEMP FROM 1 BY 1
           UNTIL WS-TEMP > HEADERS-LEN OF WS-HTTP-REQUEST
           IF HEADER-KEY OF HEADERS OF WS-HTTP-REQUEST (WS-TEMP)
                   = "Transfer-Encoding"
           AND HEADER-VALUE OF HEADERS OF WS-HTTP-REQUEST (WS-TEMP)
                   = "chunked"
           THEN
               MOVE 'Y' TO IS-TRANSFER-ENCODING-CHUNKED
           END-IF
           END-PERFORM.

       READ-BODY-USING-CONTENT-LENGTH.
           PERFORM PARSE-CONTENT-LENGTH-FROM-REQUEST-HEADERS.
           COMPUTE
           REMAINING-CONTENT-LENGTH = CONTENT-LENGTH - WS-BUFFER-LEN
           END-COMPUTE.
           PERFORM UNTIL REMAINING-CONTENT-LENGTH = 0
      * We reuse the WS-BUFFER to stream the entire request buffer
               MOVE ZERO TO WS-BUFFER-LEN
               PERFORM READ-FROM-SOCKET-AND-FILL-WS-BUFFER-WITH-TIMEOUT
               COMPUTE
               REMAINING-CONTENT-LENGTH = 
               REMAINING-CONTENT-LENGTH - WS-BUFFER-LEN
               END-COMPUTE
           END-PERFORM.

       READ-BODY-USING-CHUNK-ENCODING.
           PERFORM FOREVER
               PERFORM READ-HTTP-LINE

               MOVE FUNCTION TRIM(WS-HTTP-LINE) TO WS-HEX-STRING
      * TODO: validate WS-HEX-STRING only contains hex
               PERFORM CONVERT-HEXSTRING-TO-DECIMAL
               MOVE WS-NUMERIC-VALUE TO CHUNK-LENGTH
               IF CHUNK-LENGTH = 0
               THEN
                   EXIT PARAGRAPH
               END-IF
               PERFORM READ-ENTIRE-CHUNK
           END-PERFORM.

       READ-ENTIRE-CHUNK.
      *  If the chunk is empty just bail
           IF CHUNK-LENGTH = 0
           THEN
               EXIT PARAGRAPH
           END-IF.

      * Include \r\n inside the chunk
           COMPUTE
           CHUNK-LENGTH = CHUNK-LENGTH + 2
           END-COMPUTE.

           PERFORM UNTIL CHUNK-LENGTH = 0
               IF WS-BUFFER-LEN < CHUNK-LENGTH
               THEN
                   PERFORM
                   READ-FROM-SOCKET-AND-FILL-WS-BUFFER-WITH-TIMEOUT
               END-IF

               IF WS-BUFFER-LEN < CHUNK-LENGTH
                   MOVE WS-BUFFER-LEN TO WS-TEMP3
               ELSE
                   MOVE CHUNK-LENGTH TO WS-TEMP3
               END-IF

               COMPUTE
               WS-BUFFER-LEN = WS-BUFFER-LEN - WS-TEMP3
               END-COMPUTE

      *    Substring syntax starts with index 1...
               COMPUTE
               WS-TEMP2 = WS-TEMP3 + 1
               END-COMPUTE
               MOVE WS-BUFFER(WS-TEMP2:) TO WS-TEMP-BUFFER
               MOVE WS-TEMP-BUFFER TO WS-BUFFER

               COMPUTE
               CHUNK-LENGTH = CHUNK-LENGTH - WS-TEMP3
               END-COMPUTE
           END-PERFORM.

       CONVERT-HEXSTRING-TO-DECIMAL.
           MOVE ZERO TO WS-NUM-TRAILING-SPACES.
           MOVE ZERO TO WS-NUMERIC-VALUE.
           INSPECT WS-HEX-STRING
           REPLACING ALL X"0D" BY SPACES.
           INSPECT WS-HEX-STRING
           REPLACING ALL X"0A" BY SPACES.
           INSPECT WS-HEX-STRING
           TALLYING WS-NUM-TRAILING-SPACES FOR TRAILING SPACES.

           COMPUTE
           WS-NUM-HEX-DIGITS = 16 - WS-NUM-TRAILING-SPACES
           END-COMPUTE.
           COMPUTE
           WS-NUM-TRAILING-SPACES = WS-NUM-TRAILING-SPACES + 1
           END-COMPUTE.
           MOVE ZEROS TO WS-TEXT.
           MOVE WS-HEX-STRING(1:WS-NUM-HEX-DIGITS)
           TO WS-TEXT(WS-NUM-TRAILING-SPACES:).
           MOVE WS-TEXT TO WS-HEX-STRING.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 16
               MOVE WS-HEX-STRING (WS-I:1) TO WS-HEX-CHAR
               PERFORM CONVERT-HEX-TO-DECIMAL
               MULTIPLY 16 BY WS-NUMERIC-VALUE
               END-MULTIPLY
               ADD WS-HEX-VALUE TO WS-NUMERIC-VALUE
               END-ADD
           END-PERFORM.

       CONVERT-HEX-TO-DECIMAL.
           MOVE 0 TO WS-HEX-VALUE.
           INSPECT WS-HEX-DIGITS TALLYING WS-HEX-VALUE
           FOR CHARACTERS BEFORE FUNCTION Upper-case(WS-HEX-CHAR).

       READ-FROM-SOCKET-AND-FILL-WS-BUFFER-WITH-TIMEOUT.
           PERFORM READ-FROM-SOCKET-AND-FILL-WS-BUFFER.

      * If we get a SIGALRM during reading this is a client read timeout
      * return Http Status 408 and stop the process
           IF WS-ALRM-WAS-RAISED = "Y"
           THEN
               MOVE 408 TO HTTP-STATUS OF WS-HTTP-RESPONSE
               PERFORM COMPUTE-STATUSTEXT-FROM-STATUS
               PERFORM SEND-STATUSCODE-AS-HTTP-RESPONSE
               GOBACK
           END-IF.

       READ-FROM-SOCKET-AND-FILL-WS-BUFFER.
           COMPUTE 
           WS-TEMP2 = WS-BUFFER-SIZE - WS-BUFFER-LEN
           END-COMPUTE.

           MOVE SPACES TO WS-TEMP-BUFFER
           CALL "read"
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY REFERENCE WS-TEMP-BUFFER,
           BY VALUE WS-TEMP2
           RETURNING WS-TEMP
           END-CALL.

           COMPUTE
           WS-TEMP2 = WS-BUFFER-LEN + 1
           END-COMPUTE.

           MOVE WS-TEMP-BUFFER(1:WS-TEMP)
           TO WS-BUFFER(WS-TEMP2:).

           COMPUTE
           WS-BUFFER-LEN = WS-BUFFER-LEN + WS-TEMP
           END-COMPUTE.

       READ-HTTP-LINE.
           MOVE SPACES TO WS-HTTP-LINE.

           UNSTRING WS-BUFFER(1:WS-BUFFER-LEN)
           DELIMITED BY X"0D0A"
           INTO WS-HTTP-LINE
           COUNT IN WS-HTTP-LINE-LEN
           END-UNSTRING.
 
           COMPUTE
           WS-HTTP-LINE-LEN = WS-HTTP-LINE-LEN + 2
           END-COMPUTE.

           COMPUTE
           WS-BUFFER-LEN = WS-BUFFER-LEN - WS-HTTP-LINE-LEN
           END-COMPUTE.

      * Substring syntax starts with index 1...
           COMPUTE
           WS-TEMP2 = WS-HTTP-LINE-LEN + 1
           END-COMPUTE.
           MOVE WS-BUFFER(WS-TEMP2:) TO WS-TEMP-BUFFER.
           MOVE WS-TEMP-BUFFER TO WS-BUFFER.

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

       COMPUTE-CONTENT-TYPE.
      * Default value
           MOVE "application/octet-stream" TO WS-CONTENT-TYPE.

      * Check there is a '.' in the WS-FILENAME
      * If not we just bail
           MOVE 0 TO WS-TEMP.
           INSPECT WS-FILENAME
           TALLYING WS-TEMP FOR ALL '.'.
           IF WS-TEMP = 0
           THEN
               EXIT PARAGRAPH
           END-IF.

      * Find the last '.' and try to get the file ending
           MOVE 0 TO WS-TEMP.
           INSPECT FUNCTION REVERSE(WS-FILENAME)
           TALLYING WS-TEMP FOR CHARACTERS AFTER INITIAL '.'.
           COMPUTE
           WS-TEMP = WS-TEMP + 1
           END-COMPUTE.
           MOVE SPACES TO WS-FILESUFFIX.
           MOVE WS-FILENAME(WS-TEMP:) TO
           WS-FILESUFFIX.

           EVALUATE WS-FILESUFFIX
               WHEN ".txt"
                   MOVE "text/plain"
                   TO WS-CONTENT-TYPE
               WHEN ".html"
                   MOVE "text/html"
                   TO WS-CONTENT-TYPE
               WHEN ".js"
                   MOVE "text/javascript"
                   TO WS-CONTENT-TYPE
               WHEN ".css"
                   MOVE "text/css"
                   TO WS-CONTENT-TYPE
               WHEN ".apng"
                   MOVE "image/apng"
                   TO WS-CONTENT-TYPE
               WHEN ".avif"
                   MOVE "image/avif"
                   TO WS-CONTENT-TYPE
               WHEN ".gif"
                   MOVE "image/gif"
                   TO WS-CONTENT-TYPE
               WHEN ".jpg"
                   MOVE "image/jpeg"
                   TO WS-CONTENT-TYPE
               WHEN ".jpeg"
                   MOVE "image/jpeg"
                   TO WS-CONTENT-TYPE
               WHEN ".png"
                   MOVE "image/png"
                   TO WS-CONTENT-TYPE
               WHEN ".svg"
                   MOVE "image/svg+xml"
                   TO WS-CONTENT-TYPE
               WHEN ".webp"
                   MOVE "image/webp"
                   TO WS-CONTENT-TYPE
           END-EVALUATE.

       SEND-HTTP-STATUS-LINE.
           MOVE SPACES TO WS-BUFFER.
           STRING FUNCTION TRIM(PROTOCOL OF WS-HTTP-REQUEST) " "
           HTTP-STATUS OF WS-HTTP-RESPONSE " " DELIMITED BY SIZE
           FUNCTION TRIM(HTTP-STATUSTEXT OF WS-HTTP-RESPONSE)
           INTO WS-BUFFER
           END-STRING.
           PERFORM WRITE-TO-CLIENT-SOCKET.

           MOVE X"0D0A" TO WS-BUFFER.
           PERFORM WRITE-TO-CLIENT-SOCKET.

       SEND-RESPONSE-HEADERS.
           PERFORM VARYING WS-I FROM 1 BY 1
           UNTIL WS-I > HEADERS-LEN OF WS-HTTP-RESPONSE
               MOVE SPACES TO WS-BUFFER
               STRING
               HEADER-KEY OF HEADERS
               OF WS-HTTP-RESPONSE(WS-I) DELIMITED BY SPACE,
               ": " DELIMITED BY SIZE,
               HEADER-VALUE OF HEADERS
               OF WS-HTTP-RESPONSE(WS-I) DELIMITED BY SPACE
               INTO WS-BUFFER
               END-STRING
               PERFORM WRITE-TO-CLIENT-SOCKET

               MOVE X"0D0A" TO WS-BUFFER
               PERFORM WRITE-TO-CLIENT-SOCKET
           END-PERFORM.
           MOVE X"0D0A" TO WS-BUFFER.
           PERFORM WRITE-TO-CLIENT-SOCKET.


       SEND-STATUSCODE-AS-HTTP-RESPONSE.
           MOVE 1 TO HEADERS-LEN OF WS-HTTP-RESPONSE.
           MOVE "Content-Length" TO
           HEADER-KEY OF HEADERS OF WS-HTTP-RESPONSE
           (HEADERS-LEN OF WS-HTTP-RESPONSE).
           MOVE "0" TO
           HEADER-VALUE OF HEADERS OF WS-HTTP-RESPONSE
           (HEADERS-LEN OF WS-HTTP-RESPONSE).

           COMPUTE
           HEADERS-LEN OF WS-HTTP-RESPONSE =
           HEADERS-LEN OF WS-HTTP-RESPONSE + 1
           END-COMPUTE.

           MOVE "Server" TO
           HEADER-KEY OF HEADERS OF WS-HTTP-RESPONSE
           (HEADERS-LEN OF WS-HTTP-RESPONSE).
           MOVE "cobol-webserver" TO
           HEADER-VALUE OF HEADERS OF WS-HTTP-RESPONSE
           (HEADERS-LEN OF WS-HTTP-RESPONSE).

           COMPUTE
           HEADERS-LEN OF WS-HTTP-RESPONSE =
           HEADERS-LEN OF WS-HTTP-RESPONSE + 1
           END-COMPUTE.

           MOVE "Connection" TO
           HEADER-KEY OF HEADERS OF WS-HTTP-RESPONSE
           (HEADERS-LEN OF WS-HTTP-RESPONSE).
           MOVE "close" TO
           HEADER-VALUE OF HEADERS OF WS-HTTP-RESPONSE
           (HEADERS-LEN OF WS-HTTP-RESPONSE).

           DISPLAY "<Status: '",
           HTTP-STATUS OF WS-HTTP-RESPONSE,
           "' RequestCounter: '",
           WS-HTTP-REQUEST-COUNTER,
           "'"
           END-DISPLAY.

           PERFORM SEND-HTTP-STATUS-LINE.
           PERFORM SEND-RESPONSE-HEADERS.

           PERFORM CLOSE-CLIENT-SOCKET.

       SEND-FILE-AS-HTTP-RESPONSE.
           PERFORM COMPUTE-CONTENT-TYPE.

           MOVE 1 TO HEADERS-LEN OF WS-HTTP-RESPONSE.
           MOVE "Content-Type" TO
           HEADER-KEY OF HEADERS OF WS-HTTP-RESPONSE
           (HEADERS-LEN OF WS-HTTP-RESPONSE).
           MOVE WS-CONTENT-TYPE TO
           HEADER-VALUE OF HEADERS OF WS-HTTP-RESPONSE
           (HEADERS-LEN OF WS-HTTP-RESPONSE).

           PERFORM COMPUTE-FILE-SIZE.

           COMPUTE
           HEADERS-LEN OF WS-HTTP-RESPONSE =
           HEADERS-LEN OF WS-HTTP-RESPONSE + 1
           END-COMPUTE.
           MOVE WS-FILESIZE TO WS-FILESIZE-WITHOUT-LEADING-ZEROS.
           MOVE "Content-Length" TO
           HEADER-KEY OF HEADERS OF WS-HTTP-RESPONSE
           (HEADERS-LEN OF WS-HTTP-RESPONSE).
           MOVE FUNCTION TRIM(WS-FILESIZE-WITHOUT-LEADING-ZEROS LEADING)
           TO HEADER-VALUE OF HEADERS OF WS-HTTP-RESPONSE
           (HEADERS-LEN OF WS-HTTP-RESPONSE).

           COMPUTE
           HEADERS-LEN OF WS-HTTP-RESPONSE =
           HEADERS-LEN OF WS-HTTP-RESPONSE + 1
           END-COMPUTE.

           MOVE "Server" TO
           HEADER-KEY OF HEADERS OF WS-HTTP-RESPONSE
           (HEADERS-LEN OF WS-HTTP-RESPONSE).
           MOVE "cobol-webserver" TO
           HEADER-VALUE OF HEADERS OF WS-HTTP-RESPONSE
           (HEADERS-LEN OF WS-HTTP-RESPONSE).

           COMPUTE
           HEADERS-LEN OF WS-HTTP-RESPONSE =
           HEADERS-LEN OF WS-HTTP-RESPONSE + 1
           END-COMPUTE.

           MOVE "Connection" TO
           HEADER-KEY OF HEADERS OF WS-HTTP-RESPONSE
           (HEADERS-LEN OF WS-HTTP-RESPONSE).
           MOVE "close" TO
           HEADER-VALUE OF HEADERS OF WS-HTTP-RESPONSE
           (HEADERS-LEN OF WS-HTTP-RESPONSE).

           DISPLAY "<Status: '",
           HTTP-STATUS OF WS-HTTP-RESPONSE,
           "' File: '",
           FUNCTION TRIM(WS-FILENAME TRAILING),
           "' RequestCounter: '",
           WS-HTTP-REQUEST-COUNTER,
           "'"
           END-DISPLAY.


           PERFORM SEND-HTTP-STATUS-LINE.
           PERFORM SEND-RESPONSE-HEADERS.
           PERFORM SEND-FILE-TO-CLIENT-SOCKET.

           PERFORM CLOSE-CLIENT-SOCKET.

       COMPUTE-BUFFER-LEN.
           MOVE ZERO TO  WS-BUFFER-LEN.
           INSPECT WS-BUFFER
           TALLYING WS-BUFFER-LEN FOR TRAILING SPACES.
           COMPUTE WS-BUFFER-LEN = LENGTH OF WS-BUFFER - WS-BUFFER-LEN
           END-COMPUTE.

       SEND-FILE-TO-CLIENT-SOCKET.
           MOVE ZERO TO WS-SENDFILE-OFFSET.
           MOVE ZERO TO WS-TEMP.
           PERFORM SEND-FILE-TO-CLIENT-SOCKET-LOOP 
           UNTIL WS-FILESIZE = 0 OR WS-TEMP = -1.

       SEND-FILE-TO-CLIENT-SOCKET-LOOP.
           CALL "sendfile64"
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY VALUE WS-FILEFD,
           BY REFERENCE WS-SENDFILE-OFFSET,
           BY VALUE WS-FILESIZE
           RETURNING WS-TEMP
           END-CALL.
           IF WS-TEMP = -1
           THEN
               DISPLAY "sendfile call failed: ", WS-TEMP
               END-DISPLAY
           ELSE
               COMPUTE WS-FILESIZE = WS-FILESIZE - WS-TEMP
               END-COMPUTE
           END-IF.

       WRITE-TO-CLIENT-SOCKET.
           PERFORM COMPUTE-BUFFER-LEN.
           MOVE ZERO TO WS-TEMP.
           PERFORM WRITE-TO-CLIENT-SOCKET-LOOP 
           UNTIL WS-BUFFER-LEN = 0 OR WS-TEMP = -1.

       WRITE-TO-CLIENT-SOCKET-LOOP.
           CALL "write" 
           USING BY VALUE WS-CLIENT-SOCKFD,
           BY REFERENCE WS-BUFFER,
           BY VALUE WS-BUFFER-LEN
           RETURNING WS-TEMP
           END-CALL.
           IF WS-TEMP = -1
           THEN
               DISPLAY "write call failed: ", WS-TEMP
               END-DISPLAY
           END-IF.
           IF WS-TEMP > 0
           THEN
               MOVE SPACES TO WS-TEMP-BUFFER
               MOVE WS-BUFFER(WS-TEMP:) TO WS-TEMP-BUFFER
               MOVE WS-TEMP-BUFFER TO WS-BUFFER
               COMPUTE WS-BUFFER-LEN = WS-BUFFER-LEN - WS-TEMP
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
           RETURNING WS-TEMP
           END-CALL.

       END PROGRAM cobol-webserver.
