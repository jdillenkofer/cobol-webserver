webserver: webserver.cob sigint_handler.cob file-helper.o
	cobc -Wall -Wextra -fstatic-call -x -O2 -o $@ $^

clean:
	rm -f file-helper.o
	rm -f webserver
