webserver: webserver.cob sigint_handler.cob
	cobc -Wall -Wextra -fstatic-call -x -O2 -o $@ $^

clean:
	rm -f webserver
