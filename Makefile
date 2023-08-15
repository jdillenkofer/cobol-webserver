webserver: webserver.cob
	cobc -Wall -Wextra -fstatic-call -x -O2 -o $@ $<

clean:
	rm -f webserver
