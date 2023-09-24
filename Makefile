webserver: webserver.cob sigint_handler.cob is_dir.o
	cobc -Wall -Wextra -fstatic-call -x -O2 -o $@ $^

clean:
	rm -f is_dir.o
	rm -f webserver
