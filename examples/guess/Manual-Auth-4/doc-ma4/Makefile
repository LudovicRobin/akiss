NUM=4

all: manual-auth-${NUM}.tex
	pdflatex manual-auth-${NUM}.tex
	rm *.log *.aux

clean:
	rm `ls | grep manual-auth-*.pdf`
	rm *.log *.aux

rename:
	mv `ls | grep manual-auth-*.tex` manual-auth-${NUM}.tex
