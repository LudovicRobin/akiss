NUM=7a

all: manual-auth-${NUM}.tex
	pdflatex manual-auth-${NUM}.tex
	rm *.log *.aux

clean:
	rm `ls | grep -e 'manual-auth-.*\.pdf'`
	rm *.log *.aux

rename:
	mv `ls | grep -e 'manual-auth-.*\.tex'` manual-auth-${NUM}.tex
