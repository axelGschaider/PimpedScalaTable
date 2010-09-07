
SC = scalac
FSC = fsc

all:
	fsc -deprecation *.scala

full:
	find . -name "*.class" | xargs rm -f
	fsc -deprecation *.scala

clean: 
	find . -name "*.class" | xargs rm -f

test: 
	find . -name "*.class" | xargs rm -f
	fsc -deprecation *.scala
	scala Test


