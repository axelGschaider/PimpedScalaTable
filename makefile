
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
	scala -cp .:./lib/log4j-1.2.16.jar:./lib/opencsv-2.2.jar  at.mbm.trending.client.Main


