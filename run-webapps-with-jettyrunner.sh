#==============================================================================
# This Unix shell script starts the AceWiki web applications with Jetty Runner.
# It assumes:
# - The webapps.war file has been built from the AceWiki package.
# - A recent version of SWI Prolog is installed.
# - APE has been compiled (giving "ape.exe").
# - Jetty Runner has been downloaded (and renamed to "jetty-runner.jar").
# - The files "webapps.war", "ape.exe" and "jetty-runner.jar" are in the
#   same directory.
# - The process has write permissions to the subdirectories "data" and "logs",
#   or has the permission to create them.
# 
# (written by Tobias Kuhn with content from Jean-Marc Vanel)
#==============================================================================


# On some systems, the SWI Prolog command is "pl" instead of "swipl":

eval `swipl -dump-runtime-variables`
#eval `pl -dump-runtime-variables`


# Under Linux, the environment variable LD_PRELOAD has to refer to the SWI
# Prolog library. Under some circumstances, also LD_LIBRARY_PATH has to be set.
# Note that "i386" has to map the architecture of your system.

#export JAVA_HOME=/usr/lib/jvm/java-6-openjdk
#export LD_PRELOAD=$PLBASE/lib/$PLARCH/libjpl.so
#export LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/i386:$LD_LIBRARY_PATH
#export LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/i386/server:$LD_LIBRARY_PATH


# The following command starts the AceWiki web application. It might be
# necessary to change port number, heap size, or stack size.

java -Djava.library.path=$PLBASE/lib/$PLARCH -Xmx400m -Xss4m -jar jetty-runner.jar --port 9077 webapps.war
