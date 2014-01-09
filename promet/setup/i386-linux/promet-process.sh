#!/bin/bash
# Lazarus service application shell script

SVC_START_OPTIONS="-r"
SVC_STOP_OPTIONS="s"

# Edit SVC_ALIAS to the long description of your service application
SVC_ALIAS="Description of your program"
# Edit SVC_FILENAME to the actual name of your compiled service application
SVC_FILENAME="YourProgram"

# Edit SVC_DIR to where you place your compiled service application 
SVC_DIR="/bin/YourFolder/"

# Edit SVC_SERVICE_SCRIPT to the name of this file without the extension
SVC_SERVICE_SCRIPT="YourService"
# this will become your service name.  Ie.) service YourService start

SVC_FILE=$SVC_DIR$SVC_FILENAME
start() {
	if [ -f $SVC_FILE ]; then 
	  #reset       
	  echo -n "Starting "$SVC_ALIAS": "
	  RETVALS=$(start-stop-daemon -S -b -x $SVC_FILE -- $SVC_START_OPTIONS)
			
          Count=${#RETVALS[@]}
	  RETVAL="[FAIL]"
		
          if [ $Count -eq 0 ]; then
	    RETVAL="[OK]"
	  elif [ $Count -eq 1 ]; then
	    if [ ${#RETVALS[0]} -eq 0 ]; then
	      RETVAL="[OK]"	
	    else 
	      iStart=${#SVC_FILE}
	      iLength=${#RETVALS[0]}
	      Response=${RETVALS[0]:(iStart+1):7}
	      RETVAL=$Response
	      if [ "$Response" == "already" ]; then
	        RETVAL="[OK]"
	      fi
	    fi				
  	  fi
	  echo $RETVAL
          return 0
	else
	  echo $SVC_ALIAS" not installed" $SVC_DIR
	  exit 2;	
	fi
}

stop() {
	echo -n "Shutting down "$SVC_ALIAS":"
	RETVALS=$(start-stop-daemon -K -x $SVC_FILE -- $SVC_STOP_OPTIONS)
	#additional PROCKILLS=$(killall -w -q -e $SVC_PROCESS_NAME $SVC_FILENAME)
	Count=${#RETVALS[@]}
	Index=0	
	RETVAL="[FAIL]"
	if [ $Count -eq 1 ]; then
		if [ ${#RETVALS[0]} -eq 0 ]; then
			RETVAL="[OK]"	
		else 
			Response=${RETVALS[0]:0:2}
			RETVAL=$Response
			if [ "$Response" == "No" ]; then
				RETVAL="[OK]"
			fi
		fi			
	else
		RETVAL="[OK]"
	fi 

	echo $RETVAL	
        return 0
}

case "$1" in
    start)
        start
        ;;
    stop)
        stop
        ;;
    status)
        status $SVC_SERVICE_SCRIPT
        ;;
    restart)
        stop
        start
        ;;
    *)
	echo $SVC_ALIAS" [Invalid Startup Parameters]"        
	echo "Usage:  {start|stop|status|restart}"
        exit 1
        ;;
esac
exit $?
