#!/bin/bash
### BEGIN INIT INFO
# Provides:          promet-process
# Required-Start:    $local_fs $remote_fs $network $syslog $named
# Required-Stop:     $local_fs $remote_fs $network $syslog $named
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# X-Interactive:     true
# Short-Description: Start/stop promet-process
# Description:       Start the promet-process to manage processes
### END INIT INFO

# Promet Process Service

SVC_START_OPTIONS="-r"
SVC_STOP_OPTIONS="s"
SVC_ALIAS="Service to execute Promet services"
SVC_FILENAME="promet-process"
SVC_DIR="/usr/lib/promet-erp/tools/"
SVC_SERVICE_SCRIPT="promet-process"


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
    force-reload)
        stop
        start
        ;;
    restart)
        stop
        start
        ;;
    *)
	echo $SVC_ALIAS" [Invalid Startup Parameters]"        
	echo "Usage:  {start|stop|status|restart|force-reload}"
        exit 1
        ;;
esac
exit $?
