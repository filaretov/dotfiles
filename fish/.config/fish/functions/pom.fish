function pom -d "A command-line tomato clock"
    set task $argv[1]
    set tc_start "i "(date +'%Y/%m/%d %H:%M')" $task"
    set tc_end "o "(date -d '+30 minutes' +'%Y/%m/%d %H:%M')
    set end_time (date -d '+25 minutes' +'%H:%M')
    set stamp "$task — $end_time" 
    echo -n $stamp > /tmp/pom_task
    notify-send -t 10000 -u critical 'Pomodoro' "Session $task.\nEnds: $end_time"
    sleep (math '25*60')
    pom_end $task
    echo -e "$tc_start\n$tc_end" 
    echo -e "$tc_start\n$tc_end"  >> ~/.timeclock
end

