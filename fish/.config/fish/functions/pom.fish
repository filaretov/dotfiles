function pom -d "A command-line tomato clock"
    set task $argv[1]
    notify-send -t 10000 -u critical 'Pomodoro' "Session $task.\nEnds: "(date -d '+25 minutes' +'%H:%M')
    sleep (math '1')
    notify-send -t 0 -u critical 'Pomodoro' "End of session $task."
    kdeconnect-cli -n OnePlus --ping-msg "End of session $task."
end
