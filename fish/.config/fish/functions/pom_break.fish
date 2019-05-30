function pom_break -d "Take a 5-minute break."
    notify-send -t 10000 -u critical 'Pomodoro' "Break started."
    sleep (math '5*60')
    notify-send -t 0 -u critical 'Pomodoro' "Break finished."
end
