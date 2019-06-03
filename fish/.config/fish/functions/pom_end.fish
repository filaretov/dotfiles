function pom_end -d "End the pomodoro."
    echo -n "" > /tmp/pom_task
    notify-send -t 0 -u critical 'Pomodoro' "End of session $argv[1]"
end