function mlgpg
    gpg --output tmp.zip --decrypt $argv[1]
    unzip tmp.zip
    rm tmp.zip
end
