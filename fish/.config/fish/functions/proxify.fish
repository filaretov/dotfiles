function proxify -d "Set correct Fraunhofer IPK proxy settings"
    set -x http_proxy "https://153.96.56.101:3128"
    set -x all_proxy "https://153.96.56.101:3128"
    set -x ftp_proxy "http://153.96.56.101:3128/"
    set -x https_proxy "http://153.96.56.101:3128/"
    set -x no_proxy "localhost,127.0.0.0/8,::1"
end
