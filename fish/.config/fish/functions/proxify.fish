function proxify -d "Set correct Fraunhofer IPK proxy settings"
    set proxy "https://153.96.56.101:3128"
    set -gx all_proxy   $proxy
    set -gx https_proxy $proxy
    set -gx http_proxy  "http://153.96.56.101:3128"
    set -gx ftp_proxy   $proxy
    set -gx ALL_PROXY   $proxy
    set -gx HTTPS_PROXY $proxy
    set -gx HTTP_PROXY  "http://153.96.56.101:3128"
    set -gx FTP_PROXY   $proxy
    set -gx no_proxy "localhost,127.0.0.0/8,::1"
end
