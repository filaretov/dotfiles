function deproxify -d "Remove Fraunhofer IPK env vars"
    set -ex all_proxy
    set -ex https_proxy
    set -ex http_proxy
    set -ex ftp_proxy
    set -ex ALL_PROXY
    set -ex HTTPS_PROXY
    set -ex HTTP_PROXY
    set -ex FTP_PROXY
    set -ex no_proxy
end
