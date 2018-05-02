function deproxify -d "Remove Fraunhofer IPK env vars"
    set -ex http_proxy
    set -ex all_proxy
    set -ex ftp_proxy
    set -ex https_proxy
    set -ex no_proxy
end
