set -gx all_proxy  "http://153.96.56.101:3128"
set -gx http_proxy "http://153.96.56.101:3128"
set -gx ftp_proxy $all_proxy
set -gx https_proxy $all_proxy
set -gx ALL_PROXY  "http://153.96.56.101:3128"
set -gx HTTP_PROXY "http://153.96.56.101:3128"
set -gx FTP_PROXY $all_proxy
set -gx HTTPS_PROXY $all_proxy
set -gx no_proxy

begin
  set conda_file ~/.miniconda/etc/fish/conf.d/conda.fish
  if test -f $conda_file
    source $conda_file
  end
end
