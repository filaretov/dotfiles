set -gx all_proxy "https://153.96.56.101:3128"
set -gx http_proxy "https://153.96.56.101:3128"
set -gx ftp_proxy "https://153.96.56.101:3128"

source (conda info --root)/etc/fish/conf.d/conda.fish
