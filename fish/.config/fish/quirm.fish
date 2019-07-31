begin
  set conda_file ~/.miniconda/etc/fish/conf.d/conda.fish
  if test -f $conda_file
    source $conda_file
  end
end
