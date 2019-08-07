# SPDX-FileCopyrightText: Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT
begin
  set conda_file ~/.miniconda/etc/fish/conf.d/conda.fish
  if test -f $conda_file
    source $conda_file
  end
end
