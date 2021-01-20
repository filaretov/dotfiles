local iron = require('iron')

iron.core.add_repl_definitions {
    python = {
        mycustom = {
            command = {"what"}
        }
    }
}

iron.core.set_config {
    preferred = {
        python = "python"
    }
}
