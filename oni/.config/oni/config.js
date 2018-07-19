"use strict";
exports.__esModule = true;
exports.activate = function (oni) {
    console.log("config activated");
    // Input
    //
    // Add input bindings here:
    //
    oni.input.bind("<c-enter>", function () { return console.log("Control+Enter was pressed"); });
    //
    // Or remove the default bindings here by uncommenting the below line:
    //
    // oni.input.unbind("<c-p>")
};
exports.deactivate = function (oni) {
    console.log("config deactivated");
};

exports.configuration = {
    //add custom config here, such as
    "ui.colorscheme":  "nord",
    "oni.loadInitVim":  "$HOME/.config/oni/init.vim",
    "oni.hideMenu":  true,
    "editor.fontSize":  "14px",
    "editor.fontFamily":  "Fira Mono",
    "ui.animations.enabled":  true,
    "ui.fontSmoothing":  "auto",
    "sidebar.enabled":  false, // sidebar ui is gone
};
