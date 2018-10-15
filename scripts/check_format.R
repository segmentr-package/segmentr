reformatted <- styler::style_pkg()

if (any(reformatted$changed)) {
    quit(status=1)
}
