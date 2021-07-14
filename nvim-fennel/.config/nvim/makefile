all: lua/startup.lua lua/plugins.lua lua/config/snippets.lua lua/functions.lua 

lua/%.lua: fnl/%.fnl
	fennel --compile $< > $@

clean:
	rm lua/**.lua
