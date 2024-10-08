# Include game modules
modules = \
  modules/game/trader.scm \
  modules/game/travel.scm \
  modules/game/world.scm \
  modules/game/market.scm \
  modules/game/mission.scm

# Compile the game into WebAssembly
game.wasm: game.scm $(modules)
	# Use a tab here
	guild compile-wasm -L modules -o $@ $<

# Serve the game using Guile-Hoot
serve: game.wasm
	# Use a tab here
	guile -c '((@ (hoot web-server) serve))'

# Bundle the game files into a zip archive (removing assets directory)
bundle: game.wasm
	# Use a tab here
	rm game.zip || true
	zip game.zip -r js-runtime/ game.js game.css game.wasm index.html

# Clean up build files
clean:
	# Use a tab here
	rm -f game.wasm game.zip
