# ClojureBVE (OpenBVE Clone in Clojure)

http://www.openbve.org/

The main motivation for this project is for me to learn OpenGL and
sharpen up my clojure skills. Use it with caution.

## New Features

* Multiplayer (Pending)
* Dispatch Display (Pending)
* Drive on Any Rail (Pending)

## Current Status

Most of the core features are in progress and the game is nearly
playable. However, the app isn't really packaged for use yet.

Major Pieces Left:
* Brakes
* Command Line Interface
* Texture Clamping
* Render more overlays
* Arcade-style points
* Fix texture flipping
* Switch to quad-trees instead of sphere-based fulcrum culling

Right now only simple routes work (max 1M vertexes for 30fps running)
and only *panel.cfg* simple cabs work

## License

Copyright © 2013 Adam Hinz

ClojureBVE is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program.  If not, see <http://www.gnu.org/licenses/>.
