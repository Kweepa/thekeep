# thekeep

THE KEEP

For the unexpanded VIC-20.

First-person texture mapped raycasting game with random level generation.

Compile with ACME 0.91 or above, then compress with pucrunch.


Make your way to floor 256 before your lamp expires!
Choose from a fixed series of maps, or random maps.

Keys
W, A, S, D ... move
J, L ... turn
I ... map

Joystick
Directions ... move
Fire ... map


Code breakdown

```
Code line Addr  Purpose
0138-0167 $1014 Main game loop
0172-0221 $1045 Map display (when holding down I)
0292-1127 $1148 Generate random map
1132-1255 $1647 Play sounds, read keyboard
1258-1342 $171c Helper functions for raycast
1344-1556 $1771 Raycast using DDC
1558-1741 $18a7 Texturemap and end display loop
1787-2113 $19b0 Player collision and movement code
```
