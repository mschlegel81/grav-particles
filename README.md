# grav-particles
This is a simple particle based gravity simulation.

Let dust orbit around systems of up to 5 mass points - "stars" if you will.

Stars interact with each other in the usual manner, while dust is treated as massless (therefore non-attracting) test particles.

Dust which is neither gravitationally bound nor moves in the general direction of a star is removed.

## Controls
### Speed
...selects the simulation speed, i.e. the factor between real time and simulated time.

### Stars
Select how many stars you would like and either randomize a new system or reset the current system to its initial state.

When a system is randomized, one of 100 pre calculated systems is picked.

You can also grow or shrink the whole system (including the current dust particles). 

### Dust
Select how many dust particles are distributed. 

If you click "auto", the program tries to guess how many particles can be simulated so that a frame rate of 40fps is still achievable.

For most formations you can choose around which stars the dust will be distributed.

### Disk particle size
Sets the point size.

### Start/stop background calculation
You can manually start background threads looking for more stable star systems. Have a look at the console window to see what is happpening there.
