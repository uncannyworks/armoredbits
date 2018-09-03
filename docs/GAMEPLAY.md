VERSION 0.0.1

# Game Play

This document describes the desired game play and interaction with the game simulation.

## Terms

- Game Client - Human scripted client which interacts with the game server API
- Game Viewer - 3d game client (open source)
- Game Server - A dedicated simulation server which both the Client and Viewer interact with (open source)
- Game Service - The infrastructure the game runs on

## Game Mechanics

The core concept is that of mechs. Huge lumbering robots packed full of weapons and sophisticated war technology.

Gameplay follows an FFA team deathmatch format where the last team standing wins.

## Human Interaction

The human player will write an interactive script (the "game client") in their programming language of choice which interfaces with the game server API. The API provides client authentication, the state of the game server and game simulation updates, as well as handles incoming commands from the game client.

Once the player is satisfied that their game client works after testing it against a local version of the game server, they will upload it to the game service. They must specify the language it was written with in the case of a dynamic language, otherwise it will be assumed there is a specifically named binary file present which will be ran on the server's linux architecture.

In the case of the game service, this code will be ran in jailed environments which offer consistent CPU and memory for each connected game client, as well as block access to the outside world to prevent logic being handed off to more powerful 3rd party hardware. The player can then launch the game viewer, log into their account, and initiate a match making request. Once enough players have been found, their game clients will be connected to the game server, and the game round will be initialized. The game viewer will receive streaming updates of the game in realtime.

## Client Interaction

The client will receive streaming updates from the gameserver.

These include:

- Server and World state updates
- Mech state updates
  - Full state of the client's mech
  - Limited state of other mechs within range
- Spawn/despawn messages for in-game objects including position and rotation data
- Update messages for existing in-game objects including position and rotation data
- Free form in-game communication messages from other mechs for coordination

### Victory Conditions

**Win Conditions**

- First team to elimate the enemy team wins
- At the end of the pre-determined round time...
  - The team with the most undefeated mechs wins
  - If equal team counts, the team with the least damage taken wins

**Draw Conditions**

- If no team deals damage within a pre-designated amount of time the game is considered a draw

### Game States

The game will transition between various states.

- Configuration - Waits for all clients to connect, and allows for a brief negotation phase
- Startup - Waits for the server to initialize, and give clients a final grace period to initialize themselves
- Game Phase- The actual game simulation phase which completes on either a victory condition or round timeout
- Game Over - Gives clients a chance to record any final logging data and exit before being force destroyed
- New Round - Special phase for self-hosted games which allows game rounds to continuously restart

### Chassis

During the pre-game round game clients will use limited point allocation to buy a specific chassis and load it out. Depending on the capabilities of a chassis its point cost can vary. This means that ability focused mechs will cost more, while more versatile mechs will rely on gear to supplement their build and thus have more points to spend.

#### Chassis Types

- Assault - Heavy armor tank
- Support - Capable of repairing allies, deploying shields or turrets, etc.
- Combat - Standard medium armored fighters
- Scout - High speed low armored fighters

These types provide the basis for specific chassis, each with their own unique set of abilities (typically on long cooldowns). These abilities are supplemented by the additional weapons, armor, and other unique components which can be bought with point allocation.

The difference in the weapon types means that there is also armor mechanics in play. Base armor covers everything at the cost of no specialization, ablative armor is more resistant to energy weapons, while reactive armor is more resistant to ballistic weapons.

Each chassis has limited and specific numbers of hardpoints in any given location, as well as max engine/reactor sizes, max armor per location, and internal module limits.

#### Pool Types

A "pool" refers to the mechanic behind ability usage. Each has a limited capacity.

- Energy - constantly recharges over time
- Ammo - has a specific reload time and limited total capacity
- Fuel - drives things like high speed dashing
- Overload - charges from taking damage, discharges in the form of higher powered damage
- Drone - deployable drones which replenish over time
- Material - allows the building of things like walls and turrets and replenishes over time

#### Loadout Modules

Any given chassis is limited by what they can purchase with allocation points.

### Computers

TODO

### Engines/Reactors

TODO

### Gyros

TODO

### Sensors

TODO

### Weapons

#### Energy

TODO

#### Ballistic

TODO

#### Missiles / Rockets

TODO

#### Artillery

TODO

#### Drone

TODO

#### Hand to Hand

TODO

#### Orbital

TODO

### Armor

#### Ablative

TODO

#### Reactive

TODO

### Communications

#### Signal Boosters

TODO

#### Signal Jammers

TODO

#### Satellite

TODO

#### Laser Communication

TODO

### Defense

#### Anti Missile Systems

TODO

#### Anti Air

TODO
