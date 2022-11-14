# Changelog for `cse230-ns-shaft`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.0.0 - 2022-11-14

### Code review
Initial update: (By Ziyu Tang)

preliminary framework, including:

1. Overall state data structure.
2. Platform generation.
3. Frame refresh logic.
4. UI and `quit` keyboard event handling.

### Task assignment:

1. Caiwei Wang: 
   1. onPlatform: logics to judge if the player is on platform or not, and handle the corresponding benefit/punishment.
2. Ruihan Yin: 
   1. keyboard event handling: logics for capturing the keyboard inputs and map them to the movement of the character on the canvas.
3. Lien-Bee Huang: 
   1. isDead: logics to judge if the game is over or not. 
   2. fall: logics to judge if the player is falling or not.