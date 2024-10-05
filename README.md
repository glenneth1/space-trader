
# Space Trader Game

**Space Trader** is a text-based trading and exploration game written in **Guile Scheme** and adapted for web-based interaction using **Guile-Hoot**. The player assumes the role of a space pilot who travels between planets, engages in trading, completes missions, and manages their ship's resources such as fuel, repairs, and finances.

## Game Overview

The game begins on **Earth**, where the player can choose to earn additional funds before embarking on space exploration. The player's objective is to increase wealth through strategic trading, while managing critical resources such as fuel, ship repairs, and travel time. The game incorporates a variety of mechanics to make each journey challenging, including random events, ship damage, and the need for regular engine service.

## Features

- **Planetary Trading**: Each planet has a dynamic market where players can buy and sell goods such as food, water, and technology. Special items unique to each planet can be traded for higher profit margins.
  
- **Travel Mechanics**: Travel between planets requires fuel and time. Each trip has a fuel cost and a time duration based on the distance between planets. Random events may occur during travel, adding risk and opportunity.

- **Resource Management**: Players must carefully manage their ship's fuel, credits, and cargo capacity. Fuel can be refilled and ship repairs can be conducted on planets, but both come at a cost.
  
- **Random Events and Damage**: During space travel, random events (such as pirate attacks or tech booms) may occur, and the player's ship can take damage. Repairs are necessary to avoid long-term consequences.

- **Missions**: On each planet, the player can accept missions that involve transporting goods or passengers, or dealing with challenges that arise during space exploration.

- **Regular Engine Service**: To ensure the ship remains in good working order, the player must service the ship’s engines regularly. Neglecting engine maintenance can increase the risk of ship damage.

## Future Aspirations

The current version of **Space Trader** lays the groundwork for a fully interactive space-trading simulation. Future enhancements include:

1. **Expanded Universe**: More planets with unique markets, cultures, and challenges.
2. **Enhanced Missions**: Complex missions with multiple steps and story arcs that influence the player's reputation and relationships with planetary factions.
3. **Crew Management**: Introduce the ability to hire and manage a crew, with each member offering different skills that impact travel, trading, or combat scenarios.
4. **Combat System**: Space combat mechanics where the player can engage with pirates or other hostile forces.
5. **Upgradable Ships**: Offer the player options to upgrade their ship's cargo capacity, engines, or weapon systems.
6. **Economy Simulation**: A more dynamic economic system where prices fluctuate based on supply, demand, and political events.

## Running the Game

To run the game in the browser:

1. Install **Guile-Hoot** using Guix:
   ```bash
   guix install guile-hoot
   ```

2. Clone this repository and navigate to the project directory.

3. Compile the Scheme code into JavaScript using Guile-Hoot:
   ```bash
   hootc main.scm -o space-trader.js
   ```

4. Serve the game using a web server. For example, you can use Python’s built-in HTTP server:
   ```bash
   python3 -m http.server
   ```

5. Open your browser and go to `http://localhost:8000/` to start playing.

## Contributing

Contributions to expand the game’s features, fix bugs, or improve gameplay are welcome. Feel free to submit issues or pull requests on the project’s repository.
