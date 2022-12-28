"Lance une interface web pour Picrochole."

import argparse
from builder import build_app

if __name__ == "__main__":
    descr = "Launch a GUI for Picrochole, a game simulating pre-napoleonic "\
            "and napoleonic warfare"
    parser = argparse.ArgumentParser(description=descr)
    parser.add_argument('game_dir', help="Directory containing the current "\
                                         "game")
    args = parser.parse_args()

    app = build_app(args.game_dir)
    app.run_server(debug=True)
