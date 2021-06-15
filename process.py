# Author: Nathan Couch
# Contact: ngcouch@gmail.com

# Takes the Lichess Elite database and does some initial cleaning and processing

import collections
import glob

# get names of all files
files = glob.glob("elite/*.pgn")

# a list of games stripped of metadata
games = []

for f in files:

    # read in the data
    inDat = open(f, "r").readlines()

    # filter out lines of metadata
    inDat = [line for line in inDat if line[0] != "["]

    # get rid of empty lines
    inDat = [line.strip() for line in inDat if line != "\n"]

    # make it all sensible
    inDat = " ".join(inDat).split(" 1. ")

    # append to games
    games += inDat

# remove information indicating move number
games = [" ".join([ply for ply in game.split(" ") if ply[-1] != '.']) for game in games]

# write to file
with open('elite.sav', 'w') as f:
    for game in games:
        f.write("%s\n" % game)
    



    
