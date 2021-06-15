# Author: Nathan Couch
# Contact: ngcouch@gmail.com

'''
Takes a cleaned up version of the Lichess Elite games database,
creates a move tree from a randomly sampled subset of the games,
and then calculates some statistics.

Outputs a csv file containing move sequences, as well as associated statistics
including how often they were observed.
'''

### SETUP ###
import statistics
import collections
import itertools
from functools import reduce
import time
import random


# Define a few utility functions
# Ideally, these would be methods, but here we are.

'''
Defines the game tree. At the root of the game tree is a single node, which
represents the starting position. Each of its descendants represents a 
possible continuation of the game. The terminal nodes are counts of how often
that particular game was observed in the dataset. Note that branches of the game
tree are themselves game trees, with differing root nodes.
'''

tree = lambda: collections.defaultdict(tree)

def add_element(root, path):
    # takes a game tree and a move sequence and 
    # adds a branch representing the game. If
    # the game is already observed, increment 
    # the count, otherwise add the branch.	

    if path[0] == '':
        pass

    elif len(path) == 1:
        root[path[0]] = root.get(path[0], 0) + 1

    else:
        add_element(root[path[0]], path[1:])

def get_element(root, path):
    # retrieves the terminal node of a sequence of moves

    if len(path) == 1:

        return root[path[0]]

    else:

        return get_element(root[path[0]], path[1:])

def count_games_with_state(root, path):

    # Takes a path to a node, and then finds the sum of
    # all terminal nodes of its descendants. This gives
    # the number of games that were observed that reached
    # the point in the game defined by path.

    node = get_element(root, path)
    
    if type(node) is int:
        # If the node is terminal, return it

        return node

    else:

        # Otherwise, apply the algorithm recursively to it's 
        # children and take the sum.

        cum = 0
        children = node.keys()

        for c in children:

            new_path = path + [c]

            cum += count_games_with_state(root, new_path)

        return cum

### DATA INPUT ###

startTime = time.time()

print("Reading in games")
games = open("elite.sav", "r").read().split("\n")

endTime = time.time() - startTime
num_games = len(games)

print("Read {num} games in {time} seconds".format(num = num_games, time = endTime))

# sample 10^5 random games
games = random.sample(games, 10**5)

# Process the data file to recover the individual games
print("Processing games")
startTime = time.time()

games = [["Start"] + game.split(" ") for game in games]

plies = []

endTime = time.time() - startTime
print("{n} games processed in {time} seconds".format(n=len(games), time = endTime))

# define game tree

startTime = time.time()
      
print("Building game tree")
game_tree = tree()

# iterate over the games, building up the empirical move tree
for game in games:

    add_element(game_tree, game)

endTime = time.time() - startTime
print("Tree built in {} seconds".format(endTime))

options = []
current = []
nodes = [["Start"]]

# traverse the game tree, keeping track of the number of options at each ply
print("Counting options")

startTime = time.time()

while len(nodes) > 0:

    # retrieve a path from the queue
    current = nodes.pop()
    full_path = " ".join(current)

    # calculate game depth
    depth   = len(current)

    # get children
    children = get_element(game_tree, current)

    if type(children) is int:

        # if the node is terminal, do nothing.

        pass

    else:

        total_games = count_games_with_state(game_tree, current)
        replies = children.keys()        

        # only continue down the path if there is more than 1 game from this position
        if total_games > 1:
    
            # add replies to the queue
            to_add = map(lambda x: current + [x], replies)
            nodes.extend(to_add)

        # save data
        options.append([full_path, depth, len(replies), total_games])

endTime = time.time() - startTime

print("{states} game states processed in {time} seconds".format(states = len(options), time = endTime))
        
#   number of plys in tree
total_plys = len(options)

# filter out and count forced moves
unforced_plys = list(filter(lambda x: not (x[2] == 1 & x[3] > 1), options))
forced_plys   = total_plys - len(unforced_plys)

# calculate statistics
mean_options = statistics.mean([o[2] for o in options])
mean_actual  = statistics.mean([o[2] for o in unforced_plys])

game_lengths = list(map(len, games))
average_plys = statistics.mean(game_lengths)
median_plys  = statistics.median(game_lengths)
ply_counts  = collections.Counter(game_lengths)
modal_plys  = ply_counts.most_common(3)

# print statistics of interest
print("Average plys per game: {}".format(average_plys))
print("Median plys per game: {}".format(median_plys))
print("Modal plys per game:{}".format(modal_plys))
      
print("Total plys in tree: {}".format(total_plys))
print("Plys with more than one option: {}".format(len(unforced_plys)))
print("Forced plys: {}".format(forced_plys))

print("Average choices per ply: {}".format(mean_options))
print("Average number of actual choices per ply: {}".format(mean_actual))

print("Saving data")
# save processed data
with open("game_tree.csv", "w") as f:

    header = ",".join(["state","depth","children","games"])
    f.write(header+"\n")

    for o in options:

        outLine = ",".join([str(x) for x in o])
        f.write(outLine+"\n")
