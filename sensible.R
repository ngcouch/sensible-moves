# Author: Nathan Couch
# Contact: ngcouch@gmail.com

# More of a running scratch-pad than a coherent analysis.


require(tidyverse)

# read in the data and do some initial cleaning
states = read.csv("game_tree.csv") %>%
  arrange(desc(depth), desc(children)) %>%
  mutate(forced = children == 1 & games > 1,
         color = if_else(depth %% 2 == 0, "White", "Black"),
         state = substr(state, 7, length(state)),
         state_copy = state,
         last       = str_extract(state, "[a-zA-Z0-9+#]+$"),
         capture    = str_detect(last, "x"),
         check      = str_detect(last, "\\+")) %>%
  separate(state_copy, into = c("Opening", "Opening reply")) %>%
  filter(depth > 1) %>%
  mutate(depth = depth - 1)

# get information about the power of the sample
states %>%
  group_by(depth, color) %>%
  filter(games > 1) %>%
  summarise(games = mean(games)) %>%
  ggplot(aes(depth/2, games, shape = color, group = 1)) + 
  geom_point() + 
  theme_bw() + 
  scale_x_continuous(breaks = c(0, 1, 2, 5, 10, 15, 20), limits = c(0, 20)) +
  scale_shape_manual(values = c(16, 1)) +
  ggtitle("Average games played at each observed game state") +
  scale_y_continuous(trans = "log10", breaks = c(5, 10, 100, 1000)) +
  ylab("Log games observed") + xlab("Move")

states %>%
  group_by(depth, color) %>%
  filter(games > 10) %>%
  mutate(n = n_distinct(states)) %>%
  summarize(n = unique(n),
            SD = sd(children),
            children = mean(children),
            SE  =SD/sqrt(n)) %>%
  ggplot(aes(depth/2, children, shape = color, group = 1)) + 
  geom_point() +
  theme_bw() +
  ggtitle("Empirical sensible replies on a particular ply") +
  xlab("Move") + ylab("Average number of observed replies") + 
  scale_x_continuous(breaks = c(0, 1, 2, 5, 10, 15), limits = c(0, 15)) +
  scale_shape_manual(values = c(16, 1))

states %>%
  group_by(depth, color) %>%
  filter(games > 10) %>%
  summarize(forced = mean(forced)) %>%
  ggplot(aes(depth/2, forced, shape = color, group = 1)) +
  geom_line() + geom_point() + 
  theme_bw() +
  ggtitle("Proportion of plies that are forced") +
  xlab("Move") + ylab("Forced/Total Plies") +
  scale_x_continuous(breaks = c(0, 1, 2, 5, 10, 15), limits = c(0, 15)) +
  scale_shape_manual(values = c(16, 1))
    
forced <-
  states %>%
  group_by(depth, color) %>%
  filter(games > 1) %>%
  summarize(forced = mean(forced)) 

mean(forced$forced)

forced %>%
  group_by(color) %>%
  summarize(forced = mean(forced))
    
states %>%
  ungroup() %>%
  summarise(unique = mean(games == 1),
            average_games = mean(games))

# count frequency of openings
states %>%
  group_by(Opening) %>%
  summarize(games = sum(games)) %>%
  arrange(desc(games)) %>%
  mutate(Opening = factor(Opening, ordered = T),
         Opening = fct_reorder(Opening, games)) %>%
  ggplot(aes(Opening, games)) +
  geom_point() + 
  scale_y_continuous(trans = "log10", breaks = c(5, 10, 100, 1000)) +
  theme_bw() + 
  ggtitle("Number of games with opening move")
  
states %>%
  group_by(Opening) %>%
  mutate(opening_games = sum(games)) %>%
  group_by(Opening, `Opening reply`, opening_games) %>%
  summarize(games = sum(games)) %>%
  arrange(desc(games)) %>%
  group_by(Opening) %>%
  mutate(percent_games = games/opening_games) %>%
  ungroup() %>%
  mutate(Opening = factor(Opening, ordered = T),
         Opening = fct_reorder(Opening, opening_games)) %>%
  ggplot(aes(Opening, percent_games)) +
  geom_point() + theme_bw() +
  ylab("Proportion games with reply")


states %>%
  group_by(Opening) %>%
  mutate(opening_games = sum(games)) %>%
  filter(opening_games > 100) %>%
  ungroup() %>%
  mutate(Opening = factor(Opening, ordered = T),
         Opening = fct_reorder(Opening, opening_games)) %>%
  group_by(Opening, depth) %>%
  summarise(forced = mean(forced)) %>%
  ggplot(aes(depth, forced)) +
  geom_point() +
  facet_wrap(~Opening) +
  theme_bw()

states %>%
  group_by(games) %>%
  summarize(states = n()) %>%
  ggplot(aes(games, states)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10")
  
states %>%
  filter(games > 10) %>%
  group_by(depth, color, forced) %>%
  summarize(captures = mean(capture)) %>%
  ggplot(aes(depth, captures, shape = color, group = 1)) +
  geom_point() + 
  facet_wrap(~forced) +
  theme_bw() +
  scale_shape_manual(values = c(16, 1)) +
  ggtitle("Proportion of moves that were captures by whether they were forcing")
  
states %>%
  filter(games > 10) %>%
  group_by(depth, color, forced) %>%
  summarize(checks = mean(check)) %>%
  ggplot(aes(depth, checks, shape = color, group = 1)) +
  geom_point() + 
  facet_wrap(~forced) +
  theme_bw() +
  scale_shape_manual(values = c(16, 1)) +
  ggtitle("Proportion of moves that were checks by whether they were forcing")


states %>%
  filter(games > 10) %>%
  group_by(depth, color, capture) %>%
  summarize(forces = mean(forced)) %>%
  mutate(capture = if_else(capture, "capture", "non-capture")) %>%
  ggplot(aes(depth/2, forces, shape = color, group = 1)) +
  geom_point() + 
  facet_wrap(~capture) +
  theme_bw() +
  scale_shape_manual(values = c(16, 1))  +
  ggtitle("Proportion of plies that are forcing") +
  xlab("Move")

states %>%
  filter(games > 10) %>%
  group_by(depth, color, check) %>%
  summarize(forces = mean(forced)) %>%
  mutate(check = if_else(check, "check", "non-check")) %>%
  ggplot(aes(depth/2, forces, shape = color, group = 1)) +
  geom_point() + 
  facet_wrap(~check) +
  theme_bw() +
  scale_shape_manual(values = c(16, 1))  +
  ggtitle("Proportion of plies that are forcing") +
  xlab("Move")

states %>%
  filter(games > 10) %>%
  group_by(depth, color) %>%
  summarize(children = mean(children)) %>%
  ggplot(aes(depth/2, children, shape = color, group = 1)) +
  geom_point() + 
  theme_bw() +
  scale_shape_manual(values = c(16, 1))  +
  ggtitle("Number of observed replies") +
  xlab("Move") + ylab("Possible replies")
  
states %>%
  filter(games > 10) %>%
  group_by(depth) %>%
  summarize(captures = mean(capture)) %>%
  ggplot(aes(depth, captures)) +
  geom_point() +
  theme_bw()
  
states %>%
  filter(games > 10) %>%
  group_by(depth) %>%
  summarize(checks = mean(check)) %>%
  ggplot(aes(depth, checks)) +
  geom_point() +
  theme_bw()  
  
states %>%
  filter(games > 1) %>%
  ggplot(aes(depth, games)) +
  geom_point() +
  scale_y_continuous(trans = "log10") +
  theme_bw()

states %>%
  mutate(total_games = sum(games)) %>%
  filter(games > 1) %>%
  group_by(depth) %>%
  mutate(max_games = max(games),
         prop_games = max_games/total_games) %>%
  filter(games == max_games) %>%
  select(state, depth, prop_games) %>%
  ggplot(aes(depth, prop_games)) +
  geom_point() + theme_bw()

states %>%
  group_by(last) %>%
  filter(games > 10) %>%
  summarize(n = n_distinct(state),
            forced = mean(forced)) %>%
  ungroup() %>%
  mutate(last = factor(last, ordered = T),
         last = fct_reorder(last, n)) %>%
  ggplot(aes(as.numeric(last), n)) +
  geom_point() +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10")


states %>%
  filter(games > 2) %>%
  arrange(desc(depth)) %>%
  head()


states %>%
  filter(games > 10) %>%
  group_by(depth, color) %>%
  mutate(sample = n_distinct(state)) %>%
  select(depth, color, children, games, forced, sample) %>% 
  summarise(across(.fns = mean)) %>%
  ggplot(aes(depth/2, sample, shape = color)) + 
  geom_point() +
  xlab("Move") +
  theme_bw() +
  scale_shape_manual(values = c(16, 1)) +
  ggtitle("Number of distinct states observed >10 times at each depth")
  
states %>%
  filter(games > 10, depth < 9) %>%
  group_by(depth, color) %>%
  summarize(sample = sum(games)) %>%
  full_join(legal_states) %>%
  ggplot(aes(depth/2, shape = color)) +
  geom_path(aes(y=legal_states, linetype = "Legal States")) +
  geom_path(aes(y=sample, linetype = "Games")) +
  theme_bw() +
  scale_y_continuous(trans = "log10") +
  ylab("Log observed") + xlab("Move") +
  scale_shape_manual(values = c(16, 1))
  
states %>%
  filter(games > 10, depth < 9) %>%
  group_by(depth, color) %>%
  summarize(sample = sum(games)) %>%
  full_join(legal_states) %>%
  ggplot(aes(sample, legal_states, shape = color)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(trans = "log10") +
  ylab("Log legal states") + xlab("Observed states") +
  scale_shape_manual(values = c(16, 1))


