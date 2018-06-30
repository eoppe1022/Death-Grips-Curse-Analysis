library(tidyverse)
library(geniusR)
library(tidytext)
library(ggthemes)
library(scales)

theme_awesome <- theme_set(

    theme_minimal() +
    theme(plot.title = element_text(hjust = 0, face = "bold", family = "mono", size = 32, margin = margin(1,1,20,1)),
          axis.title = element_text(face = "bold", family = "mono", size = 25),
          axis.text = element_text(family = "mono", face = "bold", size = 16),
          axis.title.x = element_text(margin = margin(35, 0, 0, 0)),
          axis.title.y = element_text(margin = margin(0, 35, 0, 0)),
          plot.margin = margin(25, 25, 25, 25),
          legend.title = element_text(family = "mono", face = "bold", size = 20),
          legend.title.align = 0.5,
          legend.text = element_text(family = "mono", face = "bold", size = 16, margin = margin(1, 1, 1, 100)),
          legend.key.size = unit(1, "cm"),
          legend.box.background = element_rect(fill = "papayawhip", color = "darkgrey"),
          panel.background = element_rect(fill = "papayawhip", color = "papayawhip"),
          plot.background = element_rect(fill = "papayawhip"),
          panel.grid.major.x = element_line(linetype = "solid", color = "grey"),
          panel.grid.major.y = element_line(linetype = "solid", color = "grey"),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          strip.text = element_text(family = "mono", face = "bold", size = 16, margin = margin(0, 0, 10, 0)),
          strip.background = element_rect(fill = "white", color = "black"),
          plot.subtitle = element_text(size = 18, face = "bold", family = "mono", hjust = 0, margin = margin(0,0,30,0))))

albums <- tibble(artist = rep("Death Grips", 6),
                 album = c("Exmilitary", "The Money Store", "NO LOVE DEEP WEB", "Government Plates", "The Powers That B", "Bottomless Pit"))

lyrics <- albums %>% 
  mutate(tracks = map2(artist, album, genius_album)) %>%
  unnest(tracks)

tidy_lyrics <- lyrics %>%
  unnest_tokens(word, lyric) %>%
  drop_na()

song_list <- tidy_lyrics %>%
  group_by(artist, album, track_title) %>%
  summarize()

songs_with_fuck <- tidy_lyrics %>%
  filter(str_detect(word, "fuck")) %>%
  group_by(artist, album, track_title) %>%
  summarize(n_fuck = n()) %>%
  full_join(song_list, by = c("artist", "album", "track_title")) %>%
  replace(is.na(.), 0) %>%
  arrange(desc(n_fuck))

songs_with_curses <- tidy_lyrics %>%
  filter(str_detect(word, paste(c("bitch", "fuck", "nigg", "shit"), collapse = "|"))) %>%
  group_by(artist, album, track_title) %>%
  summarize(n_curse = n()) %>%
  full_join(song_list, by = c("artist", "album", "track_title")) %>%
  replace(is.na(.), 0) %>%
  arrange(desc(n_curse))

songs_with_fuck %>% filter(n_fuck == 0)
songs_with_curses %>% filter(n_curse == 0)

curses_rate <- tidy_lyrics %>%
  group_by(artist, album, track_title) %>%
  summarize(n_words = n()) %>%
  left_join(songs_with_curses, by = c("artist", "album", "track_title")) %>%
  replace(is.na(.), 0) %>%
  mutate(curses_per_hundred_words = ((n_curse / n_words) * 100) %>% round(2)) %>%
  arrange(desc(curses_per_hundred_words))

fuck_rate <- tidy_lyrics %>%
  group_by(artist, album, track_title) %>%
  summarize(n_words = n()) %>%
  left_join(songs_with_fuck, by = c("artist", "album", "track_title")) %>%
  replace(is.na(.), 0) %>%
  mutate(fucks_per_hundred_words = ((n_fuck / n_words) * 100) %>% round(2)) %>%
  arrange(desc(fucks_per_hundred_words))

# fuck histogram
songs_with_fuck %>%
  ungroup() %>%
  ggplot(aes(x = n_fuck)) +
  geom_histogram(fill = "darkmagenta", binwidth = 4) +
  labs(x = "# of 'Fuck' Occurrences", y = "Frequency", title = "Frequency of 'Fuck' Occurrences in Death Grips Songs", subtitle = "")

# curse histogram
songs_with_curses %>%
  ungroup() %>%
  ggplot(aes(x = n_curse)) +
  geom_histogram(fill = "maroon", binwidth = 4) +
  labs(x = "# of Curse Occurrences", y = "Frequency", title = "Frequency of Curse Occurrences in Death Grips Songs", subtitle = "")

# most curses song
songs_with_curses %>% 
  ungroup() %>%
  filter(n_curse >= quantile(n_curse, probs = 0.75)) %>%
  ggplot(aes(x = reorder(track_title %>% str_trunc(20), n_curse), y = n_curse, fill = album)) +
  geom_col() +
  labs(x = "", y = "# of Curses", title = "Death Grips Songs with Most Curses") +
  scale_fill_wsj(name = "Album") +
  theme_update(panel.grid.major.y = element_blank()) +
  coord_flip() 

# most fucks song
songs_with_fuck %>%
  ungroup() %>%
  filter(n_fuck >= quantile(n_fuck, probs = 0.75)) %>%
  ggplot(aes(x = track_title %>% str_trunc(20) %>% reorder(n_fuck), y = n_fuck, fill = album)) +
  geom_col() +
  labs(x = "", y = "# of 'Fuck' Occurrences", title = "Death Grips Songs with Most 'Fuck' Occurrences") +
  scale_fill_wsj(name = "Album") +
  coord_flip()

# most fucks album
songs_with_fuck %>%
  group_by(artist, album) %>%
  summarize(n_fuck = sum(n_fuck)) %>%
  ggplot(aes(x = album %>% str_trunc(20) %>% reorder(n_fuck), y = n_fuck, fill = album)) +
  geom_col(show.legend = FALSE) +
  labs(x = "", y = "# of 'Fuck' Occurrences", title = "Death Grips Albums with Most 'Fuck' Occurrences") +
  scale_fill_wsj() +
  coord_flip()

# most curses album
songs_with_curses %>%
  group_by(artist, album) %>%
  summarize(n_curse = sum(n_curse)) %>%
  ggplot(aes(x = album %>% str_trunc(20) %>% reorder(n_curse), y = n_curse, fill = album)) +
  geom_col(show.legend = FALSE) +
  labs(x = "", y = "# of Curses", title = "Death Grips Albums with Most Curses") +
  scale_fill_wsj() +
  coord_flip()

# highest curses % album
curses_rate %>%
  ungroup() %>%
  filter(curses_per_hundred_words >= quantile(curses_per_hundred_words, probs = 0.75)) %>%
  ggplot(aes(x = track_title %>% str_trunc(20) %>% reorder(curses_per_hundred_words), y = curses_per_hundred_words, fill = album)) +
  geom_col() +
  labs(x = "", y = "# of Curses per 100 Words", title = "Death Grips Songs with Highest % of Curses") +
  scale_fill_wsj(name = "Album") +
  coord_flip()

# highest fucks % song
fuck_rate %>%
  ungroup() %>%
  filter(fucks_per_hundred_words >= quantile(fucks_per_hundred_words, probs = 0.75)) %>%
  ggplot(aes(x = track_title %>% str_trunc(20) %>% reorder(fucks_per_hundred_words), y = fucks_per_hundred_words, fill = album)) +
  geom_col() +
  labs(x = "", y = "# of 'Fucks' Said per 100 Words", title = "Death Grips Songs with Highest % of `Fucks`") +
  scale_fill_wsj(name = "Album") +
  coord_flip()

# highest curse % album
curses_rate %>%
  group_by(artist, album) %>%
  summarize(n_curse = sum(n_curse), n_words = sum(n_words)) %>%
  mutate(curses_per_hundred_words = ((n_curse / n_words) * 100) %>% round(2)) %>%
  ungroup() %>%
  ggplot(aes(x = album %>% str_trunc(20) %>% reorder(curses_per_hundred_words), y = curses_per_hundred_words, fill = album)) +
  geom_col(show.legend = FALSE) +
  labs(x = "", y = "# of Curses per 100 Words", title = "Death Grips Albums with Highest % of Curses") +
  scale_fill_wsj() +
  coord_flip()

# highest fucks % album
fuck_rate %>%
  group_by(artist, album) %>%
  summarize(n_fuck = sum(n_fuck), n_words = sum(n_words)) %>%
  mutate(fucks_per_hundred_words = ((n_fuck / n_words) * 100) %>% round(2)) %>%
  ungroup() %>%
  ggplot(aes(x = album %>% str_trunc(20) %>% reorder(fucks_per_hundred_words), y = fucks_per_hundred_words, fill = album)) +
  geom_col(show.legend = FALSE) +
  labs(x = "", y = "# of 'Fucks' per 100 Words", title = "Death Grips Albums with Highest % of Fucks") +
  scale_fill_wsj() +
  coord_flip()

songs_with_curses %>%
  ungroup() %>%
  ggplot(aes(x = reorder(track_title %>% str_trunc(20), n_curse), y = n_curse, fill = album)) +
  geom_col(width = 0.9, show.legend = FALSE) +
  facet_wrap(~album, ncol = 3, scales = "free") +
  labs(x = "", y = "", title = "Death Grips Songs with Most Curses", subtitle = "# of Curses on X-Axis (different scale for each album)") +
  scale_fill_wsj(name = "Album") +
  coord_flip() 

songs_with_fuck %>%
  ungroup() %>%
  ggplot(aes(x = reorder(track_title %>% str_trunc(20), n_fuck), y = n_fuck, fill = album)) +
  geom_col(width = 0.9, show.legend = FALSE) +
  facet_wrap(~album, ncol = 3, scales = "free") +
  labs(x = "", y = "", title = "Death Grips Songs with Most 'Fuck' Occurrences", subtitle = "# of 'Fuck' Occurrences on X-Axis (different scale for each album)") +
  scale_fill_wsj(name = "Album") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  coord_flip() 

fuck_rate %>%
  ungroup() %>%
  ggplot(aes(x = reorder(track_title %>% str_trunc(20), fucks_per_hundred_words), y = fucks_per_hundred_words, fill = album)) +
  geom_col(width = 0.9, show.legend = FALSE) +
  facet_wrap(~album, ncol = 3, scales = "free") +
  labs(x = "", y = "# of 'Fucks' per 100 Words", title = "Death Grips Songs with Most 'Fuck' Occurrences") +
  scale_fill_wsj(name = "Album") +
  theme_update(axis.title = element_text(hjust = 0.4, size = 18), plot.title = element_text(hjust = 0.2)) +
  coord_flip() 

curses_rate %>%
  ungroup() %>%
  mutate(track_title = str_trunc(track_title, 20)) %>%
  mutate(track_title = reorder(track_title, curses_per_hundred_words)) %>%
  ggplot(aes(x = track_title, y = curses_per_hundred_words, fill = album)) +
  geom_col(width = 0.9, show.legend = FALSE) +
  facet_wrap(~album, ncol = 3, scales = "free") +
  labs(x = "", y = "# of Curses per 100 Words", title = "Death Grips Songs with Most 'Fuck' Occurrences") +
  scale_fill_wsj(name = "Album") +
  theme_update(axis.title = element_text(hjust = 0.4, size = 18), plot.title = element_text(hjust = 0.2)) +
  coord_flip() 

