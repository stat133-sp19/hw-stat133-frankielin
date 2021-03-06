Workout 1
================
Frankie Lin

Introduction
============

<img src="../images/warriors-starting-5.jpg" width="1040" style="display: block; margin: auto;" />

The Golden State Warriors have been the dominant face of the NBA for the past few years with three championships in the past four years. Spouting a roster that has at least three All-Stars every year since their unprecedented and historic run the Golden State Warriors, along with coach Steve Kerr, have solidified their position as a dynasty within the NBA. Even coming to topple the King himself, Lebron James, and effectively changing the composition of the league, the Warriors have left their mark on history. But with rumblings across the league and ever shifting landscape that is basketball, all dynasties are destined to come to an end.

Background/Motivation
=====================

With rumors of trades along the lines of Kevin Durant to the New York Knicks, Klay Thompson to Los Angeles, or even Draymond Green out of the team once and for all, the league may be in for another historical shift in power. Though many of these trades are speculation as the trade deadline has come and pass for this season, it still begs the question of how and why the Golden State Warriors became and remained a dominant team. In this report, we hope to dissect how Golden State’s five starters impact the game and particularly the impact they had during the 2016 season. In this report we present data and analysis on the Golden State Warrior to dissect what makes the team so successful and, in particuliar, show why team synergy has allowed this dynasty to thrive.

Data
====

``` r
shots_data <-  as.tbl(read.csv("../data/shots-data.csv", row.names = 1))

kable(head(shots_data))
```

| team\_name            | game\_date |  season|  period|  minutes\_remaining|  seconds\_remaining| shot\_made\_flag | action\_type         | shot\_type     |  shot\_distance| opponent              |    x|    y| name           |  minute|
|:----------------------|:-----------|-------:|-------:|-------------------:|-------------------:|:-----------------|:---------------------|:---------------|---------------:|:----------------------|----:|----:|:---------------|-------:|
| Golden State Warriors | 3/24/17    |    2016|       3|                   2|                  35| shot\_no         | Alley Oop Dunk Shot  | 2PT Field Goal |               0| Sacramento Kings      |    0|    1| Andre Iguodala |      34|
| Golden State Warriors | 11/3/16    |    2016|       2|                  10|                  51| shot\_no         | Alley Oop Dunk Shot  | 2PT Field Goal |               1| Oklahoma City Thunder |  -12|   13| Andre Iguodala |      14|
| Golden State Warriors | 10/25/16   |    2016|       2|                   0|                   6| shot\_yes        | Alley Oop Dunk Shot  | 2PT Field Goal |               0| San Antonio Spurs     |    0|    1| Andre Iguodala |      24|
| Golden State Warriors | 11/3/16    |    2016|       2|                  11|                  10| shot\_no         | Alley Oop Layup shot | 2PT Field Goal |               1| Oklahoma City Thunder |   -1|   11| Andre Iguodala |      13|
| Golden State Warriors | 1/8/17     |    2016|       4|                   0|                  32| shot\_yes        | Cutting Dunk Shot    | 2PT Field Goal |               0| Sacramento Kings      |    0|    1| Andre Iguodala |      48|
| Golden State Warriors | 3/16/17    |    2016|       3|                   5|                  30| shot\_yes        | Cutting Dunk Shot    | 2PT Field Goal |               0| Orlando Magic         |    0|    1| Andre Iguodala |      31|

Utilizing shot level data from the team’s 2016 season, we hope to take a look a critical factors that the make the Warriors what they are. In this report we focus on five major players: Stephen Curry, Klay Thompson, Kevin Durant, Draymond Green, and Andre Iguodala, the famous starting five for the Warriors. All these players have been extremely valuable for the Warriors, though each in a different way. With each of therse data set, we have charted out both efficiency numbers as well as the types of shots they have taken. With this we hope to uncover the key to the Golden State Warriors' success and ultimately show why teamwork has been the key factor for the Warriors' success.

Charts
======

### 2PT Effective Shooting % by Player

``` r
kable(
shots_data %>%
  filter(shot_type == "2PT Field Goal") %>%
  group_by(name) %>%
  summarise(total = n(),
            made = sum(shot_made_flag == "shot_yes"),
            percentage = paste0(round(made/total*100, 1), "%")) %>%
  arrange(desc(percentage))
)
```

| name           |  total|  made| percentage |
|:---------------|------:|-----:|:-----------|
| Andre Iguodala |    210|   134| 63.8%      |
| Kevin Durant   |    643|   390| 60.7%      |
| Stephen Curry  |    563|   304| 54%        |
| Klay Thompson  |    640|   329| 51.4%      |
| Draymond Green |    346|   171| 49.4%      |

### 3PT Effective Shooting % by Player:

``` r
kable(
shots_data %>%
  filter(shot_type == "3PT Field Goal") %>%
  group_by(name) %>%
  summarise(total = n(),
            made = sum(shot_made_flag == "shot_yes"),
            percentage = paste0(round(made/total*100, 1), "%")) %>%
  arrange(desc(percentage))
)
```

| name           |  total|  made| percentage |
|:---------------|------:|-----:|:-----------|
| Klay Thompson  |    580|   246| 42.4%      |
| Stephen Curry  |    687|   280| 40.8%      |
| Kevin Durant   |    272|   105| 38.6%      |
| Andre Iguodala |    161|    58| 36%        |
| Draymond Green |    232|    74| 31.9%      |

### Effective Shooting % by Player:

``` r
kable(
shots_data %>%
  group_by(name) %>%
  summarise(total = n(),
            made = sum(shot_made_flag == "shot_yes"),
            percentage = paste0(round(made/total*100, 1), "%")) %>%
  arrange(desc(percentage))
)
```

| name           |  total|  made| percentage |
|:---------------|------:|-----:|:-----------|
| Kevin Durant   |    915|   495| 54.1%      |
| Andre Iguodala |    371|   192| 51.8%      |
| Klay Thompson  |   1220|   575| 47.1%      |
| Stephen Curry  |   1250|   584| 46.7%      |
| Draymond Green |    578|   245| 42.4%      |

### Field Goal Totals by Player

``` r
kable(
left_join(
  shots_data %>%
  filter(shot_type == "2PT Field Goal") %>%
  group_by(name) %>%
  summarise(total = n(),
            made = sum(shot_made_flag == "shot_yes"),
            percentage = paste0(round(made/total*100, 1), "%")) %>%
  arrange(desc(percentage)), shots_data %>%
  filter(shot_type == "3PT Field Goal") %>%
  group_by(name) %>%
  summarise(total = n(),
            made = sum(shot_made_flag == "shot_yes"),
            percentage = paste0(round(made/total*100, 1), "%")) %>%
  arrange(desc(percentage)), by = "name") %>% 
  select(name, made.x , made.y) %>%
  rename(Two_Pointers_Made = made.x, Three_Pointers_Made = made.y) %>%
  mutate(Total_Field_Goal_Points = Two_Pointers_Made * 2 + Three_Pointers_Made  *3) %>%
  arrange(desc(Total_Field_Goal_Points))
)
```

| name           |  Two\_Pointers\_Made|  Three\_Pointers\_Made|  Total\_Field\_Goal\_Points|
|:---------------|--------------------:|----------------------:|---------------------------:|
| Stephen Curry  |                  304|                    280|                        1448|
| Klay Thompson  |                  329|                    246|                        1396|
| Kevin Durant   |                  390|                    105|                        1095|
| Draymond Green |                  171|                     74|                         564|
| Andre Iguodala |                  134|                     58|                         442|

Analysis
========

<img src="../images/gsw-shot-chart.png" style="display: block; margin: auto;" />

In our diagram above we show the shot distribution of the players. Note that each of the players has a very different shot pattern.This provides us with the first major insight regarding the the play styles of the key Golden State players. By understanding how and when they shoot, we can begin to understand why they play the way they do. More so, these tables leverage the the data by prociding context to the true reason why this team has become what it is today.

Iguodala and Green, as expected, seem to play off the ball a lot more than the other players, with Iguodala taking the least amount of shots of all the five starters. However, it is abundantly clear that with Iguodala’s high shot percentage in both the 2 point range as well as overall, shows that he only takes shots that he feels comfortable taking. These numbers rank in the top 2 for the player analyzed sitting at 63.8% and 51.8% respectively. It is still important to note however, that Iguodala does not attempt many shots in the first place and thus his presence on the field is often characterized an extremely capable off-the-ball player.

Differing from Iguodala, Green not only takes more shots but plays as a hybrid interior shooter. Green typically prefers to take shots from either outside the perimeter or inside the paint, with little attempts made in the spaces in between. Taking far more shots, however, has quite a few consequences as Draymond Green, in all aspects, is not an accurate shooter. Sitting dead last in efficiency for all shot categories, Drayond’s strength isn’t really with his shot, but rather much like Iguodala, the off the ball play.

On the other hand, when taking a look at the primary scorers for the Golden State Warriors, we begin to observe two very different archetypes. The first of which is the mid range shooter. This type of player, though taking a fair number of shots outside the perimeter and in the paint, isn’t afraid to shoot from the inside the perimeter. The other archetype is the three-point shooter. This player prioritizes the 3-point shot over most other shots and thus is marked by an extraordinary large number of shows outside the perimeter.

Taking those archetypes into account, it is very easy to see that Kevin Durant is a perfect fit for the mid range shooter. Though he takes quite a few number of three-pointers, as does every player on the Golden State Warriors, Durant succeeds with being able to make a shot from anywhere. When diving further and taking look at his shot percentages in the tables below, we note that Durant is one of the most effective shooters on the team. When it comes to the two point shot, not only does he have one of the highest shot percentages at 60.7%, he records the most shots taken at that range. This leads Durant to hold an extremely impressive overall shot percentage and leading the team at 54.1%.

Taking a look at probably the most popular player on the team, Stephen Curry, we see the mold of the new super star—the three-point shooter. Taking 50% shots outside the perimeter than shots taken in inside the line, Curry is the definition of a new age player. Shooting at 40.8% from the three makes Curry one of the deadliest players in the league. This, however does not mean he is invincible, despite having a great touch beyond the perimeter, Curry’s shot percentage from the two is drastically lower than both Andrew Iguodala’s and Kevin Durant’s, sitting at only 54% compared to 63% and 60% respectively.

Though Curry is the face of the team, there is one player that beats even him from beyond the perimeter, Klay Thompson. With comparable shot frequencies across the board, Klay surpasses Curry in terms of efficiency in both the three-pointer and the overall, only losing out slightly in terms of two-point percentage. Klay represents himself as a true star without the ego being a viable second threat that’s just as deadly as the first. Coming second place to only Curry in total field goals made, at 1396 points, Klay is a monster in his own right. Being unafraid of a midrange jumper, lay-up, or a three-pointer, Klay Thompson is the final piece of the Warriors puzzle shooting at 47.1% and unwavering in the pursuit of greatness.

Discussion
==========

From these limited stats alone, we begin to see the image of an extremely formidable basketball team arise. With a healthy dose of extremely strong off the ball players such as Green and Iguodala and deadly shooters such as Klay, Durant, and Curry, the Golden State Warriors are almost without fault. With great shooting percentages inside the paint and both inside and out of the perimeter, it becomes apparent why the Warriors have been able to dominate the NBA scene for so long. The lack ego and the raw talent with regards to the starting five makes this team truly unbeatable. Being able to spread the floor like no other, the Warriors are deadly at no matter the distance from the rim.

More than that, all the Warriors players have shown an ability to shoot (though many more capable than others), providing an additional depth for their offensive prowess. This, in conjunction with stellar efficiency across the board at any distance, makes Golden State nearly impossible to defend against. By being a team with 5 potential threats, there no longer become one key player to guard but rather an entire team to focus on. The unique blend of this core group has allowed the Warriors to flourish in every season since their formation and it would be an understatement to say that they have changed the league. Even now, we see teams beginning to follow the "Golden Formula," building a strong team centered on scorers. However, those teams have yet to capture the magic that the Warriors have. But ironically, that's just it. The Warriors play like a true team. Unselfish. Willing to pass the ball. The Warriors simply play as equals. Despite the wide range of stats shown above, they are each just as willing to take a shot as they are to pass the ball. With this versatility the Warriors have become what we now see as the new NBA team.

Conclusion
==========

In all, the accumulation of talent on the Golden State Warriors has been nothing but extraordinary. Playing around the three-point ball, the team has successful changed the mindset of the league and become the reigning kings of the court. When this dynasty comes to an end, we will all remember the Golden State Warriors as the team that defied the logic when it came to crafting a roster. Implementing players and a system that had never been thought of, the Warriors truly shifted the NBA into a new era. Outside of Lebron there has been no group of core players that have had such an impact on the league. Going outside of the court, the Warriors have inspired a new generation of kids that they can, too succeed through unconventional means. Gone are the days of needing to be a personality dominant super star. In the new era of basketball, even the quiet kid in the corner who isnt 6'11" can succeed. With the power of the whole rather than the indivdual, the Warrirs have proven a new formula for success.

References
==========

Images: Getty Images
