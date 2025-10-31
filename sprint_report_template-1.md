# Sprint x Report 
Video Link: 
## What's New (User Facing)
 * Fixed login system
 * Initial Course list (not scraped yet, just samples so we can test review making)
 * Initial Prof list
 * Microanimations
 * light/dark mode
 * shading

## Work Summary (Developer Facing)
We started with implementing samples of the course and professor lists so we could make the review writing system properly setup. We fixed the log in system, and added an actual user profile section that lets users see their published reviews, password, username, etc. We added a review writing system that lets people write reviews from 3 places. Starting a review from scratch from the home page, starting a professor specific review from the professor tab by clicking the review button on a specified prof, and from the course list, the same way as the professor.

## Unfinished Work
We did not fully get the course and prof lists scraped as we initially intended, instead focusing on making them work with the review writing. The user profile system is somewhat screwed up visually, as teh style.css was acting odd at the end. Will be fixed, alongside other simple UI fixes later on.

## Completed Issues/User Stories
Here are links to the issues that we completed in this sprint:
 * URL of issue 1 - https://github.com/Bioticcc/Rate-My-Prof-WSU/issues/23
 * URL of issue 2 - https://github.com/Bioticcc/Rate-My-Prof-WSU/issues/25
 * URL of issue 3 - https://github.com/Bioticcc/Rate-My-Prof-WSU/issues/20
 * URL of issue 3 - https://github.com/Bioticcc/Rate-My-Prof-WSU/issues/21
 * URL of issue 4 - https://github.com/Bioticcc/Rate-My-Prof-WSU/issues/26
 * URL of issue 5 - https://github.com/Bioticcc/Rate-My-Prof-WSU/issues/10
 * URL of issue 6 - https://github.com/Bioticcc/Rate-My-Prof-WSU/issues/16

 ## Incomplete Issues/User Stories 
 * URL of issue 1 - https://github.com/Bioticcc/Rate-My-Prof-WSU/issues/14 - Still deciding wether or not to do student ID or SSO. at this point we got to do student ID though, so going to implement that next time.


## Code Files for Review
Please review the following code files, which were actively developed during this sprint, for quality:
 * UI.R https://github.com/Bioticcc/Rate-My-Prof-WSU/blob/main/project/ui.R
 * Server.R https://github.com/Bioticcc/Rate-My-Prof-WSU/blob/main/project/server.R
 * Global.R https://github.com/Bioticcc/Rate-My-Prof-WSU/blob/main/project/global.R
 * Style.css https://github.com/Bioticcc/Rate-My-Prof-WSU/blob/main/project/www/style.css
 
## Retrospective Summary
Here's what went well:
  * The sample course and prof lists look great! as soon as we have the full course list to add.
  * Log in system succesful, worked when testing between multiple machines, so thats great
  * Reviews were postable, not viewable between machines but mainly due to the site not actually being up/persistent yet. Logins are stored locally, so those ARE persistent.
 
Here's what we'd like to improve:
   * Random UI shenanigans deciding to act funky between commits for little to no reason
   * Some text showing as the same color as its backgrround when swapping between light and dark mode
   * Getting that scraping done!
  
Here are changes we plan to implement in the next sprint:
   * FUlly scraped list of both course and prof lists directly from WSU website
   * Review rating system
   * UI improvements

   * Item x
