> module Antioch.TimeAccounting where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities

Time Accounting is conceptually simple: sessions and projects have been
allotted only so much time, and we don't want to schedule them over this
amount of time.

But the devil is in the details.  Like:

   * Project Time vs. Session Time - a session can't be scheduled anymore if
   it's project is out of time.  Thus you have to check both session & project.

   * Project.maxSemesterTime - a project, especially a large one, may only
   be allowed to observe so much in a single semester (and this may be
   less then the projects total alloted time).  This means that query's into
   the available time of a semester have to specify the semester as well.

   * What Else???

TBF: these naming conventions suck.  Need to rename them ...

> totalUsed :: Session -> Minutes
> totalUsed = sum . map duration . periods

Returns the minutes available for scheduling for this session,
i.e., time that is not encumbered in any way and therefore
completely open for scheduling, tentative or not.

> totalAvail :: Session -> Minutes
> totalAvail s = (totalTime s) - (totalUsed s)

The time available to this session might actually be further restricted by 
the time available to it's project, which may depend on which semester it is.

> totalAvail' :: Session -> String -> Minutes
> totalAvail' s sem = min (totalAvail s) (timeAvail' (project s) sem)
> timeUsed :: Project -> Minutes
> timeUsed = sum . map totalUsed . sessions

> timeAvail :: Project -> Minutes
> timeAvail p = (timeTotal p) - (timeUsed p) 

Usually, the time available for a project is simply it's total time minus
the time it has already used up.  But for large projects, it may be allowed
only a certain amount of time per semester.

> timeAvail' :: Project -> String -> Minutes
> timeAvail' p sem = min ((timeTotal p) - (timeUsed p)) (timeAvailBySemester p sem)

> timeAvailBySemester :: Project -> String -> Minutes
> timeAvailBySemester p sem = (maxSemesterTime p) - (timeUsedBySemester p sem)

> timeUsedBySemester :: Project -> String -> Minutes
> timeUsedBySemester p sem = sum $ map (totalUsedBySemester sem) $ sessions p

> totalUsedBySemester :: String -> Session -> Minutes
> totalUsedBySemester sem s = sum $ map duration $ periodsBySemester s sem 

> periodsBySemester :: Session -> String -> [Period]
> periodsBySemester s sem = filter (isSemester sem) $ periods s
>   where
>     isSemester sem p = sem == (dt2semester  . startTime $ p)


