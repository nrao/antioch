> module Antioch.TimeAccounting where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities
> import Debug.Trace

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

   * Possible factors:
      - project/session close flag
      - project/session time available
      - project semester time available

The requirements below for project completion are drawn from Project
Note 11.3.  A project is considered closed within the DSS when any of:
   1. The total amount of time allotted for that project has been billed
      (less than 15 minutes remains),
   2. The project is marked as closed by a project investigator or the
      GBT telescope scheduler,

Checked factors:
   - project close flag
   - project time available

> pComplete :: Project -> Bool
> pComplete p = (pClosed p) || ((pAvailTotal p) <= quarter )
> -- NOTE: project completeness is NOT dependent on session completeness:
> --  where
> --    allSessClosed p = all (==True) $ map sClosed $ sessions p

How much time has this session used up in periods?
The requirements below for session completion are drawn from Project Note 11.3.
A session is considered closed within the DSS when any of:
   1. the total amount of time allotted for that session has been billed (less than 15 minutes remains),
   2. the session is marked as closed by a project investigator or the GBT telescope scheduler, or
   3. the project to which it belongs is closed.

Checked factors:
   - project close flag
   - session close flag
   - session time available

> sComplete :: Session -> Bool
> sComplete s = (sClosed s) || (pClosed . project $ s) || ((sAvailTotal s) <= quarter )

How much time has this session used up in periods?

> sUsed :: Session -> Minutes
> sUsed = sum . map pTimeBilled . periods

Returns the minutes available for scheduling for this session,
i.e., time that is not encumbered in any way and therefore
completely open for scheduling, tentative or not.

Checked factors:
   - session time available

> sAvailTotal :: Session -> Minutes
> sAvailTotal s = (sAlloted s) - (sUsed s)

The time available to this session might actually be further restricted by 
the time available to it's project, which may depend on which its semester.

Checked factors:
   - project time available
   - session time available
   - project semester time available

> sAvail :: Session -> String -> Minutes
> sAvail s sem = min (sAvailTotal s) (pAvail (project s) sem)

> pUsed :: Project -> Minutes
> pUsed = sum . map sUsed . sessions

Checked factors:
   - project time available

> pAvailTotal :: Project -> Minutes
> pAvailTotal p = (pAlloted p) - (pUsed p) 

Usually, the time available for a project is simply it's total time minus
the time it has already used up.  But for large projects, it may be allowed
only a certain amount of time per semester.

Checked factors:
   - project time available
   - project semester time available

> pAvail :: Project -> String -> Minutes
> pAvail p sem = min (pAvailTotal p) (pSemesterRemainingTime p sem)

> pSemesterRemainingTime :: Project -> String -> Minutes
> pSemesterRemainingTime p sem = (maxSemesterTime p) - (pUsedBySemester p sem)

> pUsedBySemester :: Project -> String -> Minutes
> pUsedBySemester p sem = sum $ map (sUsedBySemester sem) $ sessions p

> sUsedBySemester :: String -> Session -> Minutes
> sUsedBySemester sem s = sum $ map pTimeBilled $ periodsBySemester s sem 

> periodsBySemester :: Session -> String -> [Period]
> periodsBySemester s sem = filter (isSemester sem) $ periods s
>   where
>     isSemester sem p = sem == (dt2semester  . startTime $ p)


