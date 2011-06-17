Copyright (C) 2011 Associated Universities, Inc. Washington DC, USA.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

Correspondence concerning GBT software should be addressed as follows:
      GBT Operations
      National Radio Astronomy Observatory
      P. O. Box 2
      Green Bank, WV 24944-0002 USA

> module Antioch.TimeAccounting where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities

The names of the core functions for time accounting consist of three fields:
    [ p | s ]
    [ Allotted | Committed | Used | Past | Avail | Remain | Future ]
    [ S | T ]
  where:
    p = project
    s = session
    -
    Allotted = apportioned time for the project or session
    Committed = sum of pDuration* of periods in Pending/Scheduled/Completed
    Used = sum of pDuration* of periods in Scheduled/Completed
    Past = sum of pDuration* of periods in Scheduled/Completed started
           prior to the scheduling time
    Avail = Allotted minus Committed
    Remain = Allotted minus Used
    Future = Allotted minus Past
    -
    S = Semester
    T = Total

*  - is nell period's duration if period state is Pending, else is time
     billed if state is Scheduled or Completed

Note when using semester allotted time, the minimum of the semester
and total time is used.

Time Accounting is conceptually simple: sessions and projects have been
allotted only so much time, and we don't want to schedule them over this
amount of time.

But the devil is in the details, e.g., Project Time vs. Session Time - a
session can't be scheduled anymore if it's project is out of time.  Thus
you have to check both session & project.

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

Note the divergence between this Time Accounting, and the Time Accounting
in Nell; in Nell, state is ignored: when a pending period is created, its
period_accounting.scheduled time is zero.  When that period is published,
its state is moved to scheduled, and the period_accounting.scheduled
time is set to the duration of the period.  In this way, time billed
for pending periods is zero, and when we calculate time accounting
for sessions and projects, we can simply sum up all the period time
accounting without taking state into account.  For example, deleted
periods *must* have their time accounting set such that time billed
is zero.  In contrast, in Antioch we need to take more things into
account.  For example, the various computations need to know about state,
schedule start time, planned duration vs. billed duration, and semester
boundaries; but not why some observing time is not billed.

> pComplete :: Project -> Bool
> pComplete p = (pClosed p) || ((pAvailT p) < quarter )
> -- NOTE: project completeness is NOT dependent on session completeness:
> --  where
> --    allSessClosed p = all (==True) $ map sClosed $ sessions p

How much time has this session used up in periods?  The requirements below
for session completion are drawn from Project Note 11.3.  A session is
considered closed within the DSS when any of:
   1. the total amount of time allotted for that session has been billed
   (less than 15 minutes remains),
   2. the session is marked as closed
   by a project investigator or the GBT telescope scheduler, or
   3. the project to which it belongs is closed.

Checked factors:
   - project close flag
   - session close flag
   - session time available

> sComplete :: Session -> Bool
> sComplete s = (sTerminated s) || ((sAvailT s) < quarter)

> sTerminated :: Session -> Bool
> sTerminated s = (sClosed s) || (pClosed . project $ s)

How much time has this session used up in periods?

> sCommittedT :: Session -> Minutes
> sCommittedT = sum . map pDuration . periods

> sCommittedS :: SemesterName -> Session -> Minutes
> sCommittedS sem s = sum $ map pDuration $ periodsBySemester sem s

> sUsedT :: Session -> Minutes
> sUsedT = sum . map pDuration . filter isUsed . periods

> sUsedS :: SemesterName -> Session -> Minutes
> sUsedS sem s = sum $ map pDuration . filter isUsed . periodsBySemester sem $ s 

> isUsed :: Period -> Bool
> isUsed p = (pState p) `elem` [Scheduled, Complete]

> sPastT :: DateTime -> Session -> Minutes
> sPastT dt = sum . map pDuration . filter (isPast dt) . periods

> sPastS :: DateTime -> Session -> Minutes
> sPastS dt s = sum $ map pDuration . filter (isPast dt) . periodsBySemester (dt2semester dt) $ s

> isPast :: DateTime -> Period -> Bool
> isPast dt p = isUsed p && (endTime p) < dt
>   where
>     endTime pd = (duration pd) `addMinutes` (startTime pd)

How much time has this project used up in periods?

> pCommittedT :: Project -> Minutes
> pCommittedT p = sum . map sCommittedT . sessions $ p

> pCommittedS :: SemesterName -> Project -> Minutes
> pCommittedS sem p= sum . map (sCommittedS sem) . sessions $ p

> pUsedT :: Project -> Minutes
> pUsedT p = sum . map sUsedT . sessions $ p

> pUsedS :: SemesterName -> Project -> Minutes
> pUsedS sem p = sum . map (sUsedS sem) . sessions $ p

> pPastT :: DateTime -> Project -> Minutes
> pPastT dt p = sum . map (sPastT dt) . sessions $ p

> pPastS :: DateTime -> Project -> Minutes
> pPastS dt p = sum . map (sPastS dt) . sessions $ p

Returns the minutes available for scheduling for this session,
i.e., time that is not encumbered in any way and therefore
completely open for scheduling, tentative or not.

Checked factors:
   - session time available

> sAvailT :: Session -> Minutes
> sAvailT s = (sAllottedT s) - (sCommittedT s)

> sRemainT :: Session -> Minutes
> sRemainT s = (sAllottedT s) - (sUsedT s)

> sFutureT :: DateTime -> Session -> Minutes
> sFutureT dt s = (sAllottedT s) - (sPastT dt s)

The time available to this session might actually be further restricted by 
the time available to it's project, which may depend on which its semester.

Checked factors:
   - project time available
   - session time available
   - project semester time available

> sAvailS :: SemesterName -> Session -> Minutes
> sAvailS sem s = min (sAllottedT s) (sAllottedS s) - (sCommittedS sem s)

> sRemainS :: SemesterName -> Session -> Minutes
> sRemainS sem s = min (sAllottedT s) (sAllottedS s) - (sUsedS sem s)

> sFutureS :: DateTime -> Session -> Minutes
> sFutureS dt s = min (sAllottedT s) (sAllottedS s) - (sPastS dt s)

Checked factors:
   - project time available

> pAvailT :: Project -> Minutes
> pAvailT p = (pAllottedT p) - (pCommittedT p)

> pRemainT :: Project -> Minutes
> pRemainT p = (pAllottedT p) - (pUsedT p)

> pFutureT :: DateTime -> Project -> Minutes
> pFutureT dt p = (pAllottedT p) - (pPastT dt p)

Usually, the time available for a project is simply it's total time minus
the time it has already used up.  But for large projects, it may be allowed
only a certain amount of time per semester.

Checked factors:
   - project time available
   - project semester time available

> pAvailS :: SemesterName -> Project -> Minutes
> pAvailS sem p = min (pAllottedT p) (pAllottedS p) - (pCommittedS sem p)

> pRemainS :: SemesterName -> Project -> Minutes
> pRemainS sem p = min (pAllottedT p) (pAllottedS p) - (pUsedS sem p)

> periodsBySemester :: SemesterName -> Session -> [Period]
> periodsBySemester sem s = filter (isSemester sem) $ periods s
>   where
>     isSemester sem p = sem == (dt2semester  . startTime $ p)


