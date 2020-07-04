!Calculate some common Holidays by Carl Barnes.
!Many are "Fun" versus Real like Valentine's Day, Leap Day Feb 29, St Patrick's Day, April 15th taxes, Halloween, Father/Mother's day

  PROGRAM
  INCLUDE 'KEYCODES.CLW'
  MAP  
HolidayList     PROCEDURE()   
IsHoliday       PROCEDURE(DATE pDate, *STRING OutName, BYTE pNoWeekends=0, <*STRING OutDOW>),BOOL   
DowName         PROCEDURE(LONG pDate),STRING
EasterDateCalc  PROCEDURE(LONG Year, <*LONG OutGoodFriday>, <*LONG OutAshWednesday>),LONG    
EasterDate1900  PROCEDURE(LONG Year),LONG  !Calculate Easter limited to 1900 to 2299 from Tim Down
EasterTest2     PROCEDURE()  !Test that EasterDateCalc()=EasterSunday1900() for 1900-2299
  END
  CODE
  EasterTest2()  
  HolidayList() 
  RETURN 
  
HolidayList     PROCEDURE()
HolYear         USHORT 
DyNdx           LONG            
HolidayQ    QUEUE,PRE(HolQ)
DOW               STRING(3)
DateOf            LONG
NameOf            STRING(128)
            END 
QX          USHORT
bClp        ANY 
ShowAll     BYTE  
NoWeekends  BYTE  
Window WINDOW('U. S. Holidays and Observances List'),AT(,,260,260),GRAY,SYSTEM,ICON(ICON:Thumbnail), |
            FONT('Segoe UI',10),RESIZE
        PROMPT('Year'),AT(2,4),USE(?PROMPT1)
        SPIN(@n4),AT(19,3,48),USE(HolYear),HVSCROLL
        CHECK('No Weekends'),AT(71,4),USE(NoWeekends),SKIP,TIP('Do not show Holidays on Weekends,' & |
                '<13,10>Only show days off Monday to Friday.')
        CHECK('Show All'),AT(135,4),USE(ShowAll),SKIP,TIP('Show All Dates in Year')
        BUTTON('Copy'),AT(180,2),USE(?CopyBtn),SKIP
        BUTTON('Easter'),AT(223,2,30),USE(?EasterBtn),SKIP,TIP('Easter for next 10<13,10>and last 50' & |
                ' years')
        LIST,AT(1,20),FULL,USE(?List:HolidayQ),VSCROLL,FROM(HolidayQ),FORMAT('21L(2)|FM~Day~C(0)@s3@' & |
                '42L(2)|FM~Date~C(0)@d02-@90L(2)|FM~Day Name  (*=Federal Holiday)~@s128@')
    END    
    CODE  
    SYSTEM{PROP:PropVScroll}=1
    OPEN(Window)
    HolYear = YEAR(TODAY()) 
    POST(EVENT:Accepted,?HolYear)
    ACCEPT
        IF EVENT()=EVENT:NewSelection AND FIELD()=?HolYear | 
           THEN POST(EVENT:Accepted,?HolYear).    !AND KEYCODE()=MouseLeft
        CASE ACCEPTED()
        OF ?HolYear 
        OROF ?NoWeekends 
        OROF ?ShowAll 
             DO LoadHolidaysRtn ; DISPLAY   
        OF ?CopyBtn
            bClp='Day<9>Date<9>Holiday'
            LOOP QX=1 to records(HolidayQ)       
                GET(HolidayQ,QX)
                bClp=bClp&'<13,10>' & |
                    HolQ:Dow &'<9>'& FORMAT(HolQ:DateOf,@d01) &'<9>'& CLIP(HolQ:NameOf)
            END 
            SetClipboard(bClp)  
        OF ?EasterBtn ; DO Easter50YearsRtn    
        END    
    END
    RETURN

LoadHolidaysRtn ROUTINE 
    FREE(HolidayQ)
    IF HolYear < 1800 THEN HolYear = YEAR(TODAY()).    
    LOOP DyNdx = DATE(1,1,HolYear) TO DATE(1,1,Holyear+1)-1
         HolQ:NameOf = '' 
         IF IsHoliday(DyNdx,HolQ:NameOf, NoWeekends, HolQ:DOW) THEN
            HolQ:DateOf = DyNdx 
            ADD(HolidayQ)  

         ELSIF ShowAll
            HolQ:DateOf = DyNdx 
            HolQ:NameOf = ''
            ADD(HolidayQ) 
            
         END 
    END 
    DISPLAY
    EXIT

Easter50YearsRtn ROUTINE    !Test of Easter Calc
    DATA
Y        USHORT
EDay     LONG
EDay2    LONG
GoodFri  LONG
AshWed   LONG 
    CODE
    FREE(HolidayQ)
    LOOP Y = HolYear+10 TO HolYear-50 BY -1
         EDay = EasterDateCalc(Y, GoodFri, AshWed)
         EDay2 = EasterDate1900(Y)
         HolQ:DateOf = EDay 
         HolQ:NameOf = 'Easter ' & format(EDay,@d03) &'  GoodFri='& format(GoodFri,@d03) &'  Ash='& format(AshWed,@d03) &' ClaDate=' & EDay & |
                       CHOOSE(EDay=EDay2,'',' ?{5}<<>' & EDay2 &'=EasterDate1900()')
         HolQ:DOW = DowName(EDay)
         ADD(HolidayQ) 
    END
    DISPLAY 
!==================================================== 
!Not valid before 1971 Uniform Monday Holiday Act                                
IsHoliday       PROCEDURE(DATE pDate, *STRING OutName, BYTE pNoWeekends=0, <*STRING OutDOW>)!,BOOL  
DateParts   GROUP,OVER(pDate),PRE() !DP) 
Day              BYTE
MoNum            BYTE
YrNum            USHORT
            END 
IsHol       BOOL
HolName     STRING(50)
Dow     LONG,AUTO
DowNum  LONG,AUTO
Holiday BYTE(0)
Dows    ITEMIZE(0)
Sunday          EQUATE
Monday          EQUATE
Tuesday         EQUATE
Wednesday       EQUATE
Thursday        EQUATE
Friday          EQUATE
Saturday        EQUATE
        END    
!Fhd     BOOL            !Federal Holiday    
!    ' New Year's Day            Jan 1
!    ' Martin Luther King, Jr. third Mon in Jan
!    ' Washington's Birthday third Mon in Feb  
!    ' Casimir Pulaski Day   first Mon in March (Illinois Schools)
!    ' Memorial Day          last Mon in May
!    ' Independence Day      July 4
!    ' Labor Day             first Mon in Sept
!    ' Columbus Day          second Mon in Oct
!    ' Veterans Day          Nov 11
!    ' Thanksgiving Day      fourth Thur in Nov
!    ' Christmas Day         Dec 25
EasterYear  LONG,STATIC !Year for which we have EasterDate
EasterDate  LONG,STATIC !save EasterDateCalc for repeat calls
  CODE
  IF EasterYear <>YrNum THEN 
     EasterYear = YrNum
     EasterDate = EasterDateCalc(EasterYear)
  END 
  !Day   = DAY(pDate) ; MoNum = MONTH(pDate) ; YrNum = YEAR(pDate) done by Over(pDATE)
  Dow    = pDate % 7
  DowNum = (Day-1) / 7 + 1   !1st,2nd,3rd DOW etc
  
  CASE MoNum  !MONTH(pDate)
  OF 1
        IF Day=01 THEN HolName='*New Year''s Day'.
        IF Day=02 AND Dow=Monday THEN HolName='*New Year''s Day (Observed)'.
        IF Day=20 AND (YrNum - 1) % 4 =0 THEN HolName='Inauguration Day'.    !20 jan - 
        IF Dow=Monday AND DowNum=3 THEN HolName='*Martin Luther King Day (3rd Monday)'. ! - Third Monday in January
  OF 2
        IF Dow=Monday AND DowNum=3 THEN HolName='*Washington''s Birthday (3rd Monday)'. ! - Third Monday in February
        !not the official Fed name IF Dow=Monday AND DowNum=3 THEN HolName='President''s Day (3rd Monday)'. ! - Third Monday in February
        IF Day=12 THEN HolName='Lincoln''s Birthday'.  !Illinois Holiday and CT IN CA MO NY
        IF Day=14 THEN HolName='Valentine''s Day'.
        IF Day=29 THEN HolName='Leap Day'.
  OF 3  
        IF Dow=Monday AND DowNum=1 THEN HolName='Casimir Pulaski Day (1st Monday)'.  !Chicago Schools
        IF Dow=Sunday AND DowNum=2 AND YrNum>2006 THEN HolName='Daylight Saving Begins (2nd Sunday)'.  
        IF Day=17 THEN HolName='St Patrick''s Day'.
  OF 4
        IF (Day=15 AND Dow >= Monday AND Dow <= Friday) |
        OR (Day=16 AND Dow = Monday) OR (Day=17 AND Dow = Monday) THEN HolName='Tax Form 1040 Due'.! or 4868 Extension'.
  OF 5
        IF Dow=Sunday AND DowNum=2 THEN HolName='Mother''s Day (2nd Sunday)'.  !should be 9 months after Father's day :)
  OF 6  
        IF Day=14 THEN HolName='Flag Day'.   !14 june - flag day
        IF Dow=Monday AND Day+7>31 THEN HolName='*Memorial Day (Last Monday)'.! - Last Monday in May
        IF Dow=Sunday AND DowNum=3 THEN HolName='Father''s Day (3rd Sunday)'.
  OF 7
        IF Day=4                  THEN HolName='*Independence Day (4th of July)'.
        IF (Day=3 AND Dow=Friday) |
        OR (Day=5 AND Dow=Monday) THEN HolName='*Independence Day (Observed)'.
  OF 9
        IF Dow=Monday AND DowNum=1 THEN HolName='*Labor Day (1st Monday)'.! - First Monday in September
  OF 10
        IF Dow=Monday AND DowNum=2 THEN HolName='*Columbus Day (2nd Monday)'. ! - Second Monday in October
        IF Day=31 THEN HolName='Halloween'.
  OF 11   
        !Election Day is the Tuesday following the first Monday in November in even years
        !It can fall on or between November 2 and November 8
        IF Dow=Tuesday AND DowNum<=2 AND YrNum%2=0 |
            AND ((DowNum=2 AND Day=8) OR (DowNum=1 AND Day<8)) THEN HolName='Election Day'.
        IF Dow=Sunday AND DowNum=1  AND YrNum>2006 THEN HolName='Daylight Saving Ends (1st Sunday)'.
        IF Day=11                  THEN HolName='*Veteran''s Day'. !  - November 11th
        IF (Day=10 AND Dow=Friday) |
        OR (Day=12 AND Dow=Monday) THEN HolName='*Veteran''s Day (Observed)'.
        IF Dow=Thursday AND DowNum=4 THEN HolName='Thanksgiving (4th Thurs)'. ! - Fourth Thursday in November
       !IF Dow=Friday   AND DowNum=4 THEN HolName='Friday after Thanksgiving'. ! - Fourth Friday in November
  OF 12
        IF Day=25 THEN HolName='*Christmas'.  ! - December 25th
        IF (Day=24 AND Dow=Friday) |
        OR (Day=26 AND Dow=Monday) THEN HolName='*Christmas (Observed)'.        
        IF Day=31 AND Dow=Friday THEN HolName='New Year''s Day (Observed)'.
  END
  IF ~HolName THEN 
       CASE pDate
       !OF EasterDate-47   ;  HolName = 'Mardi Gras / Fat Tuesday'
       OF EasterDate-46    ;  HolName = 'Ash Wednesday'
       OF EasterDate-2     ;  HolName = 'Good Friday'
       OF EasterDate       ;  HolName = 'Easter Sunday'
       !OF EasterDate + 49 ;  HolName = 'Pentecost' !the descent of the Holy Spirit on the disciples of Jesus after his Ascension, held on the seventh Sunday after Easter
       END        
  END 
  IF HolName[1]='*' AND (Dow=0 OR Dow=6) THEN   !*Federal holiday on Sun/Sat is not a Holday
     HolName=SUB(HolName,2,99)
  END

  OutName = HolName
  IsHol = CHOOSE(~HolName,0,1)
  IF ~OMITTED(OutDOW) THEN OutDOW = CHOOSE( Dow + 1,'Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','???').
  IF pNoWeekends AND Dow =0 OR Dow=6 THEN IsHol=0.    !Just want Weekdays Off
  RETURN IsHol
!===================================================================================================
DowName     PROCEDURE(LONG pDate)!,STRING
    CODE
    IF pDate > 7 THEN pDate = pDate % 7.
    RETURN CHOOSE( pDate + 1,'Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','???')

!===================================================================================================
! Easter
!---------------------------------------------------------------
! Calculates the date of Easter using an algorithm that was first published in Butcher's Ecclesiastical Calendar (1876).  
! It is valid for all years in the Gregorian calendar (1583+).  The code is based on an implementation by Peter 
! Duffett-Smith in Practical Astronomy with your Calculator (3rd Edition).  
! https://en.wikipedia.org/wiki/Computus#Anonymous_Gregorian_algorithm
!===============================================================
! Carl modified Modulus % that was Divide twice to instead subtract previous Divide result Multiplied * by Divisor as faster
! Because this returns a Clarion Date it can only go back to 1801. If you need more return string M/DD/YYYY.

EasterDateCalc  PROCEDURE(LONG Year, <*LONG OutGoodFri>, <*LONG OutAshWed>)!,LONG
a   LONG,AUTO
b   LONG,AUTO
c   LONG,AUTO
d   LONG,AUTO
e   LONG,AUTO
f   LONG,AUTO
g   LONG,AUTO
h   LONG,AUTO
i   LONG,AUTO
k   LONG,AUTO
l   LONG,AUTO
m   LONG,AUTO
n   LONG,AUTO
p   LONG,AUTO
Easter LONG,AUTO
    CODE                         !   Note: All divides have an integer result variable so have an implicit INT()
  a = Year % 19                  ! Step 1: Divide the year by 19 and store the remainder in variable A. E.g.: 2000%19 A=5
  b = Year / 100                 ! Step 2: Divide the year by 100.  Store the integer result in B
  c = Year - b * 100             !                                         and the remainder in C
  d = b / 4                      ! Step 3: Divide (b/4). Store integer result in D
  e = b - d * 4                  !                              and remainder in E
  f = (b + 8) / 25               ! Step 4: Divide (b+8)/25 and store integer result in F
  g = (b - f + 1) / 3            ! Step 5: Divide (b-f+1)/3 and store the integer result in G
  h = (19*a +b-d-g+15) % 30      ! Step 6: Divide (19a+b-d-g+15)/30 and store remainder in H
  i = c / 4                      ! Step 7: Divide C by 4. Store the integer result in I
  k = c - i * 4                  !                               and the remainder in K
  l = (32 + 2*e + 2*i -h-k) % 7  ! Step 8: Divide (32+2e+2i-h-k) by 7.  Store the remainder in L
  m = (a + 11*h + 22*l) / 451    ! Step 9: Divide (a + 11h + 22l) by 451 and store the integer result in M
  p = (h + l - 7*m + 114)        !Step 10: Calc   (h + l - 7m + 114) for next step Divide
  n = p / 31                     !Step 10: Divide by 31.  Store the integer result in N     is Easter month
  p = p - n * 31 + 1             !                           and the remainder     in P + 1 is Easter Day
  Easter=Date(n, p, Year)        ! Done: p is the day on which Easter falls. n is month 3 for March or 4 for April.
  IF ~OMITTED(OutGoodFri) THEN OutGoodFri = Easter-2.  !Good Friday 2 days before Sunday
  IF ~OMITTED(OutAshWed)  THEN OutAshWed  = Easter-46. !Ash Wednesday 46 days before Easter
  ! Canjun FYI ... Easter - 47  =>  Mardi Gras / Fat Tuesday  - 1 day before Ash Wednesday
  Return Easter
!===================================================================================================
EasterDate1900  PROCEDURE(LONG Year)!,LONG  !Calculate Easter from 1900 to 2299
! Valid for any 4-digit year from 1900 to 2299   Finds date of GREGORIAN WESTERN EASTER SUNDAY
! Returns a Clarion standard date (LONG)         Returns 0 for out-of-range years.

!Posted on comp.lang.clarion (comp.lang.clarion) - Subject: Function for finding Easter Day From: Tim Down <tim@ftipsDOTcom>  
!I've been tinkering with a way to highlight all the public holidays on a calendar here in the UK. The majority of these are obviously predictable (like Christmas/New Year, May, August etc) but the Easter holidays, being moveable, were proving more troublesome.
!So, I have come up with the following function if anyone finds it useful. (It uses a method for calculating the Gregorian Western Easter Sunday devised by Prof. R Sivaraman).
!Enjoy, Tim Down

!Carl checked this against above EasterDateCalc() for 1894 to 2301 and it matched exactly.
!It is much less code and calculations, so a better choice unless you need < 1900 or > 2300

A  LONG,AUTO
B  LONG,AUTO
D  LONG,AUTO          
M  LONG,AUTO
FullMoon LONG,AUTO
Easter   LONG,AUTO
  CODE
  IF Year < 1894 OR Year > 2301 THEN RETURN(0).
  A = Year % 19
  IF Year <= 2199 THEN
     B = (11*A + 5) % 30
  ELSE
     B = (11*A + 4) % 30
  END
  IF B = 0 OR (B = 1 AND A > 10) THEN
     B += 1
  END
  D = (50 - B) % 31
  IF B <= 19 THEN 
     M = 4          !OF  1 TO 19
  ELSE
     M = 3          !OF 20 TO 29
  END
  FullMoon = DATE(M,D,Year)
  Easter = FullMoon + 7 - FullMoon % 7
  RETURN(Easter)      
!===================================================================================================
EasterTest2 PROCEDURE()  !Test that EasterDateCalc()=EasterSunday1900() for 1900-2299
Yr LONG 
E1 LONG 
E2 LONG 
  CODE
  LOOP Yr=1894 TO 2301
    E1=EasterDate1900(Yr) 
    E2=EasterDateCalc(Yr)
    IF E1<>E2 THEN STOP('Easter Check failed Year: ' & Yr &'  E1=' & E1 &'  E2=' & E2 ).
  END 
  !Message('EasterTest2 done at ' & Yr)      


!===================================================================================================
    OMIT('** Original Code **')
! On Tue, Jun 23 2020 5:42 am, Tim Down <tim@ftipsDOTcom> said:

EasterSunday PROCEDURE(LONG PassedYear),LONG
! Valid for any 4-digit year from 1900 to 2299
! Finds date of GREGORIAN WESTERN EASTER SUNDAY
! Returns a Clarion standard date (LONG)
! Returns 0 for out-of-range years.
Inter_A      LONG(0)
Inter_B      LONG(0)
Inter_C      LONG(0)
Inter_D      LONG(0)
FullMoonDate LONG(0)
EasterMonth  LONG(0)
EasterDate   LONG(0)
  CODE
   IF (PassedYear < 1900) OR (PassedYear > 2299)
      RETURN(0)
   END
   Inter_A = PassedYear % 19
   Inter_B = ((11*Inter_A)+5) % 30
   IF (PassedYear > 2199)
      Inter_B = ((11*Inter_A)+4) % 30
   END
   IF (Inter_B = 0) OR (Inter_B = 1 AND Inter_A > 10)
      Inter_C = Inter_B + 1
   ELSE
      Inter_C = Inter_B
   END
   CASE Inter_C
     OF 1 TO 19
        EasterMonth = 4
     OF 20 TO 29
        EasterMonth = 3
   END
   Inter_D = (50 - Inter_C) % 31
   FullMoonDate = DATE(EasterMonth,Inter_D,PassedYear)
   EasterDate = FullMoonDate + (7 - (FullMoonDate % 7))
   RETURN(EasterDate)

    !end of OMIT('** Original Code **')  