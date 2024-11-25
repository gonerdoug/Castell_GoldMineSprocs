SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_WriteSchedule]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_WriteSchedule] AS' 
END
GO









ALTER PROCEDURE [dbo].[Castell_WriteSchedule](@gmnv varchar(20))

AS

DECLARE @currdate datetime,
		@currtime varchar(5),
		@notifyrecid varchar(15),
		@notifyref varchar(255),
		@tempgmnv varchar(20),
		@user varchar(8),
		@fieldname varchar(50),
		@value varchar(max),
		@datatype varchar(20),
		@accountno varchar(20),
		@ref varchar(65),
		@actvcode varchar(3),
		@recid varchar(15),
		@rectype varchar(1),
		@loprecid varchar(15),
		@ondate datetime,
		@ontime varchar(5),
		@alarmflag varchar(1),
		@alarmdate datetime,
		@alarmtime varchar(5),
		@userid varchar(8),
		@company varchar(60),
		@notify varchar(1),
		@contactdb varchar(123),
		@caldb sysname,
		@setcmd1 varchar(max),
		@namefield varchar(20),
		@valuefield varchar(max),
		@calondate   datetime,
		@calontime   varchar(5),
		@calrectype  varchar(1),
		@calref      varchar(80),
		@calactvcode varchar(3),
		@caluserid   varchar(8),
		@calcompany  varchar(60),
		@valid int,
		@notesptr binary(16)

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
            RETURN -120

/*retrieve required data from name/value structure*/
EXEC Castell_CreateCopy @gmnv, @tempgmnv OUTPUT

EXEC Castell_GetValue @tempgmnv, 'contactdb', @contactdb OUTPUT
EXEC Castell_Erasename @tempgmnv, 'contactdb'

IF @contactdb IS NULL
    BEGIN
       IF EXISTS(SELECT object_id('CONTACT2'))
         SELECT @contactdb = ''
       ELSE
         RETURN -190
       END
ELSE SELECT @contactdb = @contactdb + '..'

SELECT @caldb = db_name() + '..'

EXEC Castell_GetValue @tempgmnv, 'recid', @recid OUTPUT
EXEC Castell_EraseName @tempgmnv, 'recid'
IF @recid = ''
    SELECT @recid = NULL

EXEC Castell_GetValue @tempgmnv, 'notify', @notify OUTPUT
EXEC Castell_EraseName @tempgmnv, 'notify'

/*massage 0 or 1 to be Y or N*/
SELECT @notify = CASE WHEN @notify = '1' THEN 'Y'
                      WHEN @notify = '0' THEN 'N'
                      WHEN @notify = NULL THEN NULL
                      ELSE 'N'
		 END

EXEC Castell_GetValue @tempgmnv, 'rsvp', @value OUTPUT
/*massage 0 or 1 to be Y or N*/
IF @value IS NOT NULL
    BEGIN
        SELECT @value = CASE @value WHEN '1' THEN 'Y'
                                    WHEN '0' THEN 'N'
                                    ELSE 'N'
                        END
        EXEC Castell_SetValue @tempgmnv, 'rsvp', @value
    END
SELECT @value = NULL

EXEC Castell_GetValue @tempgmnv, 'user', @user OUTPUT
EXEC Castell_EraseName @tempgmnv, 'user'

IF @user IS NULL
    RETURN -130

EXEC Castell_GetValue @tempgmnv, 'accountno', @accountno OUTPUT
EXEC Castell_GetValue @tempgmnv, 'rectype', @rectype OUTPUT

/*change the name of the sales NV's so they go in the right column*/
IF @rectype = 'S'
    BEGIN
        EXEC Castell_ChangeName @tempgmnv, 'potnsale', 'number1'
        EXEC Castell_ChangeName @tempgmnv, 'probsale', 'duration'
        EXEC Castell_ChangeName @tempgmnv, 'unitssale', 'number2'
    END

EXEC Castell_GetValue @tempgmnv, 'loprecid', @loprecid OUTPUT
EXEC Castell_GetValue @tempgmnv, 'ondate', @ondate OUTPUT
/*clean time from supplied ondate full datetime value*/
if @ondate IS NOT NULL and isdate(@ondate)=1
	SELECT @ondate = CONVERT(datetime, CONVERT(char(8), @ondate, 112))
	
EXEC Castell_GetValue @tempgmnv, 'ontime', @ontime OUTPUT
EXEC Castell_GetValue @tempgmnv, 'alarm', @alarmflag OUTPUT

/*change alarm to alarmflag and make 1's into Y's etc*/
IF @alarmflag IS NOT NULL OR (@alarmflag IS NULL AND @recid IS NULL)
BEGIN
    SELECT @alarmflag = CASE @alarmflag WHEN '1' THEN 'Y'
                                        WHEN '0' THEN 'N'
                                        ELSE 'N'
                        END
    EXEC Castell_EraseName @tempgmnv, 'alarm'
    EXEC Castell_SetValue @tempgmnv, 'alarmflag', @alarmflag
END

EXEC Castell_GetValue @tempgmnv, 'alarmdate', @alarmdate OUTPUT
EXEC Castell_GetValue @tempgmnv, 'alarmtime', @alarmtime OUTPUT
EXEC Castell_GetValue @tempgmnv, 'ref', @ref OUTPUT

EXEC Castell_GetValue @tempgmnv, 'userid', @userid OUTPUT
SELECT @userid = UPPER(@userid)
    IF @userid IS NULL
        SELECT @userid = @user
EXEC Castell_SetValue @tempgmnv, 'userid', @userid

EXEC Castell_GetValue @tempgmnv, 'contact', @company OUTPUT
EXEC Castell_ChangeName @tempgmnv, 'contact', 'company'

/*Get the current date*/
SELECT @currdate = getdate()

/*get the current date*/
SELECT @currtime = RIGHT('0' + CONVERT(varchar, DATEPART(hour,@currdate)),2) + ':' + RIGHT('0' + CONVERT(varchar, DATEPART(minute, @currdate)),2)

/*make the current date 12:00am*/
SELECT @currdate = CONVERT(datetime, CONVERT(char(8), @currdate, 112))

/*check for enddate, make ondate if not provided*/
EXEC Castell_GetValue @tempgmnv, 'enddate', @value OUTPUT
IF @recid IS NULL AND @value IS NULL AND @ondate IS NULL
    EXEC Castell_SetValue @tempgmnv, 'enddate', @currdate
ELSE IF @recid IS NULL AND @value IS NULL AND @ondate IS NOT NULL
    EXEC Castell_SetValue @tempgmnv, 'enddate', @ondate

SELECT @value = NULL

IF @recid IS NULL  /*adding a new activity*/
    BEGIN
        /*Get a Recid */
        set @recid = (select dbo.Castell_RecId('MASTER'))

        /*validate rectype*/
        IF UPPER(@rectype) NOT IN ('A','C','E','M','Q','S','T','D','O')
	      RETURN -150
		
		/* loprecid */
        IF @loprecid IS NULL 
	       BEGIN
              set @loprecid = (select dbo.Castell_RecId('MASTER'))
		      SELECT @loprecid = STUFF(@loprecid, 1, 1, ' ') 
              EXEC Castell_SetValue @tempgmnv, 'loprecid', @loprecid
	       END

        /*if ondate not supplied then make it the current date*/
        IF @ondate IS NULL AND @rectype <> 'D'
            BEGIN
                SELECT @ondate = @currdate
                EXEC Castell_SetValue @tempgmnv, 'ondate', @ondate
            END

        /*alarm date handling*/
        IF UPPER(@alarmflag) = 'Y' 
            BEGIN
                IF @alarmtime IS NULL 
                    BEGIN
                        /*put ondate & ontime in one datetime variable to subtract lead time from*/
                        SELECT @alarmdate = CONVERT(datetime, CONVERT(char(8), COALESCE(@alarmdate, @ondate), 112) + ' ' + @ontime)

                        SELECT @alarmdate = DATEADD(minute, -10, @alarmdate)

                        /*pull the leadtime out of the leadalarmdate*/
                        SELECT @alarmtime = RIGHT('0' + CONVERT(varchar, DATEPART(hour,@alarmdate)),2) + ':' + RIGHT('0' + CONVERT(varchar, DATEPART(minute, @alarmdate)),2)
        
                        /*make leadalarm date back to 12:00 am*/
                        SELECT @alarmdate = CONVERT(datetime, CONVERT(char(8), @alarmdate,112))
                    END
                ELSE
                    IF @alarmdate IS NULL 
                        SELECT @alarmdate = @ondate

                EXEC Castell_SetValue @tempgmnv, 'alarmdate', @alarmdate
                EXEC Castell_SetValue @tempgmnv, 'alarmtime', @alarmtime
            END
        ELSE /*alarmflag is 0, so no alarmdate and time*/
            BEGIN
                EXEC Castell_EraseName @tempgmnv, 'alarmdate'
                EXEC Castell_EraseName @tempgmnv, 'alarmtime'
            END

        
        /*Build the Insert Statement*/
        INSERT cal (userid, status, accountno, ontime, alarmflag, alarmtime, rectype, loprecid, rsvp, createby, createon, createat, ORIG_TIME, recid, LINKRECID) 
        VALUES(UPPER(@userid), ' 0', ' ', ' ', ' ',' ',' ', ' ', 'N', UPPER(@user), @currdate, @currtime, @CURRTIME, @recid, '')

--        /*Update the Sync Log*/
        SELECT @user = UPPER(@user)
        EXEC Castell_UpdateSyncLog 'CAL', @recid, @user, 'N'
           
        /*write the notify ref if a new cal activity*/
        IF UPPER(@notify) = 'Y' AND UPPER(@rectype) IN ('C','T','A','O','E')
            SELECT @notifyref = CASE UPPER(@rectype)
		                          WHEN 'C' THEN 'Call notification: '
			                      WHEN 'T' THEN 'Next Actn notification: '
			                      WHEN 'A' THEN 'Appt notification: '
			                      WHEN 'O' THEN 'Other notification: '
			                      WHEN 'E' THEN 'Event notification: '
			                    END
            
    END

ELSE    /*modifying an existing activity*/
    BEGIN
            /*Get the fields we need from the cal record to be modified*/
        SELECT @calondate = ondate, @calontime = ontime, @calrectype = rectype, @calref = ref, 
	           @accountno = accountno,
               @calactvcode = actvcode,
               @calcompany = company,
               @caluserid = userid
               FROM cal WHERE recid = @recid

        /*does the user want to change the alarmdate or time?  check the alarmflag in cal if not passed*/
        IF @alarmflag IS NULL AND (@alarmdate IS NOT NULL OR @alarmtime IS NOT NULL)
            SELECT @alarmflag = alarmflag FROM cal WHERE recid = @recid
      
        IF UPPER(@alarmflag) = 'N'
            BEGIN
                 EXEC Castell_SetValue @tempgmnv, 'alarmflag', 'N'
                 EXEC Castell_SetValue @tempgmnv, 'alarmdate', NULL
                 EXEC Castell_SetValue @tempgmnv, 'alarmtime', ''
            END
		         				
		IF UPPER(@alarmflag) = 'Y'
	       BEGIN
	         /*IF @alarmdate is not null AND @alarmtime is not null
		      Do nothing.  The values for the alarmdate and time have already 
		      been specified. */
		
	        /*No alarm time specified, must use lead time*/
		     IF @alarmtime IS NULL 
		        BEGIN
			       IF @alarmdate IS NULL AND @ondate IS NULL /*no new ondate, no alarm date*/
				          SELECT @alarmdate = @calondate 
			       ELSE IF @alarmdate IS NULL AND @ondate IS NOT NULL /*no alarm date & new ondate*/
				          SELECT @alarmdate = @ondate
						
			       /*are we modifying the ontime?*/
			       IF @ontime IS NULL
				      SELECT @ontime = @calontime 
					
			       /*concat the dates and the time*/
			       SELECT @alarmdate = CONVERT(datetime, CONVERT(char(8), @alarmdate, 112) + ' ' + @ontime)

			       /*subtract 10 minutes and get the alarmtime*/
			       SELECT @alarmdate = DATEADD(minute, -10, @alarmdate)
			       SELECT @alarmtime = RIGHT('0' + CONVERT(varchar, DATEPART(hour,@alarmdate)),2) + ':' + RIGHT('0' + CONVERT(varchar, DATEPART(minute, @alarmdate)),2)
    
                    /*make alarm date back to 12:00 am*/
				    SELECT @alarmdate = CONVERT(datetime, CONVERT(char(8), @currdate,112))

                    EXEC Castell_SetValue @tempgmnv, 'alarmtime', @alarmtime
                END
			
			/*Alarm time is specified but alarmdate is not*/
    	     IF @alarmdate IS NULL AND @alarmtime IS NOT NULL
			     BEGIN
				    /*are we modifying the ondate*/
			        IF @ondate IS NULL
					  SELECT @ondate = @calondate
					
				    SELECT @alarmdate = @ondate
	             END

           EXEC Castell_SetValue @tempgmnv, 'alarmdate', @alarmdate
			 			 
       END
    /*prepare variables if notify is selected, DO NOT WRITE BACK TO TEMPGMNV*/	
    IF @rectype IS NULL
	   SELECT @rectype = @calrectype 

    IF UPPER(@notify) = 'Y' AND UPPER(@rectype) IN ('C','T','A','O','E')
        BEGIN
            SELECT @notifyref = CASE UPPER(@rectype)
					              WHEN 'C' THEN 'Call changed: '
					              WHEN 'T' THEN 'Next Actn changed: '
					              WHEN 'A' THEN 'Appt changed: '
					              WHEN 'O' THEN 'Other changed: '
					              WHEN 'E' THEN 'Event changed: '
				                 END 
            IF @ondate IS NULL
                SELECT @ondate = @calondate
            IF @ontime IS NULL
                SELECT @ontime = @calontime

            EXEC Castell_GetValue @tempgmnv, 'ref', @ref OUTPUT
            IF @ref IS NULL
                SELECT @ref = @calref
            IF @actvcode IS NULL
                SELECT @actvcode = @calactvcode
            IF @userid IS NULL
                SELECT @userid = @caluserid
            IF @company IS NULL
                SELECT @company = @calcompany
            IF @loprecid IS NULL
	           BEGIN
                    set @loprecid = (select dbo.Castell_RecId('MASTER'))
					SELECT @loprecid = STUFF(@loprecid, 1, 1, ' ')  
	           END
        END
    END 

/*add notes*/
EXEC Castell_GetValue @tempgmnv, 'notes', @value OUTPUT
IF @value IS NOT NULL
    BEGIN
        UPDATE cal
        SET status = substring(status, 1,1) + '1', notes=@value
        WHERE recid = @recid
        EXEC Castell_UpdateSyncLog 'cal', @recid, @user, 'U', 'notes'
        EXEC Castell_UpdateSyncLog 'cal', @recid, @user, 'U', 'status'
        EXEC Castell_EraseName @tempgmnv, 'notes'
    END
SELECT @value = NULL

/*update the fields from tempgmnv*/
SELECT @namefield 'name', @valuefield 'value' INTO #tempgmnv
DELETE FROM #tempgmnv

INSERT INTO #tempgmnv
EXEC ('SELECT fieldname, value FROM ##gmnv' + @tempgmnv)

EXEC Castell_Delete @tempgmnv

DECLARE fieldcursor CURSOR 
FOR SELECT name, value FROM #tempgmnv

OPEN fieldcursor

FETCH NEXT FROM fieldcursor INTO @fieldname, @value

WHILE @@fetch_status = 0
	BEGIN
		SELECT @datatype=NULL
		SELECT @datatype=DATA_TYPE from INFORMATION_SCHEMA.COLUMNS where COLUMN_NAME = @fieldname and TABLE_NAME = 'CAL'

		IF UPPER(@fieldname) = 'RECTYPE'
		SELECT @value = UPPER(@value)

		IF upper(@fieldname) <> 'RECID' and @datatype is not NULL
		BEGIN
			SELECT @setcmd1 = ' [' + @fieldname + '] = ' 
			+
			CASE  --deal with data types that require quotes versus those that don't.  Also deal with NULL
				WHEN @value IS NULL THEN 'NULL' 
				WHEN UPPER(@datatype) NOT IN ('FLOAT', 'INT', 'REAL','NUMERIC','SMALLINT','TINYINT','DECIMAL')
					THEN '"' + REPLACE(@value,'"','""') + '"'
				WHEN @value = '' THEN '""'
				ELSE @value 
			END 
			+
			' WHERE recid = "'+REPLACE(@recid,'"','""')+'"'

			EXEC('UPDATE CAL SET ' + @setcmd1)

			EXEC Castell_UpdateSyncLog 'CAL', @recid, @user, 'U', @fieldname
		END --most fields

		FETCH NEXT FROM fieldcursor INTO @fieldname, @value
	END
CLOSE fieldcursor
DEALLOCATE fieldcursor

------------------------------NEW FIELD CODE END-------------------------------------


UPDATE cal SET lastuser = UPPER(@user), lastdate = @currdate, lasttime = @currtime WHERE recid = @recid

EXEC Castell_UpdateSyncLog 'cal', @recid, @user, 'U', 'lastuser'
EXEC Castell_UpdateSyncLog 'cal', @recid, @user, 'U', 'lastdate'
EXEC Castell_UpdateSyncLog 'cal', @recid, @user, 'U', 'lasttime'

EXEC Castell_SetValue @gmnv, 'recid', @recid   

/* Handle Notification */

IF UPPER(@notify) = 'Y' AND UPPER(@rectype) IN ('C','T','A','O','E')
       BEGIN
           set @notifyrecid = (select dbo.Castell_RecId('MASTER'))
            
           INSERT CAL (USERID,ACCOUNTNO,ONDATE,ONTIME,ENDDATE,ALARMFLAG,ALARMTIME,ALARMDATE,ACTVCODE,RSVP,DURATION,RECTYPE,ACONFIRM,APPTUSER,STATUS,DIRCODE,NUMBER1,NUMBER2,COMPANY,REF,NOTES,LINKRECID,LDOCRECID,LOPRECID,CREATEBY,CREATEON,CREATEAT,LASTUSER,LASTDATE,LASTTIME,RECID,CALDEF_ID,CALDEFEX_ID,GLOBALID,ORIG_TIME)
			VALUES(UPPER(@userid), 
                    @accountno,
                    @currdate, 
					@currtime, 
					@currdate,
                    CASE @ondate WHEN @currdate THEN 'Y' ELSE 'N' END, --alarm flag
                    CASE @ondate WHEN @currdate THEN @currtime ELSE '' END, -- alarmtime,
                    CASE @ondate WHEN @currdate THEN @currdate ELSE NULL END, --alarm date
                    @actvcode,
                    'N', --rsvp
					NULL, --duration 
					'M', --rectype
	                NULL, --aconfirm
					NULL, --apptuser
                    ' 0', --STATUS
					NULL, --dircode
					NULL, --number1 
					NULL, --number2 
                    @company, 
	                @notifyref + --REF
	                --process time to 12 hr format
	                CASE 
                         WHEN @ontime IS NULL THEN ''
                         WHEN @ontime = '' THEN @ontime
		                 WHEN CONVERT(int, SUBSTRING(@ontime,1,(PATINDEX('%:%',@ontime) - 1))) > 12 
				               THEN CONVERT(varchar,(CONVERT(int, SUBSTRING(@ontime,1,(PATINDEX('%:%',@ontime) - 1))) - 12)) + 
					                 SUBSTRING(@ontime,PATINDEX('%:%',@ontime),3) + 'pm '
                         WHEN CONVERT(int, SUBSTRING(@ontime,1,(PATINDEX('%:%',@ontime) - 1))) = 12
						    THEN @ontime + 'pm '
		                 ELSE @ontime + 'am '
	                END  +
	                substring(datename(dw,@ondate),1,3) + ', ' +
	                substring(datename(month,@ondate),1,3) + ' ' +
	                datename(day,@ondate) + ': ' +
	                COALESCE(@ref, ''),
	                --ref complete
	                NULL, --notes
					'', --linkrecid
					NULL, --ldorecid
					@loprecid, 
	                UPPER(@user), 
					@currdate, 
					@currtime,
					UPPER(@user), --lastuser
	                @currdate, --lastdate
					@currtime, --lasttime
					@notifyrecid,
					'', --CALDEF_ID
					'', --CALDEFEX_ID
					'', --GLOBALID
					@CURRTIME ) --ORIG_TIME
		
          SELECT @user = UPPER(@user)
          EXEC Castell_UpdateSyncLog 'CAL', @notifyrecid, @user, 'N'
    END


IF @accountno IS NOT NULL
    EXEC('EXEC ' + @contactdb + 'Castell_UpdateSummary "' + @accountno + '", "CAL", "' + @user + '", "' + @caldb + '"')





GO
