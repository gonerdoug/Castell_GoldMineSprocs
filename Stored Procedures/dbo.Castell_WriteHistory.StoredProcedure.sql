SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_WriteHistory]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_WriteHistory] AS' 
END
GO







ALTER PROCEDURE [dbo].[Castell_WriteHistory](@gmnv varchar(20))

/*If no rectype is specified, then an unknown action will be completed*/

AS
/*NEED TO DEAL WITH SALES NUMBERS AND SCHEDULILNG RSVP CHANGE*/

SET NOCOUNT ON

/*Declarations*/
DECLARE @currdate datetime,
		@currtime varchar(5),
		@user varchar(8),
		@rectype varchar(10),
		@calrecid varchar(15),
		@recid varchar(15),
		@userid varchar(8),
		@accountno varchar(20),
		@contact varchar(60),
		@ondate datetime,
		@ontime varchar(5),
		@actvcode varchar(3),
		@status varchar(2),
		@duration varchar(8),
		@units varchar(8),
		@ref varchar(80),
		@linkrecid varchar(15),
		@loprecid varchar(15),
		@successprivate varchar(2),
		@rsvp varchar(1),
		@calcreateby varchar(8),
		@setcmd varchar(1000),
		@fieldname varchar(50),
		@value varchar(max),
		@datatype varchar(20),
		@tempgmnv varchar(15),
		@rsvpgmnv varchar(15),
		@namefield varchar(20),
		@valuefield varchar(max),
		@valid int,
		@notesptr binary(16),
		@caldb varchar(123),
		@contactdb sysname,
		@tempsql varchar(1000),
		@createon datetime,
		@createat varchar(5)

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
            RETURN -120

/*Get the current date*/
SELECT @currdate = getdate()

/*get the current date*/
SELECT @currtime = RIGHT('0' + CONVERT(varchar, DATEPART(hour,@currdate)),2) + ':' + RIGHT('0' + CONVERT(varchar, DATEPART(minute, @currdate)),2)

/*make the current date 12:00am*/
SELECT @currdate = CONVERT(datetime, CONVERT(char(8), @currdate, 112))

/*retrieve required data from name/value structure*/
EXEC Castell_CreateCopy @gmnv, @tempgmnv OUTPUT

EXEC Castell_GetValue @tempgmnv, 'recid', @recid OUTPUT
EXEC Castell_EraseName @tempgmnv, 'recid'
IF @recid = ''
    SELECT @recid = NULL

EXEC Castell_GetValue @tempgmnv, 'user', @user OUTPUT
EXEC Castell_EraseName @tempgmnv, 'user'

IF @user IS NULL
    RETURN -130

/*get the current database name*/
SELECT @contactdb = db_name() 

/*change the sales numbers to the fields in history*/
EXEC Castell_GetValue @tempgmnv, 'rectype', @rectype OUTPUT
IF UPPER(@rectype) = 'S'
    BEGIN
        EXEC Castell_ChangeName @tempgmnv, 'totsale', 'duration'
        EXEC Castell_ChangeName @tempgmnv, 'unitssold', 'units'
    END

/*pull needed data from gmnv*/
EXEC Castell_GetValue @tempgmnv, 'userid', @userid OUTPUT
SELECT @userid = UPPER(@userid)

IF @userid IS NULL AND @recid IS NULL
    SELECT @userid = UPPER(@user)

IF @userid IS NOT NULL
    EXEC Castell_SetValue @tempgmnv, 'userid', @userid

EXEC Castell_GetValue @tempgmnv, 'accountno', @accountno OUTPUT
EXEC Castell_GetValue @tempgmnv, 'contact', @contact OUTPUT
EXEC Castell_EraseName @tempgmnv, 'contact'
if isnull(@contact,'')='' and isnull(@accountno,'') <>''
	begin
		set @contact=(select ISNULL(CONTACT,'') from contact1 where ACCOUNTNO=@accountno)
	end

EXEC Castell_GetValue @tempgmnv, 'ondate', @ondate OUTPUT
EXEC Castell_GetValue @tempgmnv, 'ontime', @ontime OUTPUT
EXEC Castell_GetVAlue @tempgmnv, 'ref', @ref OUTPUT

EXEC Castell_GetValue @tempgmnv, 'calrecid', @calrecid OUTPUT
EXEC Castell_EraseName @tempgmnv, 'calrecid'

/*mark flags if successful and/or private*/
EXEC Castell_GetValue @tempgmnv, 'success', @value OUTPUT
IF @value = '1' OR @value IS NULL 
    SELECT @successprivate = ' '
ELSE 
    SELECT @successprivate = 'U'
EXEC Castell_EraseName @tempgmnv, 'success'

SELECT @value = NULL
EXEC Castell_GetValue @tempgmnv, 'private', @value OUTPUT
IF @value = '1'
    SELECT @successprivate = @successprivate + 'P'
EXEC Castell_EraseName @tempgmnv, 'private'
SELECT @value = NULL

/*first handle for completing an activity by pulling necessary data from cal*/
IF @calrecid IS NOT NULL AND @recid IS NULL
    BEGIN
                
        EXEC Castell_GetValue @tempgmnv, 'caldb', @caldb OUTPUT
        EXEC Castell_EraseName @tempgmnv, 'caldb'
        IF @caldb IS NULL
            BEGIN
                IF EXISTS(SELECT object_id('CAL'))
                    SELECT @caldb = ''
                ELSE
                    RETURN -190
            END
        ELSE SELECT @caldb = @caldb + '..'

        EXEC Castell_GetValue @tempgmnv, 'ontime', @ontime OUTPUT
        EXEC Castell_GetValue @tempgmnv, 'actvcode', @actvcode OUTPUT
        EXEC Castell_GetValue @tempgmnv, 'status', @status OUTPUT
        EXEC Castell_GetValue @tempgmnv, 'duration', @duration OUTPUT
        EXEC Castell_GetValue @tempgmnv, 'units', @units OUTPUT
        EXEC Castell_GetValue @tempgmnv, 'linkrecid', @linkrecid OUTPUT

        /*select fields from cal where appropriate*/
        SELECT @userid 'userid', @accountno 'accountno', @contact 'company',
               @rectype 'rectype', @ondate 'ondate', @ontime 'ontime', @actvcode 'actvcode',
               @rsvp 'rsvp', @calcreateby 'createby', @loprecid 'loprecid', 
               @status 'status', @duration 'duration', @units 'number2', @ref 'ref',
               @linkrecid 'linkrecid', @createon 'createon', @createat 'createat'
        INTO #tempcaltable
        DELETE FROM #tempcaltable

        SELECT @tempsql = 'SELECT userid, accountno, company, rectype, ondate, ontime, ' +
                          'actvcode, rsvp, createby, loprecid, status, convert(varchar,duration),' +
                          'convert(varchar,number2), ref, linkrecid, createon, createat FROM ' + @caldb +
                          'cal WHERE recid = ' + CASE  
													WHEN @calrecid LIKE '%''%' THEN '''' + @calrecid + ''''
													WHEN @calrecid LIKE '%''%'  THEN '''' + @calrecid + ''''
													ELSE '''' + @calrecid + ''''
												  END
        INSERT INTO #tempcaltable
        EXEC (@tempsql)
        
        IF (SELECT count(*) FROM #tempcaltable) = 0
            RETURN -200

        SELECT @userid = COALESCE(@userid, userid),
               @accountno = COALESCE(@accountno, accountno),
               @contact = COALESCE(@contact, company),
               @rectype = COALESCE(@rectype, rectype),
               @ondate = COALESCE(@ondate, ondate),
               @ontime = COALESCE(@ontime, ontime),
               @actvcode = COALESCE(@actvcode, actvcode),
               @rsvp = rsvp, @calcreateby = createby, @loprecid = loprecid,
               @status = COALESCE(@status, status),
               @duration = COALESCE(@duration, CONVERT(varchar, duration)),
               @units = COALESCE(@units, CONVERT(varchar, number2)),
               @ref = COALESCE(@ref, ref),
               @linkrecid = COALESCE(@linkrecid, linkrecid),
               @createon = createon, @createat = createat
         FROM #tempcaltable 


        EXEC Castell_SetValue @tempgmnv, 'rectype', @rectype
        EXEC Castell_SetValue @tempgmnv, 'userid', @userid 
        EXEC Castell_SetValue @tempgmnv, 'accountno', @accountno 
        EXEC Castell_SetValue @tempgmnv, 'rectype', @rectype 
        EXEC Castell_SetValue @tempgmnv, 'actvcode', @actvcode 
        --EXEC Castell_SetValue @tempgmnv, 'status', @status 
        EXEC Castell_SetValue @tempgmnv, 'duration', @duration
        EXEC Castell_SetValue @tempgmnv, 'units', @units 
        EXEC Castell_SetValue @tempgmnv, 'ref', @units
        EXEC Castell_SetValue @tempgmnv, 'linkrecid', @linkrecid
    END


/*complete the cal activity or create a new history record from scratch*/
IF @calrecid IS NOT NULL OR @recid IS NULL
    BEGIN
        /*validate rectype*/
        IF (UPPER(@rectype) NOT IN ('A','C','D','E','L','M','O','S','T','U', 'CC','CI','CM','CO','MG','MI','MO'))
              EXEC Castell_SetValue @tempgmnv, 'rectype', 'U'
            
        IF @ondate IS NULL
            SELECT @ondate = @currdate

        IF @ontime IS NULL
            SELECT @ontime = @currtime

        /*Get a Recid for the history record*/
		set @recid = (select dbo.Castell_RecId('MASTER'))
            
        /*get a loprecid if not provided*/
        IF @loprecid IS NULL
            BEGIN
                EXEC Castell_GetValue @tempgmnv, 'loprecid', @loprecid OUTPUT
                IF @loprecid IS NULL
                    BEGIN
						set @loprecid = (select dbo.Castell_RecId('MASTER'))
                        SELECT @loprecid = STUFF(@loprecid, 1, 1, ' ') 
                    END
            END
        
        /*Do the initial insert. */

        INSERT conthist (userid, accountno, status, srectype, loprecid, createby, createon, createat, lastuser, lastdate, lasttime, recid, completedid) 
            VALUES(CASE WHEN @userid IS NULL THEN UPPER(@user) ELSE UPPER(@userid) END,
                   COALESCE(@accountno, ''), COALESCE(@status,' 0'), SUBSTRING(@rectype,1,1), @loprecid, 
                   CASE WHEN @calrecid IS NULL THEN UPPER(@user) ELSE @calcreateby END, /*createby*/ 
                   CASE WHEN @calrecid IS NULL THEN @currdate ELSE @createon END /*createon*/, 
                   CASE WHEN @calrecid IS NULL THEN @currtime ELSE @createat END /*createat*/,  
				   UPPER(@user), /*lastuser*/ @currdate, /*lastdate*/ @currtime, /*lasttime*/ @recid, '' /*completedid*/)

        /*carry over completion notes and delete the cal activity*/
        IF @calrecid IS NOT NULL AND @caldb IS NOT NULL
            BEGIN
                /*carry over the notes*/
                SELECT @tempsql = 'UPDATE conthist ' +
                                  'SET conthist.notes = ' + @caldb + 'cal.notes, ' +
                                  'conthist.status = CASE WHEN ' + @caldb + 'cal.notes' +
                                  ' IS NOT NULL THEN '' 1'' ELSE '' 0'' END' +
                                  ' FROM ' + @caldb + 'cal, conthist' +
                                  ' WHERE ' + @caldb + 'cal.recid = ' + CASE  
																			WHEN @calrecid LIKE '%''%' THEN '''' + @calrecid + ''''
																			WHEN @calrecid LIKE '%''%'  THEN '''' + @calrecid + ''''
																			ELSE '''' + @calrecid + ''''
																		END +
                                  ' AND conthist.recid = ' + CASE  
																WHEN @recid LIKE '%''%' THEN '''' + @recid + ''''
																WHEN @recid LIKE '%''%'  THEN '''' + @recid + ''''
																ELSE '''' + @recid + ''''
															 END
                EXEC (@tempsql)

               /*delete the completed cal activity*/
                SELECT @tempsql = 'DELETE FROM ' + @caldb + 'cal WHERE recid = ' + CASE  
																						WHEN @calrecid LIKE '%''%' THEN '''' + @calrecid + ''''
																						WHEN @calrecid LIKE '%''%'  THEN '''' + @calrecid + ''''
																						ELSE '''' + @calrecid + ''''
																				   END 
                EXEC (@tempsql)

               /*update summary tab for removed cal record*/
               IF @accountno IS NOT NULL
                    EXEC Castell_UpdateSummary @accountno, 'CAL', @user, @caldb

               SELECT @tempsql = CASE  
									WHEN @calrecid LIKE '%''%' THEN '''' + @calrecid + ''''
									WHEN @calrecid LIKE '%''%'  THEN '''' + @calrecid + ''''
									ELSE '''' + @calrecid + ''''
								 END
               --EXEC ('EXEC ' + @caldb + 'Castell_UpdateSyncLog ''cal'', ' + @tempsql  + ', "' + @user + '", ''D'', ''complete''')
            END
       
       /*update sync info*/
       --EXEC Castell_UpdateSyncLog 'conthist', @recid, @user, 'N'
  
    END
 
/*add notes*/
EXEC Castell_GetValue @tempgmnv, 'notes', @value OUTPUT
IF @value IS NOT NULL
    BEGIN
        UPDATE conthist SET status = substring(status, 1,1) + '1', notes=@value WHERE recid = @recid
        --EXEC Castell_UpdateSyncLog 'conthist', @recid, @user, 'U', 'notes'
        --EXEC Castell_UpdateSyncLog 'conthist', @recid, @user, 'U', 'status'
        EXEC Castell_EraseName @tempgmnv, 'notes'
    END
SELECT @value = NULL


/*schedule rsvp if appropriate  NEED TO CHANGE THIS!!*/
       IF @rsvp = 'Y'
            BEGIN
                SELECT @value = 'RSVP Re:' + @ref

                EXEC Castell_Create @rsvpgmnv OUTPUT

                EXEC Castell_SetValue @rsvpgmnv, 'userid', @calcreateby
                EXEC Castell_SetValue @rsvpgmnv, 'accountno', @accountno
                EXEC Castell_SetValue @rsvpgmnv, 'ondate', @currdate
                EXEC Castell_SetValue @rsvpgmnv, 'ontime', @currtime
                EXEC Castell_SetValue @rsvpgmnv, 'actvcode', 'Cmp'
                EXEC Castell_SetValue @rsvpgmnv, 'rectype', 'M'
                EXEC Castell_SetValue @rsvpgmnv, 'rsvp', '*'
                EXEC Castell_SetValue @rsvpgmnv, 'company', @contact
                EXEC Castell_SetValue @rsvpgmnv, 'ref', @value
                EXEC Castell_SetValue @rsvpgmnv, 'user', @user
                EXEC Castell_SetValue @rsvpgmnv, 'contactdb', @contactdb

                SELECT @value = NULL
                
                EXEC ('EXEC ' + @caldb + 'Castell_WriteSchedule ''' + @rsvpgmnv + '''')
                EXEC Castell_GetValue @rsvpgmnv, 'recid', @calrecid OUTPUT

                SELECT @tempsql = 'UPDATE ' + @caldb + 'cal ' +
                                  'SET ' + @caldb + 'cal.notes = conthist.notes, ' + 
                                  @caldb + 'cal.status = CASE WHEN conthist.notes IS NOT ' +
                                  'NULL THEN '' 1'' ELSE '' 0'' END ' +
                                  'FROM ' + @caldb + 'cal, conthist ' +
                                  'WHERE ' + @caldb + 'cal.recid = ' + CASE  
																			WHEN @calrecid LIKE '%''%' THEN '''' + @calrecid + ''''
																			WHEN @calrecid LIKE '%''%'  THEN '''' + @calrecid + ''''
																			ELSE '''' + @calrecid + ''''
																		END +
                                  ' AND conthist.recid = ' + CASE  
																WHEN @calrecid LIKE '%''%' THEN '''' + @calrecid + ''''
																WHEN @calrecid LIKE '%''%'  THEN '''' + @calrecid + ''''
																ELSE '''' + @calrecid + ''''
															  END
                EXEC (@tempsql)
           END


EXEC Castell_SetValue @tempgmnv, 'ondate', @ondate 
EXEC Castell_SetValue @tempgmnv, 'ontime', @ontime

/*build the reference line w/ the contact name*/
SELECT @ref = CASE 
	WHEN @ref IS NULL AND @contact IS NOT NULL THEN '(oc:' + @contact + ')'
	WHEN @ref IS NOT NULL AND @contact IS NOT NULL THEN @ref + ' (oc:' + @contact + ')'
	WHEN @ref IS NULL AND @contact IS NULL THEN '(oc:)'
	WHEN @ref IS NOT NULL AND @contact IS NULL THEN @ref + ' (oc:)'
END

EXEC Castell_SetValue @tempgmnv, 'ref', @ref

/*Now handle updating history activities and filling in the fields*/
SELECT @namefield 'name', @valuefield 'value' INTO #tempgmnv

INSERT INTO #tempgmnv
EXEC ('SELECT fieldname, value FROM ##gmnv' + @tempgmnv)

EXEC Castell_Delete @tempgmnv

DECLARE fieldhistorycursor CURSOR 
FOR SELECT name, value FROM #tempgmnv

OPEN fieldhistorycursor

FETCH NEXT FROM fieldhistorycursor INTO @fieldname, @value

WHILE @@fetch_status = 0
	BEGIN
		SELECT @datatype=NULL
		SELECT @datatype=DATA_TYPE from INFORMATION_SCHEMA.COLUMNS where COLUMN_NAME = @fieldname and TABLE_NAME = 'CONTHIST'

		IF UPPER(@fieldname) = 'RECTYPE'
        SELECT @value = UPPER(@value) + ' ' + @successprivate

		IF upper(@fieldname) <> 'RECID' and upper(@fieldname) <> 'NOTES' and @datatype is not NULL
		BEGIN
			SELECT @setcmd = ' [' + @fieldname + '] = ' 
			+
			CASE  --deal with data types that require quotes versus those that don't.  Also deal with NULL
				WHEN @value IS NULL
					THEN 'NULL' 
				WHEN UPPER(@datatype) NOT IN ('FLOAT', 'INT', 'REAL','NUMERIC','SMALLINT','TINYINT','DECIMAL')
					THEN '"' + REPLACE(@value, '"', '""') + '"'
				ELSE @value 
			END 
			+
			' WHERE recid = ' 
			+ 
			CASE  --deal with recids with quote marks in them
				WHEN @recid LIKE '%''%' THEN '"' + @recid + '"'
				WHEN @recid LIKE '%"%'  THEN '''' + @recid + '''' 
				ELSE '''' + @recid + ''''
 			END

			EXEC('UPDATE CONTHIST SET ' + @setcmd)

			--EXEC Castell_UpdateSyncLog 'CONTHIST', @recid, @user, 'U', @fieldname
		END --most fields

		FETCH NEXT FROM fieldhistorycursor INTO @fieldname, @value
	END
CLOSE fieldhistorycursor
DEALLOCATE fieldhistorycursor

UPDATE conthist SET lastuser = UPPER(@user), lastdate = @currdate, lasttime = @currtime WHERE recid = @recid

--EXEC Castell_UpdateSyncLog 'conthist', @recid, @user, 'U', 'lastuser'
--EXEC Castell_UpdateSyncLog 'conthist', @recid, @user, 'U', 'lastdate'
--EXEC Castell_UpdateSyncLog 'conthist', @recid, @user, 'U', 'lasttime'

EXEC Castell_SetValue @gmnv, 'recid', @recid

/*update the summary tab for history*/
IF @accountno IS NOT NULL
    EXEC Castell_UpdateSummary @accountno, 'CONTHIST', @user




GO
