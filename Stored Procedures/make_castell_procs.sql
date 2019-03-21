
/****** Object:  StoredProcedure [dbo].[Castell_WriteSchedule]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_WriteSchedule]
GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteRelationship]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_WriteRelationship]
GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteOtherContact]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_WriteOtherContact]
GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteOpportunity]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_WriteOpportunity]
GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteMailbox]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_WriteMailbox]
GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteLinkedDoc]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_WriteLinkedDoc]
GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteHistory]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_WriteHistory]
GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteGroupMember]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_WriteGroupMember]
GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteGroup]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_WriteGroup]
GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteDetail]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_WriteDetail]
GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteContsupp]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_WriteContsupp]
GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteContactNotes]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_WriteContactNotes]
GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteContact]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_WriteContact]
GO

/****** Object:  StoredProcedure [dbo].[Castell_UpdateSyncLog]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_UpdateSyncLog]
GO

/****** Object:  StoredProcedure [dbo].[Castell_UpdateSummary]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_UpdateSummary]
GO

/****** Object:  StoredProcedure [dbo].[Castell_SetValue]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_SetValue]
GO

/****** Object:  StoredProcedure [dbo].[Castell_NameExists]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_NameExists]
GO

/****** Object:  StoredProcedure [dbo].[Castell_GetValueFromIndex]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_GetValueFromIndex]
GO

/****** Object:  StoredProcedure [dbo].[Castell_GetValue]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_GetValue]
GO

/****** Object:  StoredProcedure [dbo].[Castell_GetNameFromIndex]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_GetNameFromIndex]
GO

/****** Object:  StoredProcedure [dbo].[Castell_FormatPhone]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_FormatPhone]
GO

/****** Object:  StoredProcedure [dbo].[Castell_EraseName]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_EraseName]
GO

/****** Object:  StoredProcedure [dbo].[Castell_EraseAll]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_EraseAll]
GO

/****** Object:  StoredProcedure [dbo].[Castell_Delete]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_Delete]
GO

/****** Object:  StoredProcedure [dbo].[Castell_CreateCopy]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_CreateCopy]
GO

/****** Object:  StoredProcedure [dbo].[Castell_Create]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_Create]
GO

/****** Object:  StoredProcedure [dbo].[Castell_Count]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_Count]
GO

/****** Object:  StoredProcedure [dbo].[Castell_Copy]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_Copy]
GO

/****** Object:  StoredProcedure [dbo].[Castell_ChangeName]    Script Date: 3/21/2019 10:22:51 AM ******/
DROP PROCEDURE [dbo].[Castell_ChangeName]
GO

/****** Object:  StoredProcedure [dbo].[Castell_ChangeName]    Script Date: 3/21/2019 10:22:51 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[Castell_ChangeName] (@gmnv varchar(20), @oldname varchar(20), @newname varchar(20))
    
AS

SET NOCOUNT ON

DECLARE @upcount int
DECLARE @cntcmd varchar(40)

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
    RETURN -120

SELECT @oldname = UPPER(@oldname)
SELECT @newname = UPPER(@newname)

SELECT @upcount 'upcount' INTO #counttemp /*create the temp table to store rowcount
							                         results of update*/
DELETE FROM #counttemp 
SELECT @cntcmd = ' insert #counttemp select @@ROWCOUNT'

EXEC('UPDATE ##gmnv' + @gmnv + ' SET fieldname = ''' + @newname + ''' WHERE UPPER(fieldname) = ''' + @oldname + ''''
     + @cntcmd)

SELECT @upcount = upcount FROM #counttemp

RETURN @upcount









GO

/****** Object:  StoredProcedure [dbo].[Castell_Copy]    Script Date: 3/21/2019 10:22:51 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO








CREATE PROCEDURE [dbo].[Castell_Copy] (@destgmnv varchar(20), @sourcegmnv varchar(20))
    
AS

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @destgmnv )) IS NULL OR 
    (SELECT object_id('tempdb..##gmnv' + @sourcegmnv )) IS NULL
    RETURN -120

EXEC ('INSERT INTO ##gmnv' + @destgmnv + ' SELECT * FROM ##gmnv' + @sourcegmnv)

RETURN 1






GO

/****** Object:  StoredProcedure [dbo].[Castell_Count]    Script Date: 3/21/2019 10:22:51 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO







CREATE PROCEDURE [dbo].[Castell_Count] (@gmnv varchar(20))
    
AS

SET NOCOUNT ON

DECLARE @retval int

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
   BEGIN
        SELECT @retval = -120
        GOTO TheEnd
   END

SELECT @retval 'numnames' INTO #temptable
DELETE FROM #temptable

INSERT INTO #temptable
EXEC ('SELECT count(*) FROM ##gmnv' + @gmnv)

SELECT @retval = numnames FROM #temptable

TheEnd:
RETURN @retval





GO

/****** Object:  StoredProcedure [dbo].[Castell_Create]    Script Date: 3/21/2019 10:22:51 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO







CREATE PROCEDURE [dbo].[Castell_Create] (@gmnv varchar(20) OUTPUT)
AS

/* RETURNS -120 IF THE TABLE ALREADY EXISTS*/
SET NOCOUNT ON

DECLARE @dDate datetime
DECLARE @sTime varchar(8)

SELECT @dDate = getdate()

SELECT @sTime = RIGHT(REPLICATE('0', 8) + 
                CONVERT(varchar,(3600 * DATEPART(hour, @dDate)) + (60 * DATEPART(minute, @dDate)) + 
                     DATEPART(second, @dDate)) + CONVERT(varchar,DATEPART(ms, @dDate)), 8)

IF (SELECT object_id('tempdb..##gmnv' + @sTime )) IS NULL
    BEGIN
        EXEC ('DECLARE @index int DECLARE @name varchar(20) DECLARE @value varchar(8000) SELECT @index "indexval", @name "fieldname", @value "value" INTO ##gmnv' + @sTime)
        EXEC ('DELETE FROM ##gmnv' + @sTime)
        SELECT @gmnv = @sTime
    END
ELSE
    RETURN -120










GO

/****** Object:  StoredProcedure [dbo].[Castell_CreateCopy]    Script Date: 3/21/2019 10:22:51 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO








CREATE PROCEDURE [dbo].[Castell_CreateCopy] (@gmnv varchar(20), @copy varchar(20) OUTPUT)
    
AS

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL  
    RETURN -120

EXEC Castell_Create @copy OUTPUT

EXEC ('INSERT INTO ##gmnv' + @copy + ' SELECT * FROM ##gmnv' + @gmnv)









GO

/****** Object:  StoredProcedure [dbo].[Castell_Delete]    Script Date: 3/21/2019 10:22:51 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO







CREATE PROCEDURE [dbo].[Castell_Delete] (@gmnv varchar(20))
    
AS
DECLARE @SQL AS NVARCHAR(200)
SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NOT NULL
    BEGIN
		SET @SQL = 'DROP TABLE ' + '##gmnv' + @gmnv 
		EXECUTE sp_executesql @SQL
    END
    --EXEC Castell_EraseAll @gmnv
ELSE RETURN -120

    




GO

/****** Object:  StoredProcedure [dbo].[Castell_EraseAll]    Script Date: 3/21/2019 10:22:51 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO







CREATE PROCEDURE [dbo].[Castell_EraseAll] (@gmnv varchar(20))
    
AS

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
    RETURN -120

EXEC('DELETE FROM ##gmnv' + @gmnv)






GO

/****** Object:  StoredProcedure [dbo].[Castell_EraseName]    Script Date: 3/21/2019 10:22:51 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO







CREATE PROCEDURE [dbo].[Castell_EraseName] (@gmnv varchar(20), @name varchar(20))
    
AS

SET NOCOUNT ON

/*returns -120 if gmnv does not exist*/

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
    RETURN -120

SELECT @name = UPPER(@name)
EXEC('DELETE FROM ##gmnv' + @gmnv + ' WHERE UPPER(fieldname) = '' + @name + ''')






GO

/****** Object:  StoredProcedure [dbo].[Castell_FormatPhone]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO







CREATE PROCEDURE [dbo].[Castell_FormatPhone] (@phone varchar(25) OUTPUT)

AS

SET NOCOUNT ON

DECLARE @retval int

SELECT @retval = 1

/* 3101234567 */
IF (DATALENGTH(@phone) = 10 AND @phone NOT LIKE '(%' AND @phone NOT LIKE '%)%' 
    AND @phone NOT LIKE '%-%')

	SELECT @phone = '(' + SUBSTRING(@phone,1,3) + ')' + SUBSTRING(@phone, 4, 3) +
			 '-' + SUBSTRING(@phone,7,4)
/* (310)1234567 */
ELSE IF (DATALENGTH(@phone) = 12 AND @phone LIKE '(%' AND SUBSTRING(@phone,5,1) = ')'
	AND @phone NOT LIKE '%-%')
		
	SELECT @phone = SUBSTRING(@phone, 1, 8) + '-' + SUBSTRING(@phone, 9, 4)
	
/* 310-123-4567 OR 310/123-4567*/
ELSE IF (DATALENGTH(@phone) = 12 AND @phone NOT LIKE '(%' AND @phone NOT LIKE '%)%' AND
		 SUBSTRING(@phone, 4,1) IN ('-', '/') AND SUBSTRING(@phone,8,1) = '-')

		SELECT @phone = '(' + SUBSTRING(@phone, 1,3) + ')' + SUBSTRING(@phone, 5, 8)

/* 310 123 4567 */
ELSE IF (DATALENGTH(@phone) = 12 AND @phone NOT LIKE '(%' AND @phone NOT LIKE '%)%' AND
         SUBSTRING(@phone, 4, 1) = ' ' AND SUBSTRING(@phone, 8, 1) = ' ')

         SELECT @phone = '(' + SUBSTRING(@phone, 1,3) + ')' + SUBSTRING(@phone, 5, 3) +
                         '-' + SUBSTRING(@phone, 9, 4)
ELSE SELECT @retval = 0

RETURN @retval


                         

 







GO

/****** Object:  StoredProcedure [dbo].[Castell_GetNameFromIndex]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO








CREATE PROCEDURE [dbo].[Castell_GetNameFromIndex] (@gmnv varchar(20), @indexval int, @name varchar(20) OUTPUT)

AS

SET NOCOUNT ON

DECLARE @namefield varchar(20)
DECLARE @indexfield  int
DECLARE @retval int

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
   BEGIN
        SELECT @retval = -120
        GOTO Error
   END

SELECT @namefield 'name', @indexval 'indexval' INTO #tempgmnv
DELETE FROM #tempgmnv 

INSERT INTO #tempgmnv
EXEC ('SELECT fieldname, indexval FROM ##gmnv' + @gmnv)

DECLARE fieldcursor CURSOR 
FOR SELECT name, indexval FROM #tempgmnv

OPEN fieldcursor

FETCH NEXT FROM fieldcursor INTO @namefield, @indexfield

SELECT @retval = 0

WHILE @@fetch_status = 0
    BEGIN
        IF @indexfield = @indexval
            GOTO TheEnd

        FETCH NEXT FROM fieldcursor INTO @namefield, @indexfield
    END

CLOSE fieldcursor
DEALLOCATE fieldcursor

Error:
RETURN @retval

TheEnd:
CLOSE fieldcursor
DEALLOCATE fieldcursor
SELECT @name = @namefield








GO

/****** Object:  StoredProcedure [dbo].[Castell_GetValue]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO








CREATE PROCEDURE [dbo].[Castell_GetValue] (@gmnv varchar(20), @name varchar(20), @value varchar(8000) OUTPUT)
    
AS

/*RETURNS -120 IF TABLE DOES NOT EXIST*/
SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
    RETURN -120

SELECT @name = UPPER(@name)

SELECT @value 'value' INTO #temptable
DELETE FROM #temptable

INSERT INTO #temptable
EXEC('SELECT value FROM ##gmnv' + @gmnv + ' WHERE UPPER(fieldname) = ''' + @name + '''')

SELECT @value = value FROM #temptable










GO

/****** Object:  StoredProcedure [dbo].[Castell_GetValueFromIndex]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO








CREATE PROCEDURE [dbo].[Castell_GetValueFromIndex] (@gmnv varchar(20), @indexval int, @value varchar(8000) OUTPUT)
    
AS

SET NOCOUNT ON

DECLARE @valuefield varchar(8000)
DECLARE @indexfield  int

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
   RETURN -120
   

SELECT @valuefield 'value', @indexval 'indexval' INTO #tempgmnv
DELETE FROM #tempgmnv 

INSERT INTO #tempgmnv
EXEC ('SELECT value, indexval FROM ##gmnv' + @gmnv)

DECLARE fieldcursor CURSOR 
FOR SELECT value, indexval FROM #tempgmnv

OPEN fieldcursor

FETCH NEXT FROM fieldcursor INTO @valuefield, @indexfield

WHILE @@fetch_status = 0
    BEGIN
        IF @indexfield = @indexval
            GOTO TheEnd
           
        FETCH NEXT FROM fieldcursor INTO @valuefield, @indexfield
    END



TheEnd:
CLOSE fieldcursor
DEALLOCATE fieldcursor
SELECT @value = @valuefield








GO

/****** Object:  StoredProcedure [dbo].[Castell_NameExists]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO








CREATE PROCEDURE [dbo].[Castell_NameExists] (@gmnv varchar(20), @name varchar(20))
    
AS

SET NOCOUNT ON

DECLARE @namefield varchar(20)
DECLARE @indexval  int
DECLARE @retval int

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
   BEGIN
        SELECT @retval = -120
        GOTO TheEnd
   END

SELECT @namefield 'name', @indexval 'indexval' INTO #tempgmnv
DELETE FROM #tempgmnv 

INSERT INTO #tempgmnv
EXEC ('SELECT name, indexval FROM ##gmnv' + @gmnv)

DECLARE fieldcursor CURSOR 
FOR SELECT name, indexval FROM #tempgmnv

OPEN fieldcursor

FETCH NEXT FROM fieldcursor INTO @namefield, @indexval

SELECT @retval = 0

WHILE @@fetch_status = 0
    BEGIN
        IF UPPER(@namefield) = UPPER(@name)
            BEGIN
                SELECT @retval = @indexval
                GOTO Done
            END
        FETCH NEXT FROM fieldcursor INTO @namefield, @indexval
    END

Done:
CLOSE fieldcursor
DEALLOCATE fieldcursor

TheEnd:
RETURN @retval









GO

/****** Object:  StoredProcedure [dbo].[Castell_SetValue]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO








CREATE PROCEDURE [dbo].[Castell_SetValue](@gmnv varchar(20), @fieldname varchar(20), @value varchar(8000))

AS

SET NOCOUNT ON

/*RETURNS -120 IF TABLE DOES NOT EXIST*/
DECLARE @dummy varchar(20)
DECLARE @maxindex int
DECLARE @smax varchar(4)

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
    RETURN -120

SELECT @dummy 'fieldname', @maxindex 'indexval' INTO #tempbuffer
DELETE FROM #tempbuffer 

INSERT INTO #tempbuffer
EXEC('SELECT fieldname, indexval FROM ##gmnv' + @gmnv + ' WHERE fieldname = "' + @fieldname + '"')


IF (SELECT fieldname FROM #tempbuffer) IS NULL
    BEGIN

        /*get the highest index in the gmnv currently*/
        SELECT @maxindex 'maxindex' INTO #tempindexbuf
        DELETE FROM #tempindexbuf
        INSERT INTO #tempindexbuf
            EXEC('SELECT MAX(indexval) FROM ##gmnv' + @gmnv)

        /*if the gmnv is empty, assign the index to be 1*/
        SELECT @maxindex = CASE maxindex WHEN NULL THEN 1
                                         ELSE maxindex + 1
                           END
        FROM #tempindexbuf

        SELECT @smax = CONVERT(varchar,@maxindex)

        EXEC('INSERT ##gmnv' + @gmnv + ' VALUES("' + @smax + '","' + @fieldname + '","' + @value + '")')

    END
ELSE
    BEGIN
        SELECT @maxindex = indexval FROM #tempbuffer  
        EXEC("UPDATE ##gmnv" + @gmnv + ' SET value = "' + @value + '" WHERE fieldname = "' 
             + @fieldname + '"')
    END

RETURN isnull(@maxindex, '0')











GO

/****** Object:  StoredProcedure [dbo].[Castell_UpdateSummary]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS OFF
GO

SET QUOTED_IDENTIFIER OFF
GO









CREATE PROCEDURE [dbo].[Castell_UpdateSummary] (@accountno varchar(20), @table varchar(20), @user varchar(8), @caldb varchar(255) = NULL)

AS


SET NOCOUNT ON

DECLARE @nextaction   varchar(65),
		@c2nextaction varchar(65),
		@prevresult   varchar(65),
		@c2prevresult varchar(65),
		@actionon   datetime,
		@c2actionon datetime,
		@closedate   datetime,
		@c2closedate datetime,
		@meetdateon   datetime,
		@c2meetdateon datetime,
		@meettimeat   varchar(8),
		@c2meettimeat varchar(8),
		@callbackon   datetime,
		@c2callbackon datetime,
		@callbackat   varchar(8),
		@c2callbackat varchar(8),
		@lastconton datetime,
		@c2lastconton datetime,
		@lastcontat varchar(8),
		@c2lastcontat varchar(8),
		@lastatmpon datetime,
		@c2lastatmpon datetime,
		@lastatmpat varchar(8),
		@c2lastatmpat varchar(8),
		@c2recid varchar(15),
		@setcmd varchar(1000),
		@setcmd2 varchar(1000),
		@setcmd3 varchar(1000),
		@setcmd4 varchar(1000),
		@fieldname  varchar(50),
		@modfield varchar(50)

/*retrieve values from contact2*/
SELECT @c2closedate = closedate, @c2meetdateon = meetdateon, @c2meettimeat = meettimeat, @c2callbackon = callbackon,
	@c2callbackat = callbackat, @c2nextaction = nextaction, @c2prevresult = prevresult, @c2recid = recid,
	@c2lastconton = lastconton, @c2lastatmpat = lastatmpat, @c2lastcontat = lastcontat, @c2lastatmpon = lastatmpon
 	FROM contact2 WHERE accountno=@accountno

IF @@rowcount < 1 /*if there's no contact2 record, make one*/
	BEGIN
		set @c2recid = (select dbo.Castell_RecId('MASTER'))
		INSERT contact2 (accountno, recid) VALUES (@accountno, @c2recid)
		--EXEC Castell_UpdateSyncLog 'contact2', @c2recid, @user, 'N'
	END

/*Create temp table to hold fields to have sync info updated*/

SELECT @modfield 'modfield' INTO #UpdateSync
DELETE FROM #UpdateSync    

IF UPPER(@table) = 'CAL'
	BEGIN
		IF @caldb IS NULL
            BEGIN
                IF EXISTS(SELECT object_id('CAL'))
                    SELECT @caldb = ''
                ELSE
                    RETURN -190
            END
        --ELSE SELECT @caldb = @caldb + '..'

		/*set the nextaction*/
		SELECT @setcmd 'varcharcolumn' INTO #varchartable
        DELETE FROM #varchartable

        SELECT @setcmd = 'SELECT ref FROM ' + @caldb + 'cal WHERE accountno = "' + @accountno + '"' + ' AND recid in (SELECT RIGHT(min(rectype + ISNULL(CONVERT(varchar,ondate,112),'' '') + ' 
        SELECT @setcmd2 ='ISNULL(ontime, '' '') + userid + recid), 15) FROM ' + @caldb + 'cal WHERE accountno=''' + @accountno + ''')'

        INSERT INTO #varchartable
        EXEC (@setcmd + @setcmd2)
 
		SELECT @nextaction = varcharcolumn FROM #varchartable
        DELETE FROM #varchartable

		/*set the actionon*/
        SELECT @actionon 'datetimecolumn' INTO #datetimetable
        DELETE FROM #datetimetable

		SELECT @setcmd = 'SELECT ondate FROM ' + @caldb + 'cal WHERE ' + 'accountno = ''' + @accountno + ''' AND recid in (SELECT RIGHT(min(rectype + ISNULL(CONVERT(varchar,ondate,112), '' '')' 
        SELECT @setcmd2 ='+ ISNULL(ontime, '' '') + userid + recid), 15) FROM ' + @caldb + 'cal WHERE ' + 'accountno=''' + @accountno + ''' AND rectype = ''T'')'
        
        INSERT INTO #datetimetable
        EXEC(@setcmd + @setcmd2)

        SELECT @actionon = datetimecolumn FROM #datetimetable
        DELETE FROM #datetimetable
		
		/*set the closedate*/
        
		SELECT @setcmd = 'SELECT ondate FROM ' + @caldb + 'cal WHERE accountno=''' + @accountno + '''' + 'AND recid in (SELECT RIGHT(min(rectype + ISNULL(CONVERT(varchar,ondate,112), '' '') + ' 
        SELECT @setcmd2 ='ISNULL(ontime, '' '') + userid + recid), 15) FROM ' + @caldb + 'cal WHERE accountno=''' + @accountno + ''' AND rectype = ''S'')'
        
        INSERT INTO #datetimetable
        EXEC (@setcmd + @setcmd2)

        SELECT @closedate = datetimecolumn FROM #datetimetable
        DELETE FROM #datetimetable
		
		/*set the meetdateon & meettimeat*/
        SELECT @meetdateon 'datefield', @meettimeat 'timefield' INTO #dateandtimetable
        DELETE FROM #dateandtimetable

        SELECT @setcmd = 'SELECT ondate, CASE WHEN PATINDEX(''%:%'', ontime) = 0 THEN ontime' + ' WHEN CONVERT(int, SUBSTRING(ontime,1,(PATINDEX(''%:%'',ontime) - 1))) > 12 ' + 'THEN CONVERT(varchar,(CONVERT(int, SUBSTRING(ontime,1,(PATINDEX(''%:%'',ontime) - 1))) - 12)) + '
	    SELECT @setcmd2 =' SUBSTRING(ontime,PATINDEX(''%:%'',ontime),3) + ''pm ''' + ' WHEN CONVERT(int, SUBSTRING(ontime,1,(PATINDEX(''%:%'',ontime) - 1))) = 12' + ' THEN ontime + ''pm '' ELSE ontime + ''am '' END'       
        SELECT @setcmd3 =' FROM ' + @caldb + 'cal WHERE accountno=''' + @accountno + ''' AND recid in ' + '(SELECT RIGHT(min(rectype + ISNULL(CONVERT(varchar,ondate,112), '' '') + ' 
		SELECT @setcmd4 ='ISNULL(ontime, '' '') + userid + recid), 15) FROM ' + @caldb + 'cal WHERE accountno =''' + @accountno + ''' AND rectype = ''A'')' 

        
        INSERT INTO #dateandtimetable
        EXEC(@setcmd + @setcmd2 + @setcmd3 + @setcmd4)

        SELECT @meetdateon = datefield, @meettimeat = timefield FROM #dateandtimetable
        DELETE FROM #dateandtimetable
		
        /*set callbackon & call backat*/
		SELECT @setcmd4 = substring(@setcmd4, 1, 42) + ' FROM ' + @caldb + 'cal WHERE accountno =''' + @accountno + ''' AND rectype = ''C'')' 

        INSERT INTO #dateandtimetable
        EXEC(@setcmd + @setcmd2 + @setcmd3 + @setcmd4)

        SELECT @callbackon = datefield, @callbackat = timefield FROM #dateandtimetable
        DELETE FROM #dateandtimetable
		
        SELECT @setcmd = NULL
		/*Evaluate which fields need to be updated*/
		IF @c2nextaction <> @nextaction AND NOT (@c2nextaction ='' AND @nextaction IS NULL)
			BEGIN
				SELECT @setcmd = ' nextaction =''' + REPLACE(@nextaction,"'","''")  + ''''
				INSERT #UpdateSync VALUES('NEXTACTION')
			END

		IF @c2closedate <> @closedate 
			BEGIN
				SELECT @setcmd = @setcmd + CASE WHEN @setcmd IS NOT NULL THEN ',' END 
				 		+ ' closedate = ' + CASE
								      WHEN @closedate IS NOT NULL THEN 
									'''' + CONVERT(varchar,@closedate) + ''''
								      ELSE 'NULL'
								    END
				INSERT #UpdateSync VALUES('CLOSEDATE')
			END

		IF @c2meetdateon <> @meetdateon
			BEGIN
				SELECT @setcmd = @setcmd + CASE WHEN @setcmd IS NOT NULL THEN ',' END
 						 + ' meetdateon = ' + CASE
									WHEN @meetdateon IS NOT NULL THEN
									  '''' + CONVERT(varchar,@meetdateon) + ''''
									ELSE 'NULL'
								      END
				INSERT #UpdateSync VALUES('MEETDATEON')
			END

		IF @c2meettimeat <> @meettimeat AND NOT (@c2meettimeat = '' AND @meettimeat IS NULL)
			BEGIN
				SELECT @setcmd = @setcmd + CASE WHEN @setcmd IS NOT NULL THEN ',' END
						+ ' meettimeat =''' + @meettimeat + ''''
				INSERT #UpdateSync VALUES('MEETTIMEAT')
			END

		IF @c2callbackon <> @callbackon
			BEGIN
				SELECT @setcmd = @setcmd + CASE WHEN @setcmd IS NOT NULL THEN ',' END
						 + ' callbackon = ' + CASE
									WHEN @callbackon IS NOT NULL THEN
									  '''' + CONVERT(varchar,@callbackon) + ''''
									ELSE 'NULL'
								      END
				INSERT #UpdateSync VALUES('CALLBACKON')
			END

		IF @c2callbackat <> @callbackat AND NOT (@c2callbackat ='' AND @callbackat IS NULL)
			BEGIN
				SELECT @setcmd = @setcmd + CASE WHEN @setcmd IS NOT NULL THEN ',' END 
						+ ' callbackat =''' + @callbackat + ''''
				INSERT #UpdateSync VALUES('CALLBACKAT')
			END
		
		
		IF @setcmd IS NOT NULL
			BEGIN
				SELECT @setcmd = @setcmd + ' WHERE recid = ' + 
						 CASE  
							   WHEN @c2recid LIKE '%''%' THEN '"' + @c2recid + '"'
							   WHEN @c2recid LIKE '%"%'  THEN '''' + @c2recid + ''''
							   ELSE '''' + @c2recid + ''''
                          END
				
				EXEC('UPDATE contact2 SET' + @setcmd)
			END
	END

IF UPPER(@table) = 'CONTHIST'
	BEGIN
		SELECT @setcmd = NULL

		/*lastconton, lastcontat, lastatmpon, lastatmpat*/
		SELECT @lastconton = CASE WHEN rectype NOT LIKE '%U' AND SUBSTRING(rectype,1,1) IN ('C','A')
					THEN ondate END, 
			@lastcontat = CASE WHEN rectype NOT LIKE '%U' AND SUBSTRING(rectype,1,1) IN ('C','A')
					THEN ontime END,
			@lastatmpon = CASE WHEN rectype LIKE '%U' AND SUBSTRING(rectype,1,1) IN ('C','A')
					THEN ondate END,
			@lastatmpat = CASE WHEN rectype LIKE '%U' AND SUBSTRING(rectype,1,1) IN ('C','A')
					THEN ontime END,
			@prevresult = CASE WHEN PATINDEX('%(oc:%', ref) = 0 THEN ref
                               ELSE SUBSTRING(ref, 1, PATINDEX('%(oc:%', ref) - 1) END

		FROM conthist WHERE accountno = @accountno AND recid IN
		(select right(max(accountno+CONVERT(varchar,ondate,112)+recid),15) FROM conthist 
		 where accountno = @accountno )

				
		/*begin evaluating what needs to be updated*/
		IF @c2prevresult <> @prevresult AND NOT (@c2prevresult = '' AND @prevresult IS NULL)
			BEGIN
				SELECT @setcmd = ' prevresult =''' + REPLACE(@prevresult,"'","''")  + ''''
				INSERT #UpdateSync VALUES('PREVRESULT')
			END

		IF @c2lastconton <> @lastconton AND @lastconton IS NOT NULL
			BEGIN
				SELECT @setcmd = @setcmd + CASE WHEN @setcmd IS NOT NULL THEN ',' END + ' lastconton =''' + CONVERT(varchar,@lastconton) + ''''
				INSERT #UpdateSync VALUES('LASTCONTON')
			END

		IF @c2lastcontat <> @lastcontat AND @lastcontat IS NOT NULL
			BEGIN
				SELECT @setcmd = @setcmd + CASE WHEN @setcmd IS NOT NULL THEN ',' END + ' lastcontat =''' + @lastcontat + ''''
				INSERT #UpdateSync VALUES('LASTCONTAT')
			END

		IF @c2lastatmpon <> @lastatmpon AND @lastatmpon IS NOT NULL
			BEGIN
				SELECT @setcmd = @setcmd + CASE WHEN @setcmd IS NOT NULL THEN ',' END+ ' lastatmpon =''' + CONVERT(varchar,@lastatmpon) + ''''
				INSERT #UpdateSync VALUES('LASTATMPON')
			END

		IF @c2lastatmpat <> @lastatmpat AND @lastatmpat IS NOT NULL
			BEGIN
				SELECT @setcmd = @setcmd + CASE WHEN @setcmd IS NOT NULL THEN ',' END + ' lastatmpat =''' + @lastatmpat + ''''
				INSERT #UpdateSync VALUES('LASTATMPAT')
			END

		/*process setcmd*/
		IF @setcmd IS NOT NULL
			BEGIN
				
				SELECT @setcmd = @setcmd + ' WHERE recid = ' + 
						  CASE  
							   WHEN @c2recid LIKE '%''%' THEN '"' + @c2recid + '"'
							   WHEN @c2recid LIKE '%"%'  THEN '''' + @c2recid + ''''
							   ELSE '''' + @c2recid + ''''
                          END
				
				EXEC('UPDATE contact2 SET' + @setcmd)
			END
	END
		
		
--/*Update the Sync Info*/
--DECLARE SyncFields CURSOR
--FOR SELECT modfield from #UpdateSync

--OPEN SyncFields

--FETCH NEXT FROM SyncFields INTO @fieldname

--WHILE @@fetch_status = 0
--	BEGIN
--		--EXEC Castell_UpdateSyncLog 'contact2', @c2recid, @user, 'U', @fieldname
--		FETCH NEXT FROM SyncFields INTO @fieldname
--	END

--CLOSE SyncFields
--DEALLOCATE SyncFields

			 








GO

/****** Object:  StoredProcedure [dbo].[Castell_UpdateSyncLog]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO









CREATE PROCEDURE [dbo].[Castell_UpdateSyncLog] (@tablename varchar(20), /*Table that was updated*/
									 @recid varchar(15),     /*RECID of record modified or added*/
									 @userid varchar(8),     /*USERID who modified the record*/
									 @action varchar(1),     /*N,U,D, for what was done*/
									 @fieldname varchar(20) = NULL) /*Field that was modified, set to 'complete' if a calendar activity was completed.*/

/*
** GM_UpdateSyncLog:  MSSQL Stored Procedure to update the tlog tables used for GoldMine Synchronization
**
** Returns:
**	    -100  : Recid passed is invalid or already exists in the case of @action = N
**	    -120  : Invalid table name passed
**	    -130  : Invalid action passed
**	    -140  : No fieldname passed with Action of U 
**	    -150  : Shadow field passed, no tlog tracking applied
*/

AS

SET NOCOUNT ON
/*DECLARATIONS*/

DECLARE @tableid   varchar(1)	/*symbolic ID for the table modified*/
DECLARE	@timestamp varchar(7)	/*sync and log stamp*/
DECLARE	@tlog      varchar(8)   /*conttlog or gmtlog, depending on table modified*/
DECLARE @temprecid varchar(15)  /*temp frecid field for creating timestamps*/
DECLARE @sqlcmd    varchar(250) /*buffer for sql statements*/
DECLARE @modcmd	   varchar(250) /*buffer for insert/update statements*/
DECLARE @cntcmd	   varchar(50)  /*buffer used for retrieving rowcounts from execs*/

DECLARE @return_status varchar(15)
DECLARE @upcount int
DECLARE @retval int

/*Assign @tableid a value based on the tablename passed*/
SELECT @tableid = CASE UPPER(@tablename)
					WHEN 'CONTACT1' THEN '&'
					WHEN 'CONTACT2' THEN '''' /*single quote*/
					WHEN 'CONTSUPP' THEN '('
					WHEN 'CONTHIST' THEN ')'
					WHEN 'CONTGRPS' THEN '*'
					WHEN 'CONTUDEF' THEN '0'  /*zero*/
					WHEN 'CAL'		THEN '""'
					WHEN 'LOOKUP'	THEN '!'
					WHEN 'FORMS'	THEN '#'
					WHEN 'SCRIPTSW' THEN '4'
					WHEN 'FILTERS'	THEN '+'
					WHEN 'PERPHONE'	THEN '/'
					WHEN 'RESITEMS'	THEN '1'
					WHEN 'IMPEXP'	THEN '2'
					WHEN 'INFOMINE'	THEN '3'
					WHEN 'FIELDS'	THEN '6'
					WHEN 'TRACKS'	THEN '7'
					WHEN 'USERLOG'	THEN '9'
					WHEN 'MAILBOX'	THEN ';'
					WHEN 'OPMGR'	THEN 'B'
					WHEN 'OPMGRFLD'	THEN 'C'
					WHEN 'REPORT32'	THEN '5'
					WHEN 'NOTES'	THEN 'W'
					WHEN 'CASES'	THEN 'R'
					WHEN 'USERS'	THEN '.'
					ELSE NULL
	 			  END

/*Validate Table Name and Action*/

IF @tableid IS NULL 
	RETURN -120

IF @action NOT IN('U','N','D')
	RETURN -130

IF @action='U' AND @fieldname IS NULL
	RETURN -140

IF @action='U' AND @fieldname like 'U[_]%'
	RETURN -150

/*Assign @tlog a value based on the tablename passed*/

SELECT @tlog = CASE SUBSTRING(UPPER(@tablename),1,4)
					WHEN 'CONT' THEN 'CONTTLOG'
					ELSE 'GMTLOG'
			   END

/*Begin processing based on @action passed*/

IF UPPER(@action) = 'N'

	BEGIN	/*1*/
		/*Check if the RECID passed for the new record already exists in the tlog*/
		SELECT @sqlcmd = 'SELECT frecid FROM ' + @tlog + ' WHERE frecid = ''' + @recid + ''' AND tableid = ' + 
						CASE UPPER(@tablename)
							WHEN 'CONTACT2' THEN ''''''''''
							ELSE '''' + @tableid + ''''
						END 

		SELECT @return_status 'return_status' INTO #temptable
        DELETE FROM #temptable WHERE return_status IS NULL

		INSERT INTO #temptable
		EXEC (@sqlcmd)
		
		IF (select count(*) from #temptable) <> 0  /*The recid already exists in the tlog*/
			return -100

		/*RECID is not in the tlog already, so add new zzNew tlog record */

		/*get a timestamp*/
		set @timestamp = (select dbo.Castell_SyncStamp())
		
		/*Build insert statement*/
		
		SELECT @modcmd    = 'INSERT ' + @tlog + ' VALUES ( ''' + @timestamp + ''', ''' + @timestamp + ''', ''' + UPPER(@action) + ''', ' + 
							CASE UPPER(@tablename)
								WHEN 'CONTACT2' THEN ''''''''''
								ELSE '''' + @tableid + ''''
							END
							 + ', '''+ @recid + ''', ''zzNew'', ''~' + UPPER(@userid) + ''')'
		EXEC (@modcmd)
        SELECT @retval = 1
	END	/*1*/


IF UPPER(@action) = 'U'

	BEGIN /*1*/
		
        SELECT @upcount 'upcount' INTO #counttemp /*create the temp table to store rowcount results of update*/
        DELETE FROM #counttemp WHERE upcount IS NULL
		
		/*get a timestamp*/
		set @timestamp = (select dbo.Castell_SyncStamp())

		/*build the update statement*/
		SELECT @modcmd = 'UPDATE ' + @tlog + ' SET syncstamp = ''' + @timestamp + ''', logstamp = ''' + @timestamp + '''' + ', userid = ''~' + UPPER(@userid) + ''' WHERE frecid = ''' + @recid + ''' AND tableid = ' + 
						CASE UPPER(@tablename)
							WHEN 'CONTACT2' THEN ''''''''''
							ELSE '''' + @tableid + ''''
						END 
						+ ' AND fieldname = ''' + UPPER(@fieldname) + ''''
        
        SELECT @cntcmd = ' insert #counttemp select @@ROWCOUNT'
		
		/*attempt the update and get the number of rows it affected*/
		EXEC(@modcmd + @cntcmd)

		IF ((select upcount from #counttemp) < 1) /*there are not already field level updates for this field*/

			BEGIN /*2*/   /*check for zzNew*/
				
				delete from #counttemp /*clear contents of counttemp*/
				
				/*get a timestamp*/
				set @timestamp = (select dbo.Castell_SyncStamp())

				SELECT @modcmd = 'UPDATE ' + @tlog + ' SET syncstamp = ''' + @timestamp + '''' + ', logstamp = ''' + @timestamp + '''' + ', userid = ''~' + UPPER(@userid) + '''' + ' WHERE frecid = ''' + @recid + ''' AND tableid = ' + 
								CASE UPPER(@tablename)
									WHEN 'CONTACT2' THEN ''''''''''
									ELSE '''' + @tableid + ''''
								END 
								+ ' AND fieldname = ''zzNew'''

				/*attempt the update and get the number of rows it affected*/
				EXEC(@modcmd + @cntcmd)
				   

				IF ((select upcount from #counttemp) < 1) /*there is either a zsNew or the tlogs have been purged*/
				   BEGIN /*3*/
						/*get a timestamp*/
						set @timestamp = (select dbo.Castell_SyncStamp())

						/*Build insert statement*/
		
						SELECT @modcmd    = 'INSERT ' + @tlog + ' VALUES ( ''' + @timestamp + ''', ''' + @timestamp + ''', ''' + UPPER(@action) + ''', ' + 
											CASE UPPER(@tablename)
												WHEN 'CONTACT2' THEN ''''''''''
												ELSE '''' + @tableid + ''''
											END 
											 + ', '''+ @recid + ''', ''' + UPPER(@fieldname) + ''', ''~' + UPPER(@userid) + ''')'
						EXEC (@modcmd)
                        SELECT @retval = 4
				   END /*3*/
                   ELSE SELECT @retval = 2
			END /*2*/
            ELSE SELECT @retval = 8

					
	END /*1*/

IF UPPER(@action) = 'D'
	BEGIN  /*1*/

        SELECT @upcount 'delcount' INTO #counttemp2 /*create the temp table to store rowcount results of the delete*/
        DELETE FROM #counttemp2 WHERE delcount IS NULL
		/*Delete the zzNew Record (if it exists, then no field level changes are being kept and it hasn't synchronized yet*/

		SELECT @modcmd = 'DELETE FROM ' + @tlog + ' WHERE frecid = ''' + @recid + ''' AND tableid = ' + 
						CASE UPPER(@tablename)
							WHEN 'CONTACT2' THEN ''''''''''
							ELSE '''' + @tableid + ''''
						END 
						+ ' AND fieldname = ''zzNew''' 

		SELECT @cntcmd = ' insert #counttemp2 select @@ROWCOUNT'
	
		/*attempt the deletion and get the number of rows it affected*/
		EXEC(@modcmd + @cntcmd)

		IF ((SELECT delcount FROM #counttemp2) < 1)
			
			/*There was not a zzNew record, so add a zzzDel or zzzDelcomp*/
			BEGIN /*2*/

				/*If its a completion, set the fieldname to zzzDelComp*/
				IF (UPPER(@tablename)='CAL' AND UPPER(@action)='D' 
							AND UPPER(@fieldname)='COMPLETE')
					SELECT @fieldname = 'zzzDelComp'
				
				ELSE  /*Otherwise, treat as any other other deletion*/
					SELECT @fieldname = 'zzzDel'

				/*get a timestamp*/
				set @timestamp = (select dbo.Castell_SyncStamp())
				
				/*build the insert statement*/
				SELECT @modcmd = 'INSERT ' + @tlog + ' VALUES (''' + @timestamp + ''', ''' + @timestamp + ''', ''' + UPPER(@action) + ''', ' +  
									CASE UPPER(@tablename)
										WHEN 'CONTACT2' THEN ''''''''''
										ELSE '''' + @tableid + ''''
									END 
									 + ', '''+ @recid + ''', ''' + @fieldname + ''', ''~' + UPPER(@userid) + ''')'
				EXEC(@modcmd)
                SELECT @retval = 16
			END /*2*/
            ELSE SELECT @retval = 32

    END /*1*/
RETURN @retval

				








GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteContact]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO




CREATE PROCEDURE [dbo].[Castell_WriteContact] (@gmnv varchar(20))

AS

DECLARE @setcmd1    varchar(max),
		@user varchar(8), 
		@recid varchar(15), 
		@nonusaphone varchar(1), 
		@tempphone varchar(25), 
		@status varchar(3), 
		@email varchar(255),
		@sub_contact varchar(20),
		@url varchar(255), 
		@contact varchar(40), 
		@company varchar(40), 
		@fieldname varchar(50),
		@value varchar(8000), 
		@datatype varchar(32), 
		@tablename varchar(20), 
		@accountno varchar(20), 
		@currdate datetime, 
		@currtime varchar(5), 
		@c2recid varchar(15), 
		@gmversion int, 
		@insertstr varchar(1000), 
		@valuestr varchar(8000), 
		@updateemail varchar(1), 
		@namefield varchar(20), 
		@valuefield varchar(8000), 
		@fieldlength int,
		@tempgmnv varchar(20),
		@notes varchar(max),
		@nrecid varchar(15),
		@gmnvlog varchar(20),
		@reflogupdate varchar(80),
		@oldvalue nvarchar(100),
		@oldvalquery nvarchar(300),
		@fieldnamelocallabel varchar(25)

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
            RETURN -120

EXEC Castell_CreateCopy @gmnv, @tempgmnv OUTPUT

/*Get the current date and time*/
SELECT @currdate = getdate()
SELECT @currtime = RIGHT('0' + CONVERT(varchar, DATEPART(hour, @currdate)),2) 
		           + ':' +  RIGHT('0' + CONVERT(varchar, DATEPART(minute, @currdate)),2)

SELECT @currdate = CONVERT(datetime, CONVERT(char(8), @currdate, 112))

/*retrieve necessary data from gmnv*/
EXEC Castell_GetValue @tempgmnv, 'user', @user OUTPUT
EXEC Castell_EraseName @tempgmnv, 'user'
EXEC Castell_GetValue @tempgmnv, 'gmversion', @gmversion OUTPUT
EXEC Castell_EraseName @tempgmnv, 'gmversion'
EXEC Castell_GetValue @tempgmnv, 'emailupdate', @updateemail OUTPUT
EXEC Castell_EraseName @tempgmnv, 'emailupdate'

IF @user IS NULL
    BEGIN
        EXEC Castell_Delete @tempgmnv
        RETURN -130
    END

EXEC Castell_GetValue @tempgmnv, 'recid', @recid OUTPUT
EXEC Castell_EraseName @tempgmnv, 'recid'
IF @recid = ''
    SELECT @recid = NULL
EXEC Castell_GetValue @tempgmnv, 'nonusaphone', @nonusaphone OUTPUT
EXEC Castell_EraseName @tempgmnv, 'nonusaphone'
EXEC Castell_GetValue @tempgmnv, 'status', @status OUTPUT
EXEC Castell_GetValue @tempgmnv, 'email', @email OUTPUT
EXEC Castell_EraseName @tempgmnv, 'email'
EXEC Castell_GetValue @tempgmnv, 'website', @url OUTPUT
EXEC Castell_EraseName @tempgmnv, 'website'
EXEC Castell_GetValue @tempgmnv, 'notes', @notes OUTPUT
EXEC Castell_EraseName @tempgmnv, 'notes'

/*Format Phone Number*/
IF @recid IS NOT NULL AND @nonusaphone IS NULL AND (@status NOT LIKE 'U%' OR @status NOT LIKE 'I%')
    SELECT @nonusaphone = CASE WHEN status LIKE 'U%' THEN '0' ELSE '1' END
        FROM contact1 WHERE recid = @recid

SELECT @nonusaphone = CASE WHEN @status LIKE 'U%' THEN '0'
                           WHEN @status LIKE 'I%' THEN '1'
                           ELSE @nonusaphone
                      END

IF @nonusaphone = '0' OR @nonusaphone IS NULL
        BEGIN
            EXEC Castell_GetValue @tempgmnv, 'phone1', @tempphone OUTPUT
	        IF @tempphone IS NOT NULL 
                BEGIN
                    EXEC Castell_FormatPhone @tempphone OUTPUT
                    EXEC Castell_SetValue @tempgmnv, 'phone1', @tempphone
                END

            SELECT @tempphone = NULL
            EXEC Castell_GetValue @tempgmnv, 'phone2', @tempphone OUTPUT
	        IF @tempphone IS NOT NULL 
                BEGIN
                    EXEC Castell_FormatPhone @tempphone OUTPUT
                    EXEC Castell_SetValue @tempgmnv, 'phone2', @tempphone
                END

           SELECT @tempphone = NULL
           EXEC Castell_GetValue @tempgmnv, 'phone3', @tempphone OUTPUT
	        IF @tempphone IS NOT NULL 
                BEGIN
                    EXEC Castell_FormatPhone @tempphone OUTPUT
                    EXEC Castell_SetValue @tempgmnv, 'phone3', @tempphone
                END

           SELECT @tempphone = NULL
           EXEC Castell_GetValue @tempgmnv, 'fax', @tempphone OUTPUT
	        IF @tempphone IS NOT NULL 
                BEGIN
                    EXEC Castell_FormatPhone @tempphone OUTPUT
                    EXEC Castell_SetValue @tempgmnv, 'fax', @tempphone
                END
		       
        END

IF @recid IS NULL /*creating a new recod*/
    BEGIN
        /*Get a Recid */
        set @recid = (select dbo.Castell_RecId('MASTER'))

        /*populate status field based on @nonusaphone*/
        IF SUBSTRING(@status,1,1) NOT IN ('I','U') AND (@nonusaphone IS NULL OR @nonusaphone = '0')
            BEGIN
                SELECT @status = STUFF(@status, 1, 1, CASE @nonusaphone WHEN '0' THEN 'U'
                                                                        WHEN NULL THEN 'U'
                                                                        WHEN '1' THEN 'I' END)
                EXEC Castell_SetValue @tempgmnv, 'status', @status
            END
        
        /*populate company & contact.  need these now for accountno procedure later*/
        EXEC Castell_GetValue @tempgmnv, 'company', @company OUTPUT
        EXEC Castell_EraseName @tempgmnv, 'company'
        EXEC Castell_GetValue @tempgmnv, 'contact', @contact OUTPUT
        EXEC Castell_EraseName @tempgmnv, 'contact'

       /*Get an Accountno*/
        set @accountno = (select dbo.Castell_AccountNo())
        SELECT @insertstr = 'INSERT contact1 (accountno, company, contact, status, owner, phone1, zip, u_company, u_contact, u_lastname, u_city, u_key1, u_key2, u_key3, u_key4, u_key5, u_state, u_country, createby, createon, createat, recid)'
		SELECT @valuestr = ' VALUES(''' + @accountno + ''', ''' + REPLACE(isnull(@company, ' '), '''', '''''') + ''', ''' + REPLACE(isnull(@contact, ' '), '''', '''''') + ''', ''' + REPLACE(isnull(@status, 'U0'), '''', '''''') + ''', '' '', '' '', '' '', ''' + REPLACE(isnull(upper(@company),' '), '''', '''''') + ''',  ''' + REPLACE(isnull(upper(@contact),' '), '''', '''''') + ''', '' '', '' '', '' '', '' '', '' '', '' '', '' '', '' '', '' '', ''' + REPLACE(UPPER(@user), '''', '''''') + ''', ''' + CONVERT(varchar,@currdate) + ''', ''' + @currtime + ''', ' 
		+ 
		--REPLACE(@recid, '''', '''''') 
		CASE  --deal with recids with quote marks in them
					WHEN @recid LIKE '%''%' THEN '"' + @recid + '"'
					WHEN @recid LIKE '%"%'  THEN '''' + @recid + '''' 
					ELSE '''' + @recid + ''''
 				END
		+ 
		')'		
		--DEBUG 
		--print @insertstr + ' ' + @valuestr

        EXEC (@insertstr + ' ' + @valuestr)
         /*Update Tlogs*/
		--EXEC Castell_UpdateSyncLog 'contact1', @recid, @user, 'N'

    END

/* modifying fields of existing or newly created records*/
if exists (select * from sysobjects where id = object_id('#tempgmnv') and sysstat & 0xf = 4)
	drop table #tempgmnv

SELECT @namefield 'name', @valuefield 'value' INTO #tempgmnv
DELETE FROM #tempgmnv WHERE name IS NULL

INSERT INTO #tempgmnv
EXEC ('SELECT fieldname, value FROM ##gmnv' + @tempgmnv)

EXEC Castell_Delete @tempgmnv

----------------------NEW CODE BEGIN----------------------
DECLARE fieldcursor CURSOR 
FOR SELECT name, value FROM #tempgmnv

OPEN fieldcursor

FETCH NEXT FROM fieldcursor INTO @fieldname, @value

WHILE @@fetch_status = 0
	BEGIN
		SELECT @datatype=NULL, @tablename=NULL, @fieldlength=0

		SELECT @datatype=DATA_TYPE, @tablename=TABLE_NAME, @fieldlength=CHARACTER_MAXIMUM_LENGTH from INFORMATION_SCHEMA.COLUMNS where COLUMN_NAME = @fieldname and TABLE_NAME in ('CONTACT1', 'CONTACT2')
		
		--Some data cleanliness tasks
		IF UPPER(@fieldname) = 'OWNER'
			SELECT @value = UPPER(@value)
		
		--Check to see if field is logged to history
		IF @fieldname in (select field_name from CONTUDEF where FLDOPTS='1')
			begin
				set @accountno = (select accountno FROM contact1 WHERE recid = @recid)

				--if field is numeric, convert the value appropriately
				IF UPPER(@datatype) IN ('FLOAT', 'INT', 'REAL','NUMERIC','SMALLINT','TINYINT','DECIMAL')
					BEGIN
						set @oldvalquery = 'select @oldvalue=ltrim(rtrim(isnull(convert(nvarchar(18),convert(bigint,['+@fieldname+'])),''''))) from CONTACT1, CONTACT2 where contact1.ACCOUNTNO=contact2.ACCOUNTNO and contact1.recid='''+@recid+''''
					END
				ELSE
					BEGIN
						set @oldvalquery = 'select @oldvalue=ltrim(rtrim(isnull(['+@fieldname+'],''''))) from CONTACT1, CONTACT2 where contact1.ACCOUNTNO=contact2.ACCOUNTNO and contact1.recid='''+@recid+''''
					END
				
				Exec sp_executesql @oldvalquery, N'@oldvalue nvarchar(100) output', @oldvalue output
 				
 				--get field local label top result from fields5 and build ref with that.
 				set @fieldnamelocallabel=(select top 1 replace(replace(replace(replace(label,'"',''),'''',''),'+',''),'%','') fldlabel  from FIELDS5 where RECTYPE='F' and fldname=@fieldname order by recid)
 				
 				if @oldvalue <> @value
 					BEGIN
						set @reflogupdate = left('Updated: '+@fieldnamelocallabel+' ('+@fieldname+'): '''+@oldvalue+''' to: '''+@value+'''',80)
						
						--Create history item indicating logging
						EXEC Castell_Create @gmnvlog OUTPUT

						EXEC Castell_SetValue @gmnvlog, 'accountno', @accountno
						EXEC Castell_SetValue @gmnvlog, 'rectype', 'O'
						EXEC Castell_SetValue @gmnvlog, 'user', @user
						EXEC Castell_SetValue @gmnvlog, 'ref', @reflogupdate
						EXEC Castell_SetValue @gmnvlog, 'resultcode', 'LOG'

						EXEC Castell_WriteHistory @gmnvlog
						EXEC dbo.Castell_Delete @gmnvlog
					END
			END
					
		IF UPPER(@tablename) = 'CONTACT1' and upper(@fieldname) <> 'RECID' and upper(@fieldname) <> 'NOTES'
			BEGIN
				SELECT @value=REPLACE(isnull(@value, ' '), '''', '''''')

				SELECT @value=left(@value, @fieldlength)
				
				SELECT @setcmd1 = ' [' + @fieldname + '] = ' 
				+
				CASE  --deal with data types that require quotes versus those that don't.  Also deal with NULL
					WHEN @value IS NULL
						THEN 'NULL' 
					WHEN UPPER(@datatype) NOT IN ('FLOAT', 'INT', 'REAL','NUMERIC','SMALLINT','TINYINT','DECIMAL')
						THEN '"' + REPLACE(@value, '"', '""') + '"'
					ELSE @value 
				END 
				+
				CASE --deal with u_ uppercase shadow fields
					WHEN UPPER(@fieldname) IN ('COMPANY','CONTACT','LASTNAME','CITY','KEY1','KEY2','KEY3','KEY4','KEY5','STATE','COUNTRY') AND @value IS NOT NULL 
						THEN ', u_' + @fieldname + ' = ''' + UPPER(REPLACE(@value, '"', '""')) + ''''
					WHEN UPPER(@fieldname) IN ('COMPANY','CONTACT','LASTNAME','CITY','KEY1','KEY2','KEY3','KEY4','KEY5','STATE','COUNTRY') AND @value IS NULL 
						THEN ', u_' + @fieldname + ' = ''''' 
					ELSE '' 
				END
				+
		 		' WHERE recid = ' 
				+ 
				CASE  --deal with recids with quote marks in them
					WHEN @recid LIKE '%''%' THEN '"' + @recid + '"'
					WHEN @recid LIKE '%"%'  THEN '''' + @recid + '''' 
					ELSE '''' + @recid + ''''
 				END

				--DEBUG 
				--print 'UPDATE contact1 SET ' + @setcmd1

				EXEC('UPDATE contact1 SET ' + @setcmd1)

				--EXEC Castell_UpdateSyncLog @tablename, @recid, @user, 'U', @fieldname
			END

        IF UPPER(@tablename) = 'CONTACT2'
            BEGIN
                /*this block of accountno and recid steps should only occur once at the most*/
                set @accountno = (select accountno FROM contact1 WHERE recid = @recid)

                IF @c2recid IS NULL
                    SELECT @C2recid = recid FROM contact2 WHERE accountno = @accountno

                IF @c2recid IS /*STILL*/ NULL --there is no contact2 record for this contact
                    BEGIN
                        /*get a recid*/
                        set @c2recid = (select dbo.Castell_RecId('MASTER'))
                        INSERT contact2 (accountno, recid) VALUES (@accountno, @c2recid)
                        
						--EXEC Castell_UpdateSyncLog 'contact2', @c2recid, @user, 'N'
                    END

				IF UPPER(@fieldname) <> 'RECID'
				  BEGIN      
					SELECT @setcmd1 = ' [' + @fieldname + '] = '
					+ 
					CASE 
						WHEN @value IS NULL THEN 'NULL'
						WHEN UPPER(@datatype) NOT IN ('FLOAT', 'INT', 'REAL','NUMERIC','SMALLINT','TINYINT','DECIMAL')
							THEN '"' + REPLACE( @value,'"','""') + '"'
						ELSE @value 
					END
					+
					' WHERE recid = ' 
					+ 
					CASE  --deal with recids with quote marks in them
						WHEN @c2recid LIKE '%''%' THEN '"' + @c2recid + '"'
						WHEN @c2recid LIKE '%"%'  THEN '''' + @c2recid + '''' 
						ELSE '''' + @c2recid + ''''
 					END

					EXEC('UPDATE contact2 SET ' + @setcmd1)

					--EXEC Castell_UpdateSyncLog 'contact2', @c2recid, @user, 'U', @fieldname
				  END
            END --contact2 block


		FETCH NEXT FROM fieldcursor INTO @fieldname, @value
	END
CLOSE fieldcursor
DEALLOCATE fieldcursor

----------------------NEW CODE END----------------------

If @notes is not NULL
	BEGIN
		set @nrecid = (select dbo.Castell_RecId('MASTER'))
		INSERT INTO NOTES (CREATEDDATE,USERID,MODIFIEDDATE,MODIFIEDBY,LOPRECID,ACCOUNTNO,NOTE,RECTYPE,EXT,recid)
		VALUES (getdate(),@user,getdate(),@user,@recid,@accountno,@notes,'C1','html',@nrecid)
		--EXEC Castell_UpdateSyncLog 'notes', @nrecid, @user, 'N'
	END

UPDATE contact1 SET lastdate = @currdate, lasttime = @currtime, lastuser = UPPER(@user) WHERE accountno = @accountno

--EXEC Castell_UpdateSyncLog 'contact1', @recid, @user, 'U', 'lastuser'
--EXEC Castell_UpdateSyncLog 'contact1', @recid, @user, 'U', 'lastdate'
--EXEC Castell_UpdateSyncLog 'contact1', @recid, @user, 'U', 'lasttime'

EXEC Castell_SetValue @gmnv, 'recid', @recid

/*handle email and url*/
IF @email IS NOT NULL 
    BEGIN
        EXEC Castell_Create @tempgmnv OUTPUT

        IF @accountno IS NULL OR @accountno = ''
            SELECT @accountno = accountno, @contact = COALESCE(@contact, contact) FROM contact1 WHERE recid = @recid

        IF @email IS NOT NULL
            BEGIN
                SELECT @c2recid = NULL
                SELECT @c2recid = recid FROM contsupp WHERE accountno = @accountno AND
                    rectype = 'P' AND u_contact = 'E-MAIL ADDRESS' --AND SUBSTRING(zip, 2, 1) = '1'

                IF @c2recid IS NOT NULL
                    BEGIN
                        --IF @updateemail = '1'
                           EXEC Castell_SetValue @tempgmnv, 'recid', @c2recid
                        --ELSE
                        --    BEGIN
                        --        UPDATE contsupp 
                        --            SET zip = STUFF(zip, 2, 1, '0') WHERE recid = @c2recid
                        --        EXEC Castell_UpdateSyncLog 'contsupp', @c2recid, @user, 'U', 'zip'
                        --    END
                     END
            END

		select @sub_contact=substring(@contact,0,20)

        EXEC Castell_SetValue @tempgmnv, 'accountno', @accountno
        EXEC Castell_SetValue @tempgmnv, 'ufield2', @recid
        EXEC Castell_SetValue @tempgmnv, 'ufield8', @sub_contact
        EXEC Castell_SetValue @tempgmnv, 'status', '00'
        EXEC Castell_SetValue @tempgmnv, 'ufield4', '0100'
        EXEC Castell_SetValue @tempgmnv, 'ufield6', '011'
        EXEC Castell_SetValue @tempgmnv, 'user', @user
        SELECT @valuefield = NULL
        EXEC Castell_GetValue @gmnv, 'caldb', @valuefield OUTPUT
        EXEC Castell_SetValue @tempgmnv, 'caldb', @valuefield
    

        BEGIN
            EXEC Castell_SetValue @tempgmnv, 'detail', 'E-mail Address'
            EXEC Castell_SetValue @tempgmnv, 'ref', @email
            EXEC Castell_WriteDetail @tempgmnv
			EXEC Castell_EraseName @tempgmnv, 'recid'
        END

        EXEC Castell_Delete @tempgmnv
    END

IF @url IS NOT NULL
	BEGIN
		EXEC Castell_Create @tempgmnv OUTPUT

        IF @accountno IS NULL OR @accountno = ''
            SELECT @accountno = accountno, @contact = COALESCE(@contact, contact) FROM contact1 WHERE recid = @recid
        
		EXEC Castell_SetValue @tempgmnv, 'accountno', @accountno
        EXEC Castell_SetValue @tempgmnv, 'ufield4', '0100'
        EXEC Castell_SetValue @tempgmnv, 'user', @user
        SELECT @valuefield = NULL
        EXEC Castell_GetValue @gmnv, 'caldb', @valuefield OUTPUT
        EXEC Castell_SetValue @tempgmnv, 'caldb', @valuefield

		BEGIN
            EXEC Castell_SetValue @tempgmnv, 'detail', 'Web Site'
            EXEC Castell_SetValue @tempgmnv, 'ref', @url
            EXEC Castell_WriteDetail @tempgmnv
			EXEC Castell_EraseName @tempgmnv, 'recid'
        END
        EXEC Castell_Delete @tempgmnv
	END
	
	
	SET ANSI_NULLS ON











GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteContactNotes]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO









CREATE PROCEDURE [dbo].[Castell_WriteContactNotes] (@gmnv varchar(20))
    
AS
DECLARE @recid     varchar(15),
		@c1recid   varchar(15),
		@accountno varchar(20),
		@notes     varchar(8000),
		@user      varchar(8),
		@valid     int,
		@createddate datetime
		

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
    RETURN -120

EXEC Castell_GetValue @gmnv, 'user', @user OUTPUT

IF @user IS NULL
    RETURN -130

EXEC Castell_GetValue @gmnv, 'accountno', @accountno OUTPUT

IF @accountno IS NULL
    RETURN -170

EXEC Castell_GetValue @gmnv, 'notes', @notes OUTPUT

EXEC Castell_GetValue @gmnv, 'recid', @recid OUTPUT

set @createddate = GETDATE()

EXEC Castell_GetValue @gmnv, 'createddate', @createddate OUTPUT


if @recid is null --make a new note
		BEGIN
			If @notes is not NULL
				BEGIN
					set @recid = (select dbo.Castell_RecId('MASTER'))
					set @c1recid = (select recid from CONTACT1 where ACCOUNTNO=@accountno)
					INSERT INTO NOTES (CREATEDDATE,USERID,MODIFIEDDATE,MODIFIEDBY,LOPRECID,ACCOUNTNO,NOTE,RECTYPE,EXT,recid)
					VALUES (@createddate,@user,@createddate,@user,@c1recid,@accountno,@notes,'C1','html',@recid)
					EXEC Castell_UpdateSyncLog 'notes', @recid, @user, 'N'
				END
		END
else
		begin
				UPDATE notes SET note = @notes, MODIFIEDBY=@user, MODIFIEDDATE=GETDATE()  WHERE recid = @recid
		end

		











GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteContsupp]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO









CREATE PROCEDURE [dbo].[Castell_WriteContsupp] (@gmnv varchar(20))
   
AS
/*returns -120 bad gmnv
          -150 if no accountno passed on a new record
          -140 wrong version of sql
          -160 bad rectype
          -130 Required parameter userid not passed */

/*Declarations*/
DECLARE @currdate datetime,
		@currtime varchar(5),
		@fieldname varchar(50),
		@recid varchar(15),
		@accountno varchar(20),
		@rectype varchar(1),
		@name varchar(20),
		@value varchar(8000),
		@datatype varchar(20),
		@setcmd1 varchar(max),
		@userid varchar(8),
		@namefield varchar(20),
		@valuefield varchar(8000),
		@notesptr binary(16),
		@valid int

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
            RETURN -120

/*Get the current date*/
SELECT @currdate = getdate()

/*get the current date*/
SELECT @currtime = RIGHT('0' + CONVERT(varchar, DATEPART(hour,@currdate)),2) + ':' + RIGHT('0' + CONVERT(varchar, DATEPART(minute, @currdate)),2)

/*make the current date 12:00am*/
SELECT @currdate = CONVERT(datetime, CONVERT(char(8), @currdate, 112))

SELECT @namefield 'name', @valuefield 'value' INTO #tempgmnv
DELETE FROM #tempgmnv

INSERT INTO #tempgmnv
EXEC ('SELECT fieldname, value FROM ##gmnv' + @gmnv)

/*get the userid and place it in the sync gmnv*/
SELECT @userid = value FROM  #tempgmnv WHERE UPPER(name) = 'USER'
IF @userid IS NULL
    RETURN -130

/*read if recid is provided*/
SELECT @recid = value FROM #tempgmnv WHERE UPPER(name) = 'RECID'
DELETE FROM #tempgmnv WHERE UPPER(name) IN ('RECID', 'USER')
IF @recid = ''
    SELECT @recid = NULL

IF @recid IS NULL /*create a new recod*/
    BEGIN
        SELECT @accountno = value FROM #tempgmnv WHERE UPPER(name) = 'ACCOUNTNO'
        SELECT @rectype = value FROM #tempgmnv WHERE UPPER(name) = 'RECTYPE'

        /*validate the rectype passed*/
        IF @rectype NOT IN ('C','E','H','L','O','P','R','A')
            RETURN -160
        
        IF @accountno IS NULL
            RETURN -150
        
        DELETE FROM #tempgmnv WHERE UPPER(@name) IN ('ACCOUNTNO','RECTYPE')

        /*Get a Recid */
        set @recid = (select dbo.Castell_RecId('MASTER'))
        EXEC Castell_SetValue @gmnv, 'recid', @recid

        INSERT contsupp (accountno, rectype, u_contact, u_contsupref, u_address1, status, recid)
            VALUES (@accountno, @rectype, '', '', '', '00', @recid)

        /*update sync info*/
        --EXEC Castell_UpdateSyncLog 'contsupp', @recid, @userid, 'N'
    END

------------------------------NEW FIELD CODE BEGIN-------------------------------------

DECLARE fieldcursor CURSOR 
FOR SELECT name, value FROM #tempgmnv

OPEN fieldcursor

FETCH NEXT FROM fieldcursor INTO @fieldname, @value

WHILE @@fetch_status = 0
	BEGIN
		SELECT @datatype=NULL
		SELECT @datatype=DATA_TYPE from INFORMATION_SCHEMA.COLUMNS where COLUMN_NAME = @fieldname and TABLE_NAME = 'CONTSUPP'

		IF upper(@fieldname) not in ('RECID', 'STATUS', 'RECTYPE') and @datatype is not NULL
			BEGIN
				SELECT @setcmd1 = ' [' + @fieldname + '] = ' 
				+
				CASE  --deal with data types that require quotes versus those that don't.  Also deal with NULL
					WHEN @value IS NULL
						THEN 'NULL' 
					WHEN UPPER(@datatype) NOT IN ('FLOAT', 'INT', 'REAL','NUMERIC','SMALLINT','TINYINT','DECIMAL')
						THEN '"' + REPLACE(@value, '"', '""') + '"'
					ELSE @value 
				END 
				+
				CASE  --deal with u_ uppercase shadow fields
					WHEN UPPER(@fieldname) IN ('CONTACT','CONTSUPREF','ADDRESS1') AND @value IS NOT NULL THEN ', u_' + @fieldname + ' = ''' + UPPER(REPLACE(@value, '"', '""')) + ''''
					WHEN UPPER(@fieldname) IN ('CONTACT','CONTSUPREF','ADDRESS1') AND @value IS NULL THEN ', u_' + @fieldname + ' = ''''' 
					ELSE '' 
				END
				+ 
				' WHERE recid = ' 
				+ 
				CASE  --deal with recids with quote marks in them
					WHEN @recid LIKE '%''%' THEN '"' + @recid + '"'
					WHEN @recid LIKE '%"%'  THEN '''' + @recid + '''' 
					ELSE '''' + @recid + ''''
 				END

				EXEC('UPDATE CONTSUPP SET ' + @setcmd1)

				--EXEC Castell_UpdateSyncLog 'CONTSUPP', @recid, @userid, 'U', @fieldname
			END --most fields

		FETCH NEXT FROM fieldcursor INTO @fieldname, @value
	END
CLOSE fieldcursor
DEALLOCATE fieldcursor

------------------------------NEW FIELD CODE END-------------------------------------

UPDATE contsupp SET lastuser = UPPER(@userid), lastdate = @currdate, lasttime = @currtime WHERE recid = @recid

EXEC Castell_SetValue @gmnv, 'recid', @recid

--EXEC Castell_UpdateSyncLog 'contsupp', @recid, @userid, 'U', 'lastuser'
--EXEC Castell_UpdateSyncLog 'contsupp', @recid, @userid, 'U', 'lastdate'
--EXEC Castell_UpdateSyncLog 'contsupp', @recid, @userid, 'U', 'lasttime'







GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteDetail]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO











CREATE PROCEDURE [dbo].[Castell_WriteDetail] (@gmnv varchar(20))
    
AS

DECLARE @currdate datetime
DECLARE @currtime varchar(5)
DECLARE @profiledate varchar(30)
DECLARE @recid varchar(15)
DECLARE @profile varchar(30)
DECLARE @lookup varchar(20)
DECLARE @user varchar(8)
DECLARE @tempgmnv varchar(20)
DECLARE @caldb varchar(123)
DECLARE @tempsql varchar(1000)
DECLARE @tempvalue varchar(8000)

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
            RETURN -120

EXEC Castell_GetValue @gmnv, 'user', @user OUTPUT
IF @user IS NULL
    RETURN -130

EXEC Castell_CreateCopy @gmnv, @tempgmnv OUTPUT

EXEC Castell_GetValue @tempgmnv, 'recid', @recid OUTPUT

/*get the current date*/
IF @recid IS NULL
    BEGIN
        SELECT @currdate = getdate()
        SELECT @currtime = RIGHT('0' + CONVERT(varchar, DATEPART(hour,@currdate)),2) + ':' + 
			               RIGHT('0' + CONVERT(varchar, DATEPART(minute, @currdate)),2)
       
        SELECT @profiledate = SUBSTRING(UPPER(@user) + space(8), 1 , 8) + DATENAME(year, @currdate) + 
                                  RIGHT('0' + CONVERT(varchar,DATEPART(month, @currdate)), 2) +
                                  RIGHT('0' + CONVERT(varchar,DATEPART(day, @currdate)),2) + ' ' +
                                  CASE
                                    WHEN CONVERT(int, SUBSTRING(@currtime,1,(PATINDEX('%:%',@currtime) - 1))) > 12 
						                THEN CONVERT(varchar,(CONVERT(int, SUBSTRING(@currtime,1,(PATINDEX('%:%',@currtime) - 1))) - 12)) + 
							                  SUBSTRING(@currtime,PATINDEX('%:%',@currtime),3) + 'pm '
                                    WHEN CONVERT(int, SUBSTRING(@currtime,1,(PATINDEX('%:%',@currtime) - 1))) = 12
									    THEN @currtime + 'pm '
 				                    ELSE @currtime + 'am '
                                  END  
         EXEC Castell_SetValue @tempgmnv, 'city', @profiledate
    END

/*map the profile fields to the contsupp fields*/
EXEC Castell_ChangeName @tempgmnv, 'DETAIL', 'CONTACT'
EXEC Castell_ChangeName @tempgmnv, 'REF', 'CONTSUPREF'
EXEC Castell_ChangeName @tempgmnv, 'UFIELD1', 'TITLE'
EXEC Castell_ChangeName @tempgmnv, 'UFIELD2', 'LINKACCT'
EXEC Castell_ChangeName @tempgmnv, 'UFIELD3', 'COUNTRY'
EXEC Castell_ChangeName @tempgmnv, 'UFIELD4', 'DEAR'
EXEC Castell_ChangeName @tempgmnv, 'UFIELD5', 'FAX'
EXEC Castell_Changename @tempgmnv, 'UFIELD6', 'ZIP'
EXEC Castell_ChangeName @tempgmnv, 'UFIELD7', 'EXT'
EXEC Castell_Changename @tempgmnv, 'UFIELD8', 'STATE'
EXEC Castell_Changename @tempgmnv, 'UFIELD9', 'MERGECODES'
EXEC Castell_Changename @tempgmnv, 'UFIELD10', 'ADDRESS1'
EXEC Castell_Changename @tempgmnv, 'UFIELD11', 'ADDRESS2'
EXEC Castell_Changename @tempgmnv, 'UFIELD12', 'ADDRESS3'

EXEC Castell_SetValue @tempgmnv, 'rectype', 'P'

EXEC Castell_GetValue @tempgmnv, 'contact', @profile OUTPUT

IF UPPER(@profile) = 'E-MAIL ADDRESS'
    BEGIN
        EXEC Castell_SetValue @tempgmnv, 'contact', 'E-mail Address'
        SELECT @tempvalue = NULL
        EXEC Castell_GetValue @tempgmnv, 'contsupref', @tempvalue OUTPUT
        IF LEN(@tempvalue) > 35
            SELECT @tempvalue = RIGHT(@tempvalue, LEN(@tempvalue) - 35)
        ELSE
            SELECT @tempvalue = ' '

        EXEC Castell_SetValue @tempgmnv, 'ADDRESS1', @tempvalue
        
        EXEC Castell_GetValue @tempgmnv, 'contsupref', @tempvalue OUTPUT
		SELECT @tempvalue=left(@tempvalue,35)
        EXEC Castell_EraseName @tempgmnv, 'CONTSUPREF'
		EXEC Castell_SetValue @tempgmnv, 'CONTSUPREF', @tempvalue
				
    END
IF UPPER(@profile) = 'WEB SITE'
    BEGIN
		EXEC Castell_SetValue @tempgmnv, 'contact', 'Web site'
        SELECT @tempvalue = NULL
        EXEC Castell_GetValue @tempgmnv, 'contsupref', @tempvalue OUTPUT
        IF LEN(@tempvalue) > 35
            BEGIN
				SELECT @tempvalue='~~REF=' + @tempvalue + char(13) + char(10)
                EXEC Castell_SetValue @tempgmnv, 'NOTES', @tempvalue
				EXEC Castell_GetValue @tempgmnv, 'contsupref', @tempvalue OUTPUT
				SELECT @tempvalue=left(@tempvalue,35)
                EXEC Castell_EraseName @tempgmnv, 'CONTSUPREF'
				EXEC Castell_SetValue @tempgmnv, 'CONTSUPREF', @tempvalue
            END
    END
              
EXEC Castell_GetValue @tempgmnv, 'caldb', @caldb OUTPUT
EXEC Castell_EraseName @tempgmnv, 'caldb'
SELECT @caldb = @caldb + CASE WHEN @caldb IS NOT NULL OR @caldb <> ' ' THEN '..'
                         ELSE '' END

SELECT @lookup 'lookup' INTO #templookup
DELETE FROM #templookup

SELECT @tempsql = 'SELECT lookupsupp FROM ' + ISNULL(@caldb, ' ') + 'lookup WHERE UPPER(fieldname) = ''MPROFHEAD V'' AND u_entry = ''' + UPPER(@profile) + ''''

INSERT INTO #templookup
EXEC (@tempsql)

SELECT @lookup = lookup FROM #templookup

EXEC Castell_SetValue @tempgmnv, 'phone', @lookup

EXEC Castell_WriteContsupp @tempgmnv

EXEC Castell_GetValue @tempgmnv, 'recid', @recid OUTPUT
EXEC Castell_SetValue @gmnv, 'recid', @recid  --return the recid to the calling gmnv

EXEC Castell_Delete @tempgmnv











GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteGroup]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO










CREATE PROCEDURE [dbo].[Castell_WriteGroup] (@gmnv varchar(20))
   
AS
/*returns -120 bad gmnv
          -130 Required parameter userid not passed */

/*Declarations*/
DECLARE @code varchar(8),
		@recid varchar(15),
		@accountno varchar(20),
		@name varchar(20),
		@groupname varchar(24),
		@value varchar(8000),
		@datatype varchar(20),
		@setcmd1 varchar(max),
		@userid varchar(8),
		@fieldname varchar(50),
		@namefield varchar(20),
		@valuefield varchar(8000),
		@valid int,
		@sync int,
		@membercount int,
		@padmembers varchar(20),
		@syncflagpad int

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
            RETURN -120

SELECT @namefield 'name', @valuefield 'value' INTO #tempgmnv
DELETE FROM #tempgmnv

Exec Castell_ChangeName @gmnv, 'NAME', 'REF'

INSERT INTO #tempgmnv
EXEC ('SELECT fieldname, value FROM ##gmnv' + @gmnv)

/*get the userid and place it in the sync gmnv*/
SELECT @userid = value FROM  #tempgmnv WHERE UPPER(name) = 'USER'
IF @userid IS NULL
    RETURN -130

/*read if recid is provided*/
SELECT @recid = value FROM #tempgmnv WHERE UPPER(name) = 'RECID'
DELETE FROM #tempgmnv WHERE UPPER(name) IN ('RECID', 'USER')
IF @recid = ''
    SELECT @recid = NULL

IF @recid IS NOT NULL /*update existing group*/
    BEGIN
		--default value for group accountno: '*M                 0'
		SELECT @accountno = '*M'
		set @syncflagpad = 18
		SELECT @sync = value FROM #tempgmnv WHERE UPPER(name) = 'SYNC'
        if @sync=1
			BEGIN
				SELECT @accountno = @accountno + 'S'
				set @syncflagpad = 17
			END
		select @membercount = (select count(*) from contgrps where accountno not like '*M%' and userid=@recid)
		select @padmembers = right('                    ' + ltrim(rtrim(str(@membercount))), @syncflagpad)
		SELECT @accountno = @accountno + @padmembers

    END -- @Recid value passed

IF @recid IS NULL /*create a new Group record*/
    BEGIN
		--default value for group accountno: '*M                 0'
		SELECT @accountno = '*M                 0'
		SELECT @sync = value FROM #tempgmnv WHERE UPPER(name) = 'SYNC'
        if @sync=1
			BEGIN
				SELECT @accountno = '*MS                0'
			END
        SELECT @code = value FROM #tempgmnv WHERE UPPER(name) = 'CODE'
		SELECT @groupname = value FROM #tempgmnv WHERE UPPER(name) = 'REF'

        DELETE FROM #tempgmnv WHERE UPPER(@name) IN ('CODE','REF','SYNC')

        /*Get a Recid */
        set @recid = (select dbo.Castell_RecId('MASTER'))
        EXEC Castell_SetValue @gmnv, 'recid', @recid

        INSERT contgrps (userid, code, accountno, ref, u_code, recid)
            VALUES (@userid, @code, @accountno, @groupname, upper(@code), @recid)

        /*update sync info*/
        EXEC Castell_UpdateSyncLog 'contgrps', @recid, @userid, 'N'
    END -- NULL @Recid


DECLARE fieldcursor CURSOR 
FOR SELECT name, value FROM #tempgmnv

OPEN fieldcursor

FETCH NEXT FROM fieldcursor INTO @fieldname, @value

WHILE @@fetch_status = 0
	BEGIN
		SELECT @datatype=NULL
		SELECT @datatype=DATA_TYPE from INFORMATION_SCHEMA.COLUMNS where COLUMN_NAME = @fieldname and TABLE_NAME = 'CONTGRPS'

		IF upper(@fieldname) not in ('RECID') and @datatype is not NULL
		BEGIN
			SELECT @setcmd1 = ' [' + @fieldname + '] = ' 
			+
			CASE  --deal with data types that require quotes versus those that don't.  Also deal with NULL
				WHEN UPPER(@datatype) NOT IN ('FLOAT', 'INT', 'REAL','NUMERIC','SMALLINT','TINYINT','DECIMAL') AND @value IS NOT NULL THEN '"' + @value + '"'
				WHEN @value IS NULL THEN 'NULL' 
				ELSE @value 
			END 
			+
			CASE 
				WHEN UPPER(@fieldname) IN ('CODE') AND @value IS NOT NULL THEN ', u_' + @fieldname + ' = ''' + UPPER(@value) + ''''
				WHEN UPPER(@fieldname) IN ('CODE') AND @value IS NULL THEN ', u_' + @fieldname + ' = ''''' 
				ELSE '' 
			END
			+ 
			' WHERE recid = ' 
			+ 
			CASE  --deal with recids with quote marks in them
				WHEN @recid LIKE '%''%' THEN '"' + @recid + '"'
				WHEN @recid LIKE '%"%'  THEN '''' + @recid + '''' 
				ELSE '''' + @recid + ''''
 			END

			EXEC('UPDATE CONTGRPS SET ' + @setcmd1)

			EXEC Castell_UpdateSyncLog 'CONTGRPS', @recid, @userid, 'U', @fieldname
		END --most fields

		FETCH NEXT FROM fieldcursor INTO @fieldname, @value
	END
CLOSE fieldcursor
DEALLOCATE fieldcursor

EXEC Castell_SetValue @gmnv, 'recid', @recid









GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteGroupMember]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO










CREATE PROCEDURE [dbo].[Castell_WriteGroupMember] (@gmnv varchar(20))
   
AS
/*returns -120 bad gmnv
          -130 Required parameter userid not passed
		  -140 Required parameter grouprecid not passed 
		  -150 Required contact match parameter not passed (accountno of new member or recid of existing member)*/

DECLARE @code varchar(8),
		@recid varchar(15),
		@grouprecid varchar(15),
		@ref varchar(24),
		@accountno varchar(20),
		@groupaccountno varchar(20),
		@name varchar(20),
		@value varchar(8000),
		@datatype varchar(20),
		@setcmd1 varchar(max),
		@userid varchar(8),
		@fieldname varchar(50),
		@namefield varchar(20),
		@valuefield varchar(8000),
		@valid int,
		@sync int,
		@membercount int,
		@padmembers varchar(20),
		@syncflagpad int,
		@groupgmvn varchar(20)

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
            RETURN -120

SELECT @namefield 'name', @valuefield 'value' INTO #tempgmnv
DELETE FROM #tempgmnv

INSERT INTO #tempgmnv
EXEC ('SELECT fieldname, value FROM ##gmnv' + @gmnv)

/*get the userid and place it in the sync gmnv*/
SELECT @userid = value FROM  #tempgmnv WHERE UPPER(name) = 'USER'
IF @userid IS NULL
    RETURN -130

SELECT @grouprecid = value FROM #tempgmnv WHERE UPPER(name) = 'GROUPRECID'
IF @grouprecid IS NULL
    RETURN -140

Exec Castell_ChangeName @gmnv, 'GROUPRECID', 'USERID'

/*read if recid is provided*/
SELECT @recid = value FROM #tempgmnv WHERE UPPER(name) = 'RECID'
DELETE FROM #tempgmnv WHERE UPPER(name) IN ('RECID', 'USER')
IF @recid = ''
    SELECT @recid = NULL

--create/update a new Group member record match on recid (first) and fall back to accountno matching if necessary

SELECT @accountno = value FROM #tempgmnv WHERE UPPER(name) = 'ACCOUNTNO'
SELECT @code = value FROM #tempgmnv WHERE UPPER(name) = 'CODE'
SELECT @ref = value FROM #tempgmnv WHERE UPPER(name) = 'REF'

IF @recid IS NULL --see if this accountno is in this group already
	BEGIN
		select @recid = (select recid from contgrps where userid=@grouprecid and accountno=@accountno)
	END

if @recid is not null and @accountno is null
	BEGIN
		SELECT @accountno = (select accountno from contgrps where recid=@recid)
		update #tempgmnv set value = @accountno where upper(name) = 'ACCOUNTNO'
	END

if @recid is null and @accountno is null
	RETURN -150
--DELETE FROM #tempgmnv WHERE UPPER(@name) IN ('CODE','REF','ACCOUNTNO')

if @recid is NULL
	BEGIN
		/*Get a Recid */
		set @recid = (select dbo.Castell_RecId('MASTER'))
		EXEC Castell_SetValue @gmnv, 'recid', @recid

		INSERT contgrps (userid, code, accountno, ref, u_code, recid)
			VALUES (@grouprecid, @code, @accountno, @ref, upper(@code), @recid)

		/*update sync info*/
		--EXEC Castell_UpdateSyncLog 'contgrps', @recid, @userid, 'N'
	END

--Update the group header record
SELECT @groupaccountno = '*M'
set @syncflagpad = 18
select @membercount = (select count(*) from contgrps where accountno not like '*M%' and userid=@grouprecid)
select @padmembers = right('                    ' + ltrim(rtrim(str(@membercount))), 18)
SELECT @groupaccountno = @groupaccountno + @padmembers

update contgrps set accountno=@groupaccountno where recid=@grouprecid

/*update sync info*/
--EXEC Castell_UpdateSyncLog 'contgrps', @grouprecid, @userid, 'U'

DECLARE fieldcursor CURSOR 
FOR SELECT name, value FROM #tempgmnv

OPEN fieldcursor

FETCH NEXT FROM fieldcursor INTO @fieldname, @value

WHILE @@fetch_status = 0
	BEGIN
		SELECT @datatype=NULL
		SELECT @datatype=DATA_TYPE from INFORMATION_SCHEMA.COLUMNS where COLUMN_NAME = @fieldname and TABLE_NAME = 'CONTGRPS'

		IF upper(@fieldname) not in ('RECID') and @datatype is not NULL
		BEGIN
			SELECT @setcmd1 = ' [' + @fieldname + '] = ' 
			+
			CASE  --deal with data types that require quotes versus those that don't.  Also deal with NULL
				WHEN UPPER(@datatype) NOT IN ('FLOAT', 'INT', 'REAL','NUMERIC','SMALLINT','TINYINT','DECIMAL') AND @value IS NOT NULL THEN '"' + @value + '"'
				WHEN @value IS NULL THEN 'NULL' 
				ELSE @value 
			END 
			+
			CASE 
				WHEN UPPER(@fieldname) IN ('CODE') AND @value IS NOT NULL THEN ', u_' + @fieldname + ' = ''' + UPPER(@value) + ''''
				WHEN UPPER(@fieldname) IN ('CODE') AND @value IS NULL THEN ', u_' + @fieldname + ' = ''''' 
				ELSE '' 
			END
			+ 
			' WHERE recid = ' 
			+ 
			CASE  --deal with recids with quote marks in them
				WHEN @recid LIKE '%''%' THEN '"' + @recid + '"'
				WHEN @recid LIKE '%"%'  THEN '''' + @recid + '''' 
				ELSE '''' + @recid + ''''
 			END

			EXEC('UPDATE CONTGRPS SET ' + @setcmd1)

			--EXEC Castell_UpdateSyncLog 'CONTGRPS', @recid, @userid, 'U', @fieldname
		END --most fields

		FETCH NEXT FROM fieldcursor INTO @fieldname, @value
	END
CLOSE fieldcursor
DEALLOCATE fieldcursor

EXEC Castell_SetValue @gmnv, 'recid', @recid









GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteHistory]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO






CREATE PROCEDURE [dbo].[Castell_WriteHistory](@gmnv varchar(20))

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
		@value varchar(8000),
		@datatype varchar(20),
		@tempgmnv varchar(15),
		@rsvpgmnv varchar(15),
		@namefield varchar(20),
		@valuefield varchar(8000),
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

/****** Object:  StoredProcedure [dbo].[Castell_WriteLinkedDoc]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO








CREATE PROCEDURE [dbo].[Castell_WriteLinkedDoc] (@gmnv varchar(20))
                                                                                   
AS

/*Declarations*/
DECLARE @currdate datetime,
		@currtime varchar(5),
		@owner varchar(8),
		@contact varchar(30),
		@title varchar(35),
		@dear varchar(20),
		@extension varchar(20),
		@sDate varchar(12),
		@address1 varchar(40),
		@address2 varchar(40),
		@city varchar(30),
		@new varchar(1),
		@filename varchar(255),
		@user varchar(8),
		@syncfile varchar(1),
		@recid varchar(15),
		@iFileNameStart int,
		@linkeddoctemp varchar(255),
		@linkeddocptr varbinary(16),
		@tempgmnv varchar(20)

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
            RETURN -120

EXEC Castell_GetValue @gmnv, 'user', @user OUTPUT
IF @user IS NULL
    RETURN -130

/*create the temp gmnv & transfer data*/
EXEC Castell_CreateCopy @gmnv, @tempgmnv OUTPUT

/*pull out necessary data for processing*/
EXEC Castell_GetValue @tempgmnv, 'recid', @recid OUTPUT

EXEC Castell_GetValue @tempgmnv, 'syncfile', @syncfile OUTPUT
EXEC Castell_EraseName @tempgmnv, 'syncfile'

EXEC Castell_GetValue @tempgmnv, 'owner', @owner OUTPUT
EXEC Castell_EraseName @tempgmnv, 'owner'
if @owner is null
	set @owner=''

EXEC Castell_ChangeName @tempgmnv, 'MBRECID', 'LINKRECID'
EXEC Castell_ChangeName @tempgmnv, 'REF', 'CONTSUPREF'

EXEC Castell_GetValue @tempgmnv, 'DocDate', @currdate OUTPUT
EXEC Castell_EraseName @tempgmnv, 'DocDate'

/*Get the current date*/
if @currdate is null
	SELECT @currdate = getdate()

/*get the current date*/
SELECT @currtime = RIGHT('0' + CONVERT(varchar, DATEPART(hour,@currdate)),2) + ':' + RIGHT('0' + CONVERT(varchar, DATEPART(minute, @currdate)),2)

/*set the dear field variable*/
SELECT @sDate = CONVERT(varchar,DATEPART(year, @currdate)) + RIGHT('0' + CONVERT(varchar,DATEPART(month, @currdate)),2) + RIGHT('0' + CONVERT(varchar, DATEPART(day, @currdate)),2)
SELECT @dear = @sDate + @currtime + ':' + RIGHT('0' + CONVERT(varchar, DATEPART(second, @currdate)),2)
EXEC Castell_SetValue @tempgmnv, 'dear', @dear

/*make the current date 12:00am*/
SELECT @currdate = CONVERT(datetime, CONVERT(char(8), @currdate, 112))

EXEC Castell_GetValue @tempgmnv, 'filename', @filename OUTPUT
EXEC Castell_EraseName @tempgmnv, 'FILENAME'

/*city field*/
IF @recid IS NULL OR @recid = 'null'
    BEGIN
        SELECT @new = 'Y'
        if @currdate is null
			SELECT @currdate = getdate()
        SELECT @currtime = RIGHT('0' + CONVERT(varchar, DATEPART(hour,@currdate)),2) + ':' + RIGHT('0' + CONVERT(varchar, DATEPART(minute, @currdate)),2)

        SELECT @city = SUBSTRING(UPPER(@user) + SPACE(8), 1, 8) + DATENAME(year, @currdate) + RIGHT('0' + CONVERT(varchar,DATEPART(month, @currdate)), 2) + RIGHT('0' + CONVERT(varchar,DATEPART(day, @currdate)),2) + ' ' +
                                  CASE
                                    WHEN CONVERT(int, SUBSTRING(@currtime,1,(PATINDEX('%:%',@currtime) - 1))) > 12 
						                THEN CONVERT(varchar,(CONVERT(int, SUBSTRING(@currtime,1,(PATINDEX('%:%',@currtime) - 1))) - 12)) + 
							                  SUBSTRING(@currtime,PATINDEX('%:%',@currtime),3) + 'pm '
                                    WHEN CONVERT(int, SUBSTRING(@currtime,1,(PATINDEX('%:%',@currtime) - 1))) = 12
									    THEN @currtime + 'pm '
 				                    ELSE @currtime + 'am '
                                  END   
        EXEC Castell_SetValue @tempgmnv, 'city', @city
        EXEC Castell_SetValue @tempgmnv, 'rectype', 'L'
    END
 
IF @filename IS NOT NULL
	BEGIN
		/*get the extension and write the contact field variable*/
		SELECT @extension =  RIGHT(@filename, (DATALENGTH(@filename) - PATINDEX('%.%', @filename)) + 1) 
	    
		IF @recid IS NULL
			BEGIN   /*build the contact field from scratch*/
				SELECT @contact = LEFT(UPPER(@owner) + SPACE(10), 10) + LEFT(@extension + SPACE(12), 12) + @sDate
				EXEC Castell_SetValue @tempgmnv, 'contact', @contact
			END
	    
		/*get the file description from the registry*/
		--EXEC master..xp_regread 'HKEY_CLASSES_ROOT', @extension, NULL, @title OUTPUT
		--IF @title IS NOT NULL
		--	EXEC master..xp_regread 'HKEY_CLASSES_ROOT', @title, NULL, @title OUTPUT
		select @title=case upper(@extension)
							when '.DOC' then 'Word Document'
							when '.DOCX' then 'Word Document'
							when '.DOT' then 'Word Template'
							when '.DOTX' then 'Word Template'
							when '.XLS' then 'Excel Spreadsheet'
							when '.XLSX' then 'Excel Spreadsheet'
							when '.PDF' then 'Adobe Acrobat Document'
							when '.HTM' then 'HTML Document'
							when '.HTML' then 'HTML Document'
							when '.JPG' then 'JPG Image'
							when '.GIF' then 'GIF Image'
							when '.PNG' then 'PNG Image'
							when '.BMP' then 'BMP Image'
							else ''
					   end --case extension


		EXEC Castell_SetValue @tempgmnv, 'title', @title

		/*put filename in address fields*/
		IF DATALENGTH(@filename) < 40
			SELECT @address1 = @filename
		ELSE
			BEGIN
				SELECT @address1 = SUBSTRING(@filename, 1, 40)
				SELECT @address2 = SUBSTRING(@filename, 41, 40)
			END

		EXEC Castell_SetValue @tempgmnv, 'address1', @address1
		EXEC Castell_SetValue @tempgmnv, 'address2', @address2
	END


EXEC Castell_WriteContsupp @tempgmnv

EXEC Castell_GetValue @tempgmnv, 'recid', @recid OUTPUT
EXEC Castell_SetValue @gmnv, 'recid', @recid

IF @new = 'Y'
    BEGIN
        SELECT @linkeddoctemp = '~~SYNC=' + CASE @syncfile    WHEN '0' THEN '0'
                                                              WHEN '1' THEN '1'
                                                              ELSE '0' END + char(10) +
                                '~~CREATETIME=' + @sDate + SUBSTRING(@currtime, 1,2) +
                                SUBSTRING(@currtime,4,2) + char(10) + '~~FILENAME='
        UPDATE contsupp
            SET linkeddoc = NULL
        WHERE recid = @recid

        SELECT @linkeddocptr = TEXTPTR(linkeddoc) FROM contsupp WHERE recid = @recid
    
        WRITETEXT contsupp.linkeddoc @linkeddocptr @linkeddoctemp
        UPDATETEXT contsupp.linkeddoc @linkeddocptr NULL 0 @filename

        SELECT @linkeddoctemp = char(10) /*write a lf at the end of the notes field*/
        UPDATETEXT contsupp.linkeddoc @linkeddocptr NULL 0 @linkeddoctemp
        EXEC Castell_UpdateSyncLog 'contsupp', @recid, @user, 'U', 'linkeddoc'
   END
ELSE IF @filename IS NOT NULL OR @syncfile IS NOT NULL OR @owner IS NOT NULL
    BEGIN
        UPDATE contsupp
        SET contact = CASE WHEN @owner IS NOT NULL THEN LEFT(UPPER(@owner) + SPACE(10), 10) 
                           ELSE LEFT(contact,10) END + 
                      CASE WHEN @filename IS NOT NULL 
                                THEN LEFT(@extension + SPACE(12), 12) + RIGHT(contact, 8)
                           ELSE RIGHT(contact, 20) END
        WHERE recid = @recid
        
        IF @filename IS NOT NULL OR @syncfile IS NOT NULL
            /*get the pointer to the linkeddoc field in contsupp*/
            SELECT @linkeddocptr = TEXTPTR(linkeddoc), @iFileNameStart = PATINDEX('%~~FILENAME=%',linkeddoc)
            FROM contsupp WHERE recid = @recid

        IF @syncfile IS NOT NULL
            BEGIN
                SELECT @linkeddoctemp = '~~SYNC=' + CASE @syncfile    WHEN '0' THEN '0'
                                                              WHEN '1' THEN '1'
                                                              ELSE '0' END + char(10) 
                UPDATETEXT contsupp.linkeddoc @linkeddocptr 0 9 @linkeddoctemp
            END

        IF @filename IS NOT NULL
            BEGIN
                SELECT @iFileNameStart = @iFileNameStart + 10
                UPDATETEXT contsupp.linkeddoc @linkeddocptr @iFileNameStart NULL @filename
                SELECT @linkeddoctemp = char(10) /*write a lf at the end of the notes field*/
                UPDATETEXT contsupp.linkeddoc @linkeddocptr NULL 0 @linkeddoctemp
             END
        EXEC Castell_UpdateSyncLog 'contsupp', @recid, @user, 'U', 'linkeddoc'
     END

EXEC Castell_Delete @tempgmnv










GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteMailbox]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO




CREATE PROCEDURE [dbo].[Castell_WriteMailbox](@gmnv varchar(20), @mailbody varchar(max))
AS

SET NOCOUNT ON

/*Declarations*/
DECLARE @currdate varchar(10),
		@currtime varchar(8),
		@user varchar(8),
		@userid varchar(8),
		@rectype varchar(10),
		@linkrecid varchar(15),
		@loprecid varchar(15),
		@recid varchar(15),		
		@accountno varchar(20),
		@mailto varchar(180),
		@mailfrom varchar(180),
		@maildate varchar(20),
		@mailtime varchar(8),
		@mailref varchar(100),
		@mailsize int,
		@subject varchar(100),
		@folder varchar(20),
		@folder2 varchar(20),
		@mailheader varchar(max),
		@rfc822 varchar(max),
		@bodypointer varbinary(30),
		@mailid varchar(200),
		@setcmd varchar(1000),
		@fieldname varchar(50),
		@value varchar(8000),
		@datatype varchar(20),
		@tempgmnv varchar(15),
		@namefield varchar(20),
		@valuefield varchar(8000),
		@valid int,
		@caldb varchar(128),
		@contactdb varchar(128),
		@tempsql varchar(1000),
		@createon datetime,
		@contact varchar(60),
		@CRLF char(2),
		@gmnv2 varchar(20),
		@flags varchar(8),
		@histrecid varchar(15),
		@direction varchar(1),
		@chrectype varchar(2),
		@mimetype varchar(120),
		@mimeboundary varchar(50),
		@attachment1 varchar(300),
		@attachment1filename varchar(100),
		@attachpart1 varchar(1000)

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
            RETURN -120

/*get the current time*/
SELECT @currtime = RIGHT('0' + CONVERT(varchar, DATEPART(hour,GETDATE())),2) + ':' + RIGHT('0' + CONVERT(varchar, DATEPART(minute, GETDATE())),2) + ':' + RIGHT('0' + CONVERT(varchar, DATEPART(second, GETDATE())),2)

/*get the current date*/
SELECT @currdate = CONVERT(varchar(10), GETDATE(), 101)

SELECT @CRLF = char(10) + char(13)
	
/*retrieve required data from name/value structure*/
EXEC Castell_CreateCopy @gmnv, @tempgmnv OUTPUT

EXEC Castell_GetValue @tempgmnv, 'recid', @recid OUTPUT
EXEC Castell_EraseName @tempgmnv, 'recid'
IF @recid = ''
    SELECT @recid = NULL
IF @recid IS NOT NULL
	RETURN -200

EXEC Castell_GetValue @tempgmnv, 'user', @user OUTPUT
EXEC Castell_EraseName @tempgmnv, 'user'

IF @user IS NULL
    RETURN -130
    
/* get db nav stuff */
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
IF @contact IS NULL
	SELECT @contact=''
	
EXEC Castell_GetValue @tempgmnv, 'maildate', @maildate OUTPUT
IF @maildate IS NULL or @maildate=''
	BEGIN
		EXEC Castell_SetValue @tempgmnv, 'maildate', @currdate
		SELECT @maildate=@currdate
	END

EXEC Castell_GetValue @tempgmnv, 'mailtime', @mailtime OUTPUT
IF @mailtime IS NULL or @mailtime=''
	BEGIN
		EXEC Castell_SetValue @tempgmnv, 'mailtime', @currtime
		SELECT @mailtime=@currtime
	END
	
EXEC Castell_GetVAlue @tempgmnv, 'subject', @subject  OUTPUT
EXEC Castell_EraseName @tempgmnv, 'subject'
IF @subject IS NULL
	SELECT @subject=''
	ELSE
	select @subject=REPLACE(@subject,'''','')
	
EXEC Castell_GetVAlue @tempgmnv, 'mailto', @mailto  OUTPUT
EXEC Castell_EraseName @tempgmnv, 'mailto'
EXEC Castell_GetVAlue @tempgmnv, 'mailfrom', @mailfrom  OUTPUT
EXEC Castell_EraseName @tempgmnv, 'mailfrom'

EXEC Castell_GetValue @tempgmnv, 'attachment1', @attachment1  OUTPUT
EXEC Castell_EraseName @tempgmnv, 'attachment1'

EXEC Castell_GetValue @tempgmnv, 'folder', @folder OUTPUT
EXEC Castell_GetValue @tempgmnv, 'folder2', @folder2 OUTPUT

EXEC Castell_GetVAlue @tempgmnv, 'direction', @direction  OUTPUT
EXEC Castell_EraseName @tempgmnv, 'direction'

IF @direction = 'I'
	BEGIN
		SELECT @chrectype='MI'
		SELECT @flags='2'
		if @folder is null or @folder=''
			BEGIN
				select @folder='Filed'
				if @folder2 is null or @folder2=''
					BEGIN
						select @folder2=(select top 1 folder2 from mailbox where userid=@userid and folder='X-GM-SUBFILED' order by recid desc)
					END
			END
	END
	
IF @direction = 'O'
	BEGIN
		SELECT @chrectype='MO'
		SELECT @flags='15'
		if @folder is null or @folder=''
			BEGIN
				select @folder='Sent'
				if @folder2 is null or @folder2=''
					BEGIN
						select @folder2=(select top 1 folder2 from mailbox where userid=@userid and folder='X-GM-SUBSENT' order by recid desc)
					END
			END
	END
	
EXEC Castell_GetValue @tempgmnv, 'createon', @createon OUTPUT
EXEC Castell_EraseName @tempgmnv, 'createon'
IF @createon = '' or @createon IS NULL
    SELECT @createon = GETDATE()

IF @recid IS NULL  /*adding a new activity*/
	set @recid = (select dbo.Castell_RecId('MASTER'))

EXEC Castell_GetVAlue @tempgmnv, 'mimetype', @mimetype  OUTPUT
EXEC Castell_EraseName @tempgmnv, 'mimetype'
IF @mimetype is NULL or @mimetype = ''
	BEGIN
		SELECT @mimeboundary = 'castellproc' + @recid +''
		SELECT @mimetype= 'multipart/related; boundary="' + @mimeboundary + '"'
	END

EXEC Castell_GetValue @tempgmnv, 'mailid', @mailid OUTPUT
IF @mailid IS NULL
	BEGIN
		SELECT @mailid = '<' + @recid + @accountno + 'castell' + '>'
		EXEC Castell_SetValue @tempgmnv, 'mailid', @mailid
    END
    
EXEC Castell_GetValue @tempgmnv, 'linkrecid', @linkrecid OUTPUT
IF @linkrecid IS NULL
	SELECT @linkrecid = ''

/*Do the initial insert. */
INSERT mailbox (userid, accountno, linkrecid, folder, folder2, createon, recid) 
     VALUES(CASE WHEN @userid IS NULL THEN UPPER(@user) ELSE UPPER(@userid) END,
            COALESCE(@accountno, ''), 
            @linkrecid, 
            @folder,
            @folder2,
            @createon,
            @recid)

/*update sync info*/
--EXEC Castell_UpdateSyncLog 'mailbox', @recid, @user, 'N'
 
/*build the mail header */
SELECT @mailheader  = 'Subject: ' + @subject + @CRLF + 'Date: ' + @maildate + @CRLF + 'From: ' + @mailfrom + @CRLF + 'Reply to: ' + @mailfrom + @CRLF + 'To: ' + @mailto + @CRLF

IF @mailid IS NOT NULL
	SELECT @mailheader = @mailheader + 'Message-id: ' + @mailid + @CRLF 
	
IF @mimetype IS NOT NULL
	SELECT @mailheader = @mailheader + 'MIME Type: ' + @mimetype + @CRLF

/*build rfc822*/
IF @attachment1 IS  NULL or @attachment1='' -- no attachment
	BEGIN
		SELECT @rfc822 = @mailheader + @CRLF + @mailbody
		update mailbox set RFC822=@rfc822 where recid=@recid
		Select @mailsize = LEN(@rfc822)
	END

IF @attachment1 IS NOT NULL and @attachment1<>'' --handle an attachment
	BEGIN
		SELECT @attachment1filename = RIGHT(@attachment1, CHARINDEX('\', REVERSE(@attachment1)) -1)
		SELECT @mailheader = @mailheader + '--' + @mimeboundary 
		SELECT @rfc822 = @mailheader + @CRLF + 'Content-Type: multipart/mixed; boundary="' + @mimeboundary + '"' + @CRLF + @CRLF + '--' + @mimeboundary  + @CRLF + 'Content-Type: text/plain; charset="utf-8"   Content-Transfer-Encoding: quoted-printable' + @CRLF + @CRLF + @mailbody + @CRLF 
		--to-do: loop this for additional attachments?
		SELECT @attachpart1= '--' + @mimeboundary + @CRLF + 'Content-Disposition: attachment; filename="' + @attachment1filename +'"  Content-Transfer-Encoding: base64' + @CRLF + @CRLF + @attachment1 + @CRLF + '--' + @mimeboundary + '--' + @CRLF
		
		update mailbox set RFC822=@rfc822 where recid=@recid

		Select @bodypointer=TEXTPTR(rfc822)from mailbox Where recid=@recid
		UPDATETEXT mailbox.rfc822 @bodypointer NULL 0 @attachpart1
		
		Select @mailsize = LEN(@rfc822) + LEN(@attachpart1)
	END

EXEC Castell_EraseName @tempgmnv, 'rfc822'

EXEC Castell_SetValue @tempgmnv, 'mailsize', @mailsize

/*build the mailref line */
IF @contact<>'' AND @subject<>'' 
	SELECT @mailref=@contact + char(9) + @subject
ELSE
	SELECT @mailref=@subject

EXEC Castell_SetValue @tempgmnv, 'mailref', @mailref

/*make the conthist record */
EXEC Castell_Create @gmnv2 OUTPUT

EXEC Castell_SetValue @gmnv2, 'accountno', @accountno
EXEC Castell_SetValue @gmnv2, 'rectype', @chrectype
EXEC Castell_SetValue @gmnv2, 'contact', @contact
EXEC Castell_SetValue @gmnv2, 'user', @userid
EXEC Castell_SetValue @gmnv2, 'ref', @subject
EXEC Castell_SetValue @gmnv2, 'ondate', @maildate
EXEC Castell_SetValue @gmnv2, 'ontime', @mailtime
EXEC Castell_SetValue @gmnv2, 'linkrecid', @recid

EXEC Castell_WriteHistory @gmnv2
EXEC Castell_GetValue @gmnv2, 'recid', @histrecid OUTPUT

EXEC dbo.Castell_Delete @gmnv2
	
EXEC Castell_SetValue @tempgmnv, 'flags', @flags
EXEC Castell_SetValue @tempgmnv, 'ext', 'eml'
EXEC Castell_SetValue @tempgmnv, 'linkrecid', @histrecid

/*Now handle updating mailbox fields*/
SELECT @namefield 'name', @valuefield 'value' INTO #tempgmnv

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
		SELECT @datatype=DATA_TYPE from INFORMATION_SCHEMA.COLUMNS where COLUMN_NAME = @fieldname and TABLE_NAME = 'MAILBOX'

		IF upper(@fieldname) <> 'RECID' and upper(@fieldname) <> 'RFC822' and @datatype is not NULL
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

			EXEC('UPDATE MAILBOX SET ' + @setcmd)
			--EXEC Castell_UpdateSyncLog 'MAILBOX', @recid, @user, 'U', @fieldname
		END

		FETCH NEXT FROM fieldcursor INTO @fieldname, @value
	END
CLOSE fieldcursor
DEALLOCATE fieldcursor

EXEC Castell_SetValue @gmnv, 'recid', @recid




GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteOpportunity]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO









CREATE PROCEDURE [dbo].[Castell_WriteOpportunity] (@gmnv varchar(20))
AS
DECLARE @setcmd1 varchar(max),
		@user varchar(8), 
		@recid varchar(15), 
		@crecid varchar(15),
		@contact varchar(40), 
		@company varchar(40), 
		@fieldname varchar(50),
		@value varchar(8000), 
		@datatype varchar(32), 
		@accountno varchar(20), 
		@currtime varchar(5), 
		@gmversion int, 
		@valuestr varchar(8000), 
		@namefield varchar(20), 
		@valuefield varchar(8000), 
		@tempgmnv varchar(20),
		@notes varchar(8000),
		@nrecid varchar(15),
		@actvcode varchar(3),
		@closedate datetime,
		@product varchar(80),
		@units float,
		@potential float,
		@probability float

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
            RETURN -120

EXEC Castell_CreateCopy @gmnv, @tempgmnv OUTPUT

--retrieve necessary data from gmnv
EXEC Castell_GetValue @tempgmnv, 'userid', @user OUTPUT
EXEC Castell_EraseName @tempgmnv, 'userid'
EXEC Castell_GetValue @tempgmnv, 'gmversion', @gmversion OUTPUT
EXEC Castell_EraseName @tempgmnv, 'gmversion'
EXEC Castell_GetValue @tempgmnv, 'recid', @recid OUTPUT
EXEC Castell_EraseName @tempgmnv, 'recid'
EXEC Castell_GetValue @tempgmnv, 'accountno', @accountno OUTPUT
EXEC Castell_EraseName @tempgmnv, 'accountno'
EXEC Castell_GetValue @tempgmnv, 'notes', @notes OUTPUT
EXEC Castell_EraseName @tempgmnv, 'notes'

--get values for the forecaseted sale	
EXEC Castell_GetValue @tempgmnv, 'cycle', @actvcode OUTPUT
EXEC Castell_GetValue @tempgmnv, 'closeby', @closedate OUTPUT
EXEC Castell_GetValue @tempgmnv, 'productname', @product OUTPUT
EXEC Castell_GetValue @tempgmnv, 'units', @units OUTPUT
EXEC Castell_GetValue @tempgmnv, 'foramt', @potential OUTPUT
EXEC Castell_GetValue @tempgmnv, 'forprob', @probability OUTPUT


IF @user IS NULL
    BEGIN
        EXEC Castell_Delete @tempgmnv
        RETURN -130
    END

IF @accountno IS NULL
    BEGIN
        EXEC Castell_Delete @tempgmnv
        RETURN -180
    END

IF @notes = ''
    SELECT @notes = NULL

select @company=company from contact1 where accountno=@accountno
select @contact=contact from contact1 where accountno=@accountno

IF @recid = ''
    SELECT @recid = NULL

IF @recid IS NULL --creating a new record
    BEGIN
        --Get the Recids we need
        set @recid = (select dbo.Castell_RecId('MASTER'))
		waitfor delay '00:00:00.010' --make sure we don't get the same recid for both!
        set @crecid = (select dbo.Castell_RecId('MASTER'))
        --Update Tlogs
		EXEC Castell_UpdateSyncLog 'OPMGR', @recid, @user, 'N'
		--Inert the new empty Opportunity record
		INSERT INTO OPMGR 
		(OPID,RECTYPE,ACCOUNTNO,USERID,FLAGS,COMPANY,CONTACT,NAME,STATUS,CYCLE,STAGE,SOURCE,F1,F2,F3,STARTDATE,CLOSEDDATE,CLOSEBY,FORAMT,FORPROB,CLOSEAMT,NOTES,U_STAGE,recid,PRODUCTNAME) 
		VALUES 
		(@recid,'O',@accountno,@user, '', @company, @contact, '', '', '', '', '', '', '', '',NULL,NULL,NULL,0,0,0, '', '',@recid, '')
		--Inert the Opportunity-Contact link record
		INSERT INTO OPMGR 
		(OPID,RECTYPE,ACCOUNTNO,USERID,FLAGS,COMPANY,CONTACT,NAME,STATUS,CYCLE,STAGE,SOURCE,F1,F2,F3,STARTDATE,CLOSEDDATE,CLOSEBY,FORAMT,FORPROB,CLOSEAMT,NOTES,U_STAGE,recid,PRODUCTNAME) 
		VALUES 
		(@recid,'OC',@accountno,'', NULL, @company, @contact, '', '', '', NULL, NULL, NULL, NULL, NULL,NULL,NULL,NULL,NULL,NULL,NULL, '', '',@crecid, NULL)
    END

--modify fields on the existing (or newly created) record
if exists (select * from sysobjects where id = object_id('#tempgmnv') and sysstat & 0xf = 4)
	drop table #tempgmnv

SELECT @namefield 'name', @valuefield 'value' INTO #tempgmnv
DELETE FROM #tempgmnv WHERE name IS NULL

INSERT INTO #tempgmnv
EXEC ('SELECT fieldname, value FROM ##gmnv' + @tempgmnv)

EXEC Castell_Delete @tempgmnv

DECLARE fieldcursor CURSOR 
FOR SELECT name, value FROM #tempgmnv

OPEN fieldcursor

FETCH NEXT FROM fieldcursor INTO @fieldname, @value

WHILE @@fetch_status = 0
	BEGIN
		SELECT @datatype=DATA_TYPE from INFORMATION_SCHEMA.COLUMNS where COLUMN_NAME = @fieldname and TABLE_NAME = 'OPMGR'

		BEGIN
			SELECT @setcmd1 = ' [' + @fieldname + '] = ' 
			+
			CASE  --deal with data types that require quotes versus those that don't.  Also deal with NULL
				WHEN UPPER(@datatype) NOT IN ('FLOAT', 'INT', 'REAL','NUMERIC','SMALLINT','TINYINT','DECIMAL') AND @value IS NOT NULL THEN '"' + REPLACE(@value,'"','""') + '"'
				WHEN @value IS NULL THEN 'NULL' 
				ELSE @value 
			END 
			+
			CASE 
				WHEN UPPER(@fieldname) IN ('STAGE') AND @value IS NOT NULL THEN ', u_' + @fieldname + ' = ''' + UPPER(REPLACE(@value, '"', '""')) + ''''
				WHEN UPPER(@fieldname) IN ('STAGE') AND @value IS NULL THEN ', u_' + @fieldname + ' = ''''' 
				ELSE '' 
			END
			+ 
			' WHERE recid = ' 
			+ 
			CASE  --deal with recids with quote marks in them
				WHEN @recid LIKE '%''%' THEN '"' + @recid + '"'
				WHEN @recid LIKE '%"%'  THEN '''' + @recid + '''' 
				ELSE '''' + @recid + ''''
 			END

			EXEC('UPDATE OPMGR SET ' + @setcmd1)

			EXEC Castell_UpdateSyncLog 'OPMGR', @recid, @user, 'U', @fieldname
		END

		FETCH NEXT FROM fieldcursor INTO @fieldname, @value
	END
CLOSE fieldcursor
DEALLOCATE fieldcursor

If @notes is not NULL
	BEGIN
		set @nrecid = (select dbo.Castell_RecId('MASTER'))
		INSERT INTO NOTES (CREATEDDATE,USERID,MODIFIEDDATE,MODIFIEDBY,LOPRECID,ACCOUNTNO,NOTE,RECTYPE,EXT,recid)
		VALUES (getdate(),@user,getdate(),@user,@recid,@accountno,@notes,'OP','html',@nrecid)
	END


Declare @gmnv2 varchar(20)
 
EXEC dbo.Castell_Create @gmnv2 OUTPUT
--EXEC dbo.Castell_SetValue @gmnv, 'Recid', ''
EXEC dbo.Castell_SetValue @gmnv2, 'User', @user
EXEC dbo.Castell_SetValue @gmnv2, 'accountno', @accountno
EXEC dbo.Castell_SetValue @gmnv2, 'rectype', 'S'
EXEC dbo.Castell_SetValue @gmnv2, 'contact', @contact
EXEC dbo.Castell_SetValue @gmnv2, 'loprecid', @recid
EXEC dbo.Castell_SetValue @gmnv2, 'actvcode', @actvcode 
EXEC dbo.Castell_SetValue @gmnv2, 'ondate', @closedate
EXEC dbo.Castell_SetValue @gmnv2, 'unitssale', @units  
EXEC dbo.Castell_SetValue @gmnv2, 'potnsale', @potential 
EXEC dbo.Castell_SetValue @gmnv2, 'probsale', @probability 
EXEC dbo.Castell_SetValue @gmnv2, 'notes', ''
EXEC dbo.Castell_SetValue @gmnv2, 'ref', @product
EXEC Castell_WriteSchedule @gmnv2
EXEC dbo.Castell_Delete @gmnv2

EXEC Castell_SetValue @gmnv, 'recid', @recid

EXEC Castell_Delete @tempgmnv









GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteOtherContact]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO






CREATE PROCEDURE [dbo].[Castell_WriteOtherContact] (@gmnv varchar(20))
AS

DECLARE @nonusaphone varchar(1),
		@tempphone varchar(20),
		@tempvalue varchar(8000),
		@emailgmnv varchar(20),
		@email varchar(35),
		@tempgmnv varchar(20),
		@retval int,
		@user varchar(10),
		@recid varchar(15),
		@emailrecid varchar(15),
		@usemergecodes varchar(1),
		@mergecodes varchar(255)

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
            RETURN -120

EXEC Castell_GetValue @gmnv, 'user', @user OUTPUT
IF @user IS NULL
    RETURN -130

EXEC Castell_CreateCopy @gmnv, @tempgmnv OUTPUT

/*Establish that this is an additional contact*/
EXEC Castell_SetValue @tempgmnv, 'rectype', 'C'

/*format phone*/
EXEC Castell_GetValue @tempgmnv, 'nonusaphone', @nonusaphone OUTPUT
EXEC Castell_EraseName @tempgmnv, 'nonusaphone'
EXEC Castell_GetValue @tempgmnv, 'phone', @tempphone OUTPUT
IF (@nonusaphone <> '1' OR @nonusaphone IS NULL) AND @tempphone IS NOT NULL
    BEGIN
        EXEC Castell_FormatPhone @tempphone OUTPUT
        EXEC Castell_SetValue @tempgmnv, 'phone', @tempphone
    END

/*Repeat for fax(mobile)*/
SELECT @tempphone = NULL
EXEC Castell_GetValue @tempgmnv, 'fax', @tempphone OUTPUT
IF (@nonusaphone <> '1' OR @nonusaphone IS NULL) AND @tempphone IS NOT NULL
    BEGIN
        EXEC Castell_FormatPhone @tempphone OUTPUT
        EXEC Castell_SetValue @tempgmnv, 'fax', @tempphone
    END

/*mergecodes? and usemergecodes?*/
EXEC Castell_GetValue @tempgmnv, 'usemergecodes', @usemergecodes OUTPUT
EXEC Castell_EraseName @tempgmnv, 'usemergecodes'
EXEC Castell_GetValue @tempgmnv, 'mergecodes', @mergecodes OUTPUT    
EXEC Castell_EraseName @tempgmnv, 'mergecodes' 
SELECT @mergecodes

/*make ref into contsuppref*/
EXEC Castell_ChangeName @tempgmnv, 'REF', 'CONTSUPREF'

/*call WriteContSupp to make our additional contact*/
EXEC Castell_WriteContsupp @tempgmnv

EXEC Castell_GetValue @tempgmnv, 'recid', @recid OUTPUT
EXEC Castell_SetValue @gmnv, 'recid', @recid

/*deal with mergecodes*/
IF @mergecodes IS NOT NULL OR @mergecodes <> '' OR @usemergecodes IS NOT NULL OR @usemergecodes <> '' OR @recid IS NULL OR @recid = '' 
	UPDATE contsupp SET mergecodes = CASE WHEN @usemergecodes = '1'
											THEN ' '
										  WHEN @usemergecodes = '0' OR
											  (@usemergecodes IS NULL AND
											   (@recid IS NULL OR @recid = ''))
											THEN char(10)
										  ELSE SUBSTRING(mergecodes, 1,1)
									 END +
									 CASE WHEN @mergecodes IS NULL  
										  THEN SUBSTRING(mergecodes, 2, DATALENGTH(mergecodes) - 1)
										  ELSE @mergecodes
									 END
	WHERE recid = @recid

/*handle email*/
/*get the email address*/
EXEC Castell_GetValue @tempgmnv, 'email', @email OUTPUT

IF @email IS NOT NULL
    BEGIN
        
        EXEC Castell_Create @emailgmnv OUTPUT
    
        SELECT @tempvalue = NULL
        EXEC Castell_GetValue @tempgmnv, 'user', @tempvalue OUTPUT
        IF @tempvalue IS NOT NULL
            EXEC Castell_SetValue @emailgmnv, 'user', @tempvalue
       
        SELECT @tempvalue = NULL
        EXEC Castell_GetValue @tempgmnv, 'caldb', @tempvalue OUTPUT
        IF @tempvalue IS NOT NULL
            EXEC Castell_SetValue @emailgmnv, 'caldb', @tempvalue

		--EXEC Castell_GetValue @tempgmnv, 'recid', @emailrecid OUTPUT --need this later for email

		/*Is this email address already in the database linked to this contact?*/
        --IF @emailrecid IS NOT NULL AND @emailrecid <> ''
           -- BEGIN
                SELECT @emailrecid = NULL
                SELECT @emailrecid = cs1.recid FROM contsupp cs1, contsupp cs2
                    WHERE cs1.accountno = cs2.accountno AND cs1.rectype = 'P' AND
                      cs1.u_contact = 'E-MAIL ADDRESS' 
                      AND cs2.recid = @recid AND cs1.linkacct = @recid
            --END
  
        IF @emailrecid IS NOT NULL AND @emailrecid <> ''
            BEGIN
                EXEC Castell_SetValue @emailgmnv, 'ref', @email
                EXEC Castell_SetValue @emailgmnv, 'recid', @emailrecid
            END
        ELSE
            BEGIN
                
                SELECT @tempvalue = NULL
                EXEC Castell_GetValue @tempgmnv, 'accountno', @tempvalue OUTPUT
                IF @tempvalue IS NULL
                    SELECT @tempvalue = accountno FROM contsupp WHERE recid = @recid
                EXEC Castell_SetValue @emailgmnv, 'accountno', @tempvalue
                EXEC Castell_SetValue @emailgmnv, 'linkacct', @recid
                EXEC Castell_SetValue @emailgmnv, 'detail', 'E-mail Address'
				EXEC Castell_SetValue @emailgmnv, 'ufield6', '001'
                EXEC Castell_SetValue @emailgmnv, 'ref', @email
        
                SELECT @tempvalue = NULL
                EXEC Castell_GetValue @tempgmnv, 'recid', @tempvalue OUTPUT
                EXEC Castell_SetValue @emailgmnv, 'ufield2', @tempvalue

                SELECT @tempvalue = NULL
                EXEC Castell_GetValue @tempgmnv, 'contact', @tempvalue OUTPUT
                IF @tempvalue IS NOT NULL
                    EXEC Castell_SetValue @emailgmnv, 'ufield11', @tempvalue
            
                UPDATE contsupp SET STATUS = '1' + SUBSTRING(STATUS, 2,1)
                    WHERE recid = @recid
                EXEC Castell_UpdateSyncLog 'contsupp', @recid, @user, 'U', 'Status'
            END

        EXEC Castell_WriteDetail @emailgmnv
        EXEC Castell_Delete @emailgmnv

    END




GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteRelationship]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO






CREATE PROCEDURE [dbo].[Castell_WriteRelationship] (@gmnv varchar(20))
AS

DECLARE @accountno varchar(20),
		@parentaccountno varchar(20),
		@tempvalue varchar(8000),
		@tempgmnv varchar(20),
		@retval int,
		@user varchar(10),
		@level varchar(10),
		@label varchar(40),
		@parentid varchar(15),
		@city varchar(30),
		@currdate datetime,
		@currtime varchar(5),
		@mergecodes varchar(20),
		@ref varchar(35),
		@recid varchar(15),
		@bookaccountno varchar(20)

SET NOCOUNT OFF
/*returns -120 bad gmnv
          -130 Required parameter invalid or not passed*/
          
IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
            RETURN -120

EXEC Castell_GetValue @gmnv, 'user', @user OUTPUT
IF @user IS NULL
    RETURN -130

EXEC Castell_GetValue @gmnv, 'accountno', @accountno OUTPUT
--IF @accountno IS NULL
--    RETURN -130

EXEC Castell_GetValue @gmnv, 'level', @level OUTPUT
IF @level IS NULL or (upper(@level) not in ('BOOK','FOLDER','CONTACT','1','3','6'))
    RETURN -130

EXEC Castell_CreateCopy @gmnv, @tempgmnv OUTPUT

/*Establish that this is relationship tree entry*/
EXEC Castell_SetValue @tempgmnv, 'rectype', 'O'

/*city field*/
IF @recid IS NULL OR @recid = 'null' or @recid=''
    BEGIN
        if @currdate is null
			SELECT @currdate = getdate()
			SELECT @currtime = RIGHT('0' + CONVERT(varchar, DATEPART(hour,@currdate)),2) + ':' + RIGHT('0' + CONVERT(varchar, DATEPART(minute, @currdate)),2)
			SELECT @city = SUBSTRING(UPPER(@user) + SPACE(8), 1, 8) + DATENAME(year, @currdate) + RIGHT('0' + CONVERT(varchar,DATEPART(month, @currdate)), 2) + RIGHT('0' + CONVERT(varchar,DATEPART(day, @currdate)),2) + ' ' +
				  CASE
					WHEN CONVERT(int, SUBSTRING(@currtime,1,(PATINDEX('%:%',@currtime) - 1))) > 12 THEN CONVERT(varchar,(CONVERT(int, SUBSTRING(@currtime,1,(PATINDEX('%:%',@currtime) - 1))) - 12)) + SUBSTRING(@currtime,PATINDEX('%:%',@currtime),3) + 'pm '
					WHEN CONVERT(int, SUBSTRING(@currtime,1,(PATINDEX('%:%',@currtime) - 1))) = 12 THEN @currtime + 'pm '
					ELSE @currtime + 'am '
				  END   
        EXEC Castell_SetValue @tempgmnv, 'city', @city
    END
    
EXEC Castell_GetValue @gmnv, 'label', @label OUTPUT
EXEC Castell_GetValue @gmnv, 'parentid', @parentid OUTPUT

if @parentid is not null and @parentid<>'' 
	begin
		select @parentaccountno=contact from CONTSUPP where RECTYPE='O' and recid=@parentid
	end
	
if @parentid is null
	begin
		set @parentaccountno=@accountno
	end
	
	
/*Level fun
	This procedure does not (yet) account for multiple nested folder levels 
*/
if upper(@level)='BOOK' or @level='1'
	BEGIN
		set @mergecodes='**'
		set @label='  ' + @label
		set @bookaccountno=(select dbo.Castell_AccountNo())
		EXEC Castell_SetValue @tempgmnv, 'contsupref', '10000'
		EXEC Castell_SetValue @tempgmnv, 'ext', '1'
		EXEC Castell_SetValue @tempgmnv, 'contact', @bookaccountno
		EXEC Castell_SetValue @gmnv, 'accountno', '                    '
	END
if upper(@level)='FOLDER' or @level='3'
	BEGIN
		set @mergecodes='**'
		
		select @ref=contsupref from CONTSUPP where RECTYPE='O' and recid=@parentid
		set @ref=@ref+'00500'
		EXEC Castell_SetValue @tempgmnv, 'contsupref', @ref
		
		set @label='  '+@label

		EXEC Castell_SetValue @tempgmnv, 'ext', '3'
		EXEC Castell_SetValue @tempgmnv, 'contact', @parentaccountno 
		EXEC Castell_SetValue @gmnv, 'accountno', @accountno
	END
if upper(@level)='CONTACT' or @level='6'
	BEGIN
		set @mergecodes='**'
		select @ref=contsupref from CONTSUPP where RECTYPE='O' and recid=@parentid
		set @ref=@ref+' '+(select contact from CONTACT1 where ACCOUNTNO=@accountno)

		EXEC Castell_SetValue @tempgmnv, 'contsupref', @ref
		
		select @label=contact from CONTACT1 where ACCOUNTNO=@accountno
		set @label='  '+@label
		
		EXEC Castell_SetValue @tempgmnv, 'ext', '6'
		EXEC Castell_SetValue @tempgmnv, 'contact', @parentaccountno
		EXEC Castell_SetValue @gmnv, 'accountno', @accountno
	END

EXEC Castell_SetValue @tempgmnv, 'address1', @label
EXEC Castell_SetValue @tempgmnv, 'mergecodes', @mergecodes

/*and the NULL junk fields*/
EXEC Castell_SetValue @tempgmnv, 'title', NULL
EXEC Castell_SetValue @tempgmnv, 'status', NULL
EXEC Castell_SetValue @tempgmnv, 'dear', NULL
EXEC Castell_SetValue @tempgmnv, 'phone', NULL
EXEC Castell_SetValue @tempgmnv, 'FAX', NULL
EXEC Castell_SetValue @tempgmnv, 'LINKACCT', NULL
EXEC Castell_SetValue @tempgmnv, 'NOTES', NULL
EXEC Castell_SetValue @tempgmnv, 'ADDRESS2', NULL
EXEC Castell_SetValue @tempgmnv, 'ADDRESS3', NULL
EXEC Castell_SetValue @tempgmnv, 'STATE', NULL
EXEC Castell_SetValue @tempgmnv, 'ZIP', NULL
EXEC Castell_SetValue @tempgmnv, 'COUNTRY', NULL
EXEC Castell_SetValue @tempgmnv, 'STATUS', NULL
EXEC Castell_SetValue @tempgmnv, 'LINKEDDOC', NULL

/*call WriteContSupp to make the record*/
EXEC Castell_WriteContsupp @tempgmnv

EXEC Castell_GetValue @tempgmnv, 'recid', @recid OUTPUT
EXEC Castell_SetValue @gmnv, 'recid', @recid

    









GO

/****** Object:  StoredProcedure [dbo].[Castell_WriteSchedule]    Script Date: 3/21/2019 10:22:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO








CREATE PROCEDURE [dbo].[Castell_WriteSchedule](@gmnv varchar(20))

AS

DECLARE @currdate datetime,
		@currtime varchar(5),
		@notifyrecid varchar(15),
		@notifyref varchar(255),
		@tempgmnv varchar(20),
		@user varchar(8),
		@fieldname varchar(50),
		@value varchar(8000),
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
		@valuefield varchar(8000),
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


