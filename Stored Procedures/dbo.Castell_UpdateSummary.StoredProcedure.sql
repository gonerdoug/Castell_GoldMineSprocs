SET ANSI_NULLS OFF
GO
SET QUOTED_IDENTIFIER OFF
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_UpdateSummary]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_UpdateSummary] AS' 
END
GO










ALTER PROCEDURE [dbo].[Castell_UpdateSummary] (@accountno varchar(20), @table varchar(20), @user varchar(8), @caldb varchar(255) = NULL)

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
