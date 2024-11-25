SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_UpdateSyncLog]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_UpdateSyncLog] AS' 
END
GO










ALTER PROCEDURE [dbo].[Castell_UpdateSyncLog] (@tablename varchar(20), /*Table that was updated*/
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
