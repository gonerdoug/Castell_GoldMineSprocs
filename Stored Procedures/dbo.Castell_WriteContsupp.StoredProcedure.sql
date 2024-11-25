SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_WriteContsupp]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_WriteContsupp] AS' 
END
GO










ALTER PROCEDURE [dbo].[Castell_WriteContsupp] (@gmnv varchar(20))
   
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
		@value varchar(max),
		@datatype varchar(20),
		@setcmd1 varchar(max),
		@userid varchar(8),
		@namefield varchar(20),
		@valuefield varchar(max),
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
