SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_WriteGroup]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_WriteGroup] AS' 
END
GO











ALTER PROCEDURE [dbo].[Castell_WriteGroup] (@gmnv varchar(20))
   
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
