SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_WriteOpportunity]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_WriteOpportunity] AS' 
END
GO










ALTER PROCEDURE [dbo].[Castell_WriteOpportunity] (@gmnv varchar(20))
AS
DECLARE @setcmd1 varchar(max),
		@user varchar(8), 
		@recid varchar(15), 
		@crecid varchar(15),
		@contact varchar(40), 
		@company varchar(40), 
		@fieldname varchar(50),
		@value varchar(max), 
		@datatype varchar(32), 
		@accountno varchar(20), 
		@currtime varchar(5), 
		@gmversion int, 
		@valuestr varchar(max), 
		@namefield varchar(20), 
		@valuefield varchar(max), 
		@tempgmnv varchar(20),
		@notes varchar(max),
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
