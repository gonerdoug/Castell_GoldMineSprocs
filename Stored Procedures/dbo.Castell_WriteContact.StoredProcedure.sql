SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_WriteContact]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_WriteContact] AS' 
END
GO




ALTER PROCEDURE [dbo].[Castell_WriteContact] (@gmnv varchar(20))

AS

DECLARE @setcmd1    varchar(max),
		@user varchar(8), 
		@recid varchar(15), 
		@nonusaphone varchar(1), 
		@tempphone varchar(25), 
		@status varchar(3), 
		@email varchar(255),
		@sub_contact varchar(40),
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

				EXEC('UPDATE contact1 SET ' + @setcmd1)

				--EXEC Castell_UpdateSyncLog @tablename, @recid, @user, 'U', @fieldname
			END

        IF UPPER(@tablename) = 'CONTACT2'
            BEGIN
                set @accountno = (select accountno FROM contact1 WHERE recid = @recid)

                IF @c2recid IS NULL
                    SELECT @C2recid = recid FROM contact2 WHERE accountno = @accountno

                IF @c2recid IS /*STILL*/ NULL --there is no contact2 record for this contact!
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

		select @sub_contact=substring(@contact,0,40)

        EXEC Castell_SetValue @tempgmnv, 'accountno', @accountno
        EXEC Castell_SetValue @tempgmnv, 'ufield2', @recid
        EXEC Castell_SetValue @tempgmnv, 'ufield11', @sub_contact
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
