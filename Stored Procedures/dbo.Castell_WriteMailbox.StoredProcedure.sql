SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_WriteMailbox]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_WriteMailbox] AS' 
END
GO





ALTER PROCEDURE [dbo].[Castell_WriteMailbox](@gmnv varchar(20), @mailbody varchar(max))
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
		@valuefield varchar(max),
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
