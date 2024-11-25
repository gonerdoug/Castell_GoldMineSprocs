SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_WriteLinkedDoc]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_WriteLinkedDoc] AS' 
END
GO









ALTER PROCEDURE [dbo].[Castell_WriteLinkedDoc] (@gmnv varchar(20))
                                                                                   
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
