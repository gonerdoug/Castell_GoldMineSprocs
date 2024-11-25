SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_WriteDetail]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_WriteDetail] AS' 
END
GO












ALTER PROCEDURE [dbo].[Castell_WriteDetail] (@gmnv varchar(20))
    
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
DECLARE @tempvalue varchar(max)

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
