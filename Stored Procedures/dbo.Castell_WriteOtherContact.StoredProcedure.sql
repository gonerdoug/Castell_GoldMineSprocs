SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_WriteOtherContact]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_WriteOtherContact] AS' 
END
GO







ALTER PROCEDURE [dbo].[Castell_WriteOtherContact] (@gmnv varchar(20))
AS

DECLARE @nonusaphone varchar(1),
		@tempphone varchar(20),
		@tempvalue varchar(max),
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
