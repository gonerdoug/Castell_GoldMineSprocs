SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_WriteRelationship]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_WriteRelationship] AS' 
END
GO





ALTER PROCEDURE [dbo].[Castell_WriteRelationship] (@gmnv varchar(20))
AS
  
DECLARE @accountno varchar(20),
		@parentaccountno varchar(20),
		@tempvalue varchar(8000),
		@tempgmnv varchar(20),
		@retval int,
		@user varchar(10),
		@level varchar(10),
		@label varchar(40),
		@parentid varchar(15),
		@city varchar(30),
		@currdate datetime,
		@currtime varchar(5),
		@mergecodes varchar(20),
		@ref varchar(35),
		@recid varchar(15),
		@bookaccountno varchar(20)
  
SET NOCOUNT OFF
/*returns -120 bad gmnv
          -130 Required parameter invalid or not passed*/
          
IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
            RETURN -120
  
EXEC Castell_GetValue @gmnv, 'user', @user OUTPUT
IF @user IS NULL
    RETURN -130
  
EXEC Castell_GetValue @gmnv, 'accountno', @accountno OUTPUT
--IF @accountno IS NULL
--    RETURN -130
  
EXEC Castell_GetValue @gmnv, 'level', @level OUTPUT
IF @level IS NULL or (upper(@level) not in ('BOOK','FOLDER','FOLDER2','CONTACT','1','3','6'))
    RETURN -130
  
EXEC Castell_CreateCopy @gmnv, @tempgmnv OUTPUT
  
/*Establish that this is relationship tree entry*/
EXEC Castell_SetValue @tempgmnv, 'rectype', 'O'
  
/*city field*/
IF @recid IS NULL OR @recid = 'null' or @recid=''
    BEGIN
        if @currdate is null
			SELECT @currdate = getdate()
			SELECT @currtime = RIGHT('0' + CONVERT(varchar, DATEPART(hour,@currdate)),2) + ':' + RIGHT('0' + CONVERT(varchar, DATEPART(minute, @currdate)),2)
			SELECT @city = SUBSTRING(UPPER(@user) + SPACE(8), 1, 8) + DATENAME(year, @currdate) + RIGHT('0' + CONVERT(varchar,DATEPART(month, @currdate)), 2) + RIGHT('0' + CONVERT(varchar,DATEPART(day, @currdate)),2) + ' ' +
				  CASE
					WHEN CONVERT(int, SUBSTRING(@currtime,1,(PATINDEX('%:%',@currtime) - 1))) > 12 THEN CONVERT(varchar,(CONVERT(int, SUBSTRING(@currtime,1,(PATINDEX('%:%',@currtime) - 1))) - 12)) + SUBSTRING(@currtime,PATINDEX('%:%',@currtime),3) + 'pm '
					WHEN CONVERT(int, SUBSTRING(@currtime,1,(PATINDEX('%:%',@currtime) - 1))) = 12 THEN @currtime + 'pm '
					ELSE @currtime + 'am '
				  END   
        EXEC Castell_SetValue @tempgmnv, 'city', @city
    END
    
EXEC Castell_GetValue @gmnv, 'label', @label OUTPUT
EXEC Castell_GetValue @gmnv, 'parentid', @parentid OUTPUT
  
if @parentid is not null and @parentid<>'' 
        begin
		select @parentaccountno=contact from CONTSUPP where RECTYPE='O' and recid=@parentid
        end
        
if @parentid is null
        begin
		set @parentaccountno=@accountno
        end
        
/*Level fun*/
if upper(@level)='BOOK' or @level='1'
    BEGIN
		set @mergecodes='**'
		set @label='  ' + @label
		set @bookaccountno=(select dbo.Castell_AccountNo())
		EXEC Castell_SetValue @tempgmnv, 'contsupref', '10000'
		EXEC Castell_SetValue @tempgmnv, 'ext', '1'
		EXEC Castell_SetValue @tempgmnv, 'contact', @bookaccountno
		EXEC Castell_SetValue @gmnv, 'accountno', '                    '
    END
if upper(@level)='FOLDER' or @level='3'
    BEGIN
		set @mergecodes='**'
		--Figure out sub-folder name code, by adding 00500 to the value of the top folder (or that of the sub-folder if any)
		set @ref=(select top 1 cast(contsupref as bigint)+500 from contsupp where contact=@parentaccountno and ext=3 and len(contsupref)=10 order by cast(contsupref as bigint) desc)
  
		if isnull(@ref,'')='' 
		begin
			set @ref=(select top 1 cast(concat(contsupref , '00000')  as bigint)+500   from contsupp where contact=@parentaccountno  and ext=1 order by cast(concat(contsupref , '00000')  as bigint) desc)
		end
  
		EXEC Castell_SetValue @tempgmnv, 'contsupref', @ref
		set @label='  '+@label
		EXEC Castell_SetValue @tempgmnv, 'ext', '3'
		EXEC Castell_SetValue @tempgmnv, 'contact', @parentaccountno 
		EXEC Castell_SetValue @gmnv, 'accountno', @accountno
    END
  
if upper(@level)='FOLDER2'
    BEGIN
		set @mergecodes='**'
		--Figure out sub-folder name code, by adding 00500 to the value of the parent folder (or that of the sub-folder if any)
		set @ref=(select top 1 cast(concat(contsupref , '00000')  as bigint)+500 from contsupp where recid=@parentid order by cast(concat(contsupref , '00000')  as bigint) desc)
 
		EXEC Castell_SetValue @tempgmnv, 'contsupref', @ref
		set @label='  '+@label
		EXEC Castell_SetValue @tempgmnv, 'ext', '3'
		EXEC Castell_SetValue @tempgmnv, 'contact', @parentaccountno 
		EXEC Castell_SetValue @gmnv, 'accountno', @accountno
    END

if upper(@level)='CONTACT' or @level='6'
    BEGIN
		set @mergecodes='**'
		select @ref=contsupref from CONTSUPP where RECTYPE='O' and recid=@parentid
		set @ref=@ref+' '+(select contact from CONTACT1 where ACCOUNTNO=@accountno)
  
		EXEC Castell_SetValue @tempgmnv, 'contsupref', @ref
		/*Label the contact's entry*/	
		if isnull(@label,'')='' 
		begin
			/*prefer the contact name*/
			select @label=isnull(contact,'') from CONTACT1 where ACCOUNTNO=@accountno
			if isnull(@label,'')='' 
				begin
					/*fall back to company name*/
					select @label=isnull(company,'') from CONTACT1 where ACCOUNTNO=@accountno
				end
		end

		set @label='  '+@label
		
		EXEC Castell_SetValue @tempgmnv, 'ext', '6'
		EXEC Castell_SetValue @tempgmnv, 'contact', @parentaccountno
		EXEC Castell_SetValue @gmnv, 'accountno', @accountno
    END
  
EXEC Castell_SetValue @tempgmnv, 'address1', @label
EXEC Castell_SetValue @tempgmnv, 'mergecodes', @mergecodes
  
/*and the NULL junk fields*/
EXEC Castell_SetValue @tempgmnv, 'title', NULL
EXEC Castell_SetValue @tempgmnv, 'status', NULL
EXEC Castell_SetValue @tempgmnv, 'dear', NULL
EXEC Castell_SetValue @tempgmnv, 'phone', NULL
EXEC Castell_SetValue @tempgmnv, 'FAX', NULL
EXEC Castell_SetValue @tempgmnv, 'LINKACCT', NULL
EXEC Castell_SetValue @tempgmnv, 'NOTES', NULL
EXEC Castell_SetValue @tempgmnv, 'ADDRESS2', NULL
EXEC Castell_SetValue @tempgmnv, 'ADDRESS3', NULL
EXEC Castell_SetValue @tempgmnv, 'STATE', NULL
EXEC Castell_SetValue @tempgmnv, 'ZIP', NULL
EXEC Castell_SetValue @tempgmnv, 'COUNTRY', NULL
EXEC Castell_SetValue @tempgmnv, 'STATUS', NULL
EXEC Castell_SetValue @tempgmnv, 'LINKEDDOC', NULL
  
/*call WriteContSupp to make the record*/
EXEC Castell_WriteContsupp @tempgmnv
  
EXEC Castell_GetValue @tempgmnv, 'recid', @recid OUTPUT
EXEC Castell_SetValue @gmnv, 'recid', @recid
  
EXEC Castell_Delete @tempgmnv
GO
