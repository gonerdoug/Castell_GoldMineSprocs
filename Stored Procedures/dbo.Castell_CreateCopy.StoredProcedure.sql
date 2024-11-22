SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_CreateCopy]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_CreateCopy] AS' 
END
GO









ALTER PROCEDURE [dbo].[Castell_CreateCopy] (@gmnv varchar(20), @copy varchar(20) OUTPUT)
    
AS

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL  
    RETURN -120

EXEC Castell_Create @copy OUTPUT

EXEC ('INSERT INTO ##gmnv' + @copy + ' SELECT * FROM ##gmnv' + @gmnv)









GO
