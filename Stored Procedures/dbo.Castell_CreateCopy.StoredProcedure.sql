DROP PROCEDURE [dbo].[Castell_CreateCopy]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO





CREATE PROCEDURE [dbo].[Castell_CreateCopy] (@gmnv varchar(20), @copy varchar(20) OUTPUT)
    
AS

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL  
    RETURN -120

EXEC Castell_Create @copy OUTPUT

EXEC ('INSERT INTO ##gmnv' + @copy + ' SELECT * FROM ##gmnv' + @gmnv)







GO
