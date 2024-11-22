SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_EraseAll]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_EraseAll] AS' 
END
GO








ALTER PROCEDURE [dbo].[Castell_EraseAll] (@gmnv varchar(20))
    
AS

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
    RETURN -120

EXEC('DELETE FROM ##gmnv' + @gmnv)






GO
