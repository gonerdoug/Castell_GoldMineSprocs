SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_Copy]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_Copy] AS' 
END
GO









ALTER PROCEDURE [dbo].[Castell_Copy] (@destgmnv varchar(20), @sourcegmnv varchar(20))
    
AS

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @destgmnv )) IS NULL OR 
    (SELECT object_id('tempdb..##gmnv' + @sourcegmnv )) IS NULL
    RETURN -120

EXEC ('INSERT INTO ##gmnv' + @destgmnv + ' SELECT * FROM ##gmnv' + @sourcegmnv)

RETURN 1






GO
