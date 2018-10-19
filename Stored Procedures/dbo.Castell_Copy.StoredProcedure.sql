DROP PROCEDURE [dbo].[Castell_Copy]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO





CREATE PROCEDURE [dbo].[Castell_Copy] (@destgmnv varchar(20), @sourcegmnv varchar(20))
    
AS

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @destgmnv )) IS NULL OR 
    (SELECT object_id('tempdb..##gmnv' + @sourcegmnv )) IS NULL
    RETURN -120

EXEC ('INSERT INTO ##gmnv' + @destgmnv + ' SELECT * FROM ##gmnv' + @sourcegmnv)

RETURN 1




GO
