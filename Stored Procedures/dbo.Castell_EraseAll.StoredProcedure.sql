DROP PROCEDURE [dbo].[Castell_EraseAll]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[Castell_EraseAll] (@gmnv varchar(20))
    
AS

SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
    RETURN -120

EXEC('DELETE FROM ##gmnv' + @gmnv)




GO
