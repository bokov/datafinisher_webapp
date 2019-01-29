helptext <- list(
  disclaimer=includeMarkdown('www/docs/disclaimer.md')
  ,hInfile="
Please upload a .csv file that has been created by this version of DataFinisher or a .db SQLite file created by DataBuilder. Not just any .csv or .db file will work: they have to have been created by DataFinisher or DataBuilder. Excel files are not currently supported."
  ,hMainInfo=tagList(includeMarkdown('www/docs/instructions.md')
                     ,includeMarkdown('www/docs/about.md'))
  ,hInputData="
This is a sample of the the top few rows from the file you uploaded."
  ,hTransform="
Here you can change which transformations are applied to which main columns. One main column can be used to create multiple output columns. If you don't choose anything for a column, it will not be deleted. Rather, default transformations will be applied. The main columns always keep getting passed on with all their metadata so you can re-upload a file produced by DataFinisher and make a different set of choices without having to re-pull the data from i2b2."
  ,hDivChosen=includeMarkdown('www/docs/hDivChosen.md')
  ,hDivAvailable=includeMarkdown('www/docs/hDivAvailable.md')
  ,hSelCodes=includeMarkdown('www/docs/hSelCodes.md')
  ,hCustomTrans="
If none of the built-in transformations do what you need them to do, you can create your own in this tab and specify for which columns it s`hould be available."
  ,hCustomTrName=includeMarkdown('www/docs/hCustomTrName.md')
  ,hCustomTrDesc=includeMarkdown('www/docs/hCustomTrDesc.md')
  ,hCustomWhichCols=includeMarkdown('www/docs/hCustomWhichCols.md')
  ,hCustomWhichFields=includeMarkdown('www/docs/hCustomWhichFields.md')
  ,hCustomAggregate=includeMarkdown('www/docs/hCustomAggregate.md')
  ,hCustomQBHead=includeMarkdown('www/docs/hCustomQBHead.md')
  ,hCustomSave=includeMarkdown('www/docs/hCustomSave.md')
  ,hOutput="
When you want to see how your transformations will look, click  the 'Generate/Update Preview of Results' button. Once it completes, you will get access to the 'Prepare Results for Download Button'. After you click that and  the download is prepared (this may take a while if your file is large) you will see a third button, 'Download Full Results'. This tab does not update automatically. You will need to manually click 'Generate/Update Preview of Results' whenever you want to see the most recent changes you made."
)

messages <- list(
  mLoading='DataFinisher is preparing your file. Large files may take a while to prepare.'
)

