# RapTLR (Rapid TLR) Package 

(pronunciation: raptor)

## <font color="blue"> Tutorial </font> 

### 1. Install the package directly using the code below, replace the capitalized words with your JnJ username and password, 

```
## If you don't have it, first install the package remotes, install.packages("remotes")

## To install from Github
remotes::install_github("copy_path_to_this_repo(...git)")

## For comapany BitBucket internal users. If this doesn't work, use install_git() with your username and password as credentials like below.
remotes::install_git("copy_path_to_this_repo(.../raptlr.git)")
## For security reasons, the exact internal path is not displayed starting the 06/27/2025 commit.

## Using credentials instead
gitcred <- git2r::cred_user_pass(username="YOUR_USERNAME",password="PASSWORD/TOKEN(PAT)")
remotes::install_git("copy_path_to_this_repo(.../raptlr.git)", credentials = gitcred)
```

### 2. To use the internal examples, use export_examples() to automatically copy-paste to your local machine or SPACE.
```
library(RapTLR)
export_examples(path_workingFolder = "C:/Documents/TLR_folder/")
```

### 3. First killer feature: Add Appendix automatically. 

Locate where you saved the TLFs and the shell docx. If you copied the example in step 2, you should see 2 folders, "TLF_outputs" and "TLR_shell", containing dummy TLF outputs and TLR shell, respectively. Then run the following code to automatically add TLFs with hyperlinks to the docx. Please use `help(run_apdx)` to read the brand new function documentation!

```
path_TLFs = "REPLACE WITH YOUR OWN PATH C:/Documents/TLR_folder/TLF_outputs"
path_docx = "REPLACE WITH YOUR OWN PATH C:/Documents/TLR_folder/"
path_result = "REPLACE WITH YOUR OWN PATH C:/Documents/TLR_folder/TLR_shell_processed.docx"
library(RapTLR)
# Specifying 3 sections, and allocating tsidem03 to the 1st section, tefmad01a to the second, and both tsfae10 & lsfae03 to the 3rd
run_apdx(path_TLFs = path_TLFs,
         docx_object = file.path(path_docx, "TLR_shell.docx"),
         keyword = "LISTOFAPPENDICES",
         return_to_file = path_result)
```         
         
To specify which listing is in what section titles, use a csv file, two columns, with the first column indicating section titles and the second indicating corresponding TLF files' titles.
```
run_apdx(path_TLFs = path_TLFs,
         docx_object = file.path(path_docx, "TLR_shell.docx"),
         keyword = "LISTOFAPPENDICES",
         sections_structure = "REPLACE WITH YOUR OWN PATH C:/Documents/TLR_folder/TLF_list.csv",
         return_to_file = path_result)
```

Or, you can construct a list telling the function which listings are under what section titles. Remember, sections_structure is optional. If none is specified, no Section titles will be added.
```
# Or a list following the below format :
output_list <- list(Efficacy = "tefmad01a",
                   Overall = "tsidem03",
                   Safety = c("tsfae10", "lsfae03"))
run_apdx(path_TLFs = path_TLFs,
        docx_object = file.path(path_docx, "TLR_shell.docx"),
        sections_structure = output_list)
```

Remember, you don't have to write to file. If return_to_file is ignored or set to NULL, you can catch it as an officer object for subsequent use in R. 
```
outTLR = run_apdx(path_TLFs = path_TLFs,
                 docx_object = file.path(path_docx, "TLR_shell.docx"),
                 keyword = "LISTOFAPPENDICES",
                 sections_structure = REPLACE WITH YOUR OWN PATH C:/Documents/TLR_folder/TLF_list.csv")
```

### 4. Second killer feature: Replace text. 

Topline Report documents often contain a lot of texts with trial output-related numbers. 
To prepare for the report beforehand without programmers' final numbers, you can write the TLR first with these in-text placeholders,
while dynamically calculate the numbers in R. This way, when the final data come out, all you have to do is update your R code, 
and replace all in-line placeholder numbers automatically!

Below we have a basic example of replacing the keywords pre-planted in the TLR shell with your own dynamic texts and numbers. 
In the example shell docx, please note that there are 4 unique keywords as examples for replacing.

<font color="red">**IMPORTANT**</font>: please put your placeholders in their own lines, or else other texts in the same line would be replaced.

<font color="red">**IMPORTANT**</font>: please note that officer package currently modifies objects **in-place**, which means that even if you use our function like so `x=read_docx(...); y = x; x = textReplace(x)`, and y is then **also modified**. Therefore, if you attempt to re-do a maneuver twice, like so `y = textReplace(x, ...="TTsubjectTT"...); y = textReplace(x, ...="TTsubjectTT"...)`. You will encounter and error saying "TTsubjectTT" is not found!! Because it was already replaced in both x and y!

By default, not returning to file, pass the officer object to the next function one by one.

```
## Load some ADaM data and find n_safety = nrow(ADSL[ADSL$SAFFL == "Y", ])
n_safety = 500
new_text = paste0("This study has a safety set totaling ", n_safety, " participants.")

outTLR = textReplace(
            doc_object = file.path(path_docx, "TLR_shell.docx"), ## If you want to edit on whatever your appendix-added file from run_apdx, just use your filename from last step instead. 
            keyword = "TTsubjectTT",
            replacement = new_text
            )

## replace another keyword.
# Calculate efficacy numbers in R, and insert it into text.
rate_remission = 0.3
new_text2 = paste0("Compared to the placebo arm, the treatment arm patients exhibit a mean increase of ", rate_remission*100, "%", " in disease remission rate.")
outTLR = textReplace(
            doc_object = outTLR,
            keyword = "TTsomethingTT",
            replacement = new_text2 )
```

In the end, when you are done replacing texts, you can write to file
```
textReplace(
            doc_object = outTLR,
            keyword = "TTsomething2TT",
            replacement = "New texts",
            return_to_file = file.path(path_docx, "TLR_shell_replacedtexts.docx")
            )

# or you can write to file manually using officer
print( outTLR,  file.path(path_docx, "TLR_shell_replacedtexts.docx") )
```

In fact, now you can even use R's built-in or dplyr's function chains to chain your functions
```
textReplace(
            doc_object = file.path(path_docx, "TLR_shell.docx"),
            keyword = "TTsubjectTT",
            replacement = "This is new text"
            )  |>
  textReplace(keyword = "TTsomethingTT", replacement = "This is new text 2") |>
  textReplace(keyword = "TTsomething2TT", replacement = "This is new text 3") |>
  textReplace(keyword = "TTsomethingelseTT", replacement = "This is some final new text",
              return_to_file = file.path(path_docx, "TLR_shell_replacedtexts.docx"))
              
## OR
library(dplyr)
textReplace(
            doc_object = file.path(path_docx, "TLR_shell.docx"),
            keyword = "TTsubjectTT",
            replacement = "This is new text"
            )  %>%
  textReplace(keyword = "TTsomethingTT", replacement = "This is new text 2") %>%
  textReplace(keyword = "TTsomething2TT", replacement = "This is new text 3") %>%
  textReplace(keyword = "TTsomethingelseTT", replacement = "This is some final new text",
              return_to_file = file.path(path_docx, "TLR_shell_replacedtexts.docx"))

```

---


## Developers

<font color="blue">Package development working group</font>

| Name       | Contribution                                                       |
|-----------------|-----------------------------------------------------------------|
| Lauren Crow         | Code, project management, reviews, outreach               |
| Yannick Vandendijck | Code, reviews        |
| Antoine Stos        | Code, reviews        |
| Caesar Li           | Code, reviews        |
| Xiang Li             | Founder code       |


<font color="blue">Special Acknowledgement</font>

| Name         | Contribution                                               |
|-----------------|--------------------------------------------------------|
| Surya Mohanty      | Sponsor, approver               |

---

