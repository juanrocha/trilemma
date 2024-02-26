library(pdftools)

## Routine for first submission
pdf_subset("paper/manuscript.pdf",pages = 1:3, output = "paper/title_page.pdf")
pdf_subset("paper/manuscript.pdf", pages = 4:25, output = "paper/main_manuscript.pdf")
pdf_subset("paper/manuscript.pdf", pages = 1, output = "paper/conflict_statement.pdf")


## Routine for second sumbission
pdf_subset("revised_manuscript_230703.pdf", pages = 4:32, output = "revised_main_manuscript.pdf")
pdf_subset("response to reviewers.pdf", pages = 1, output = "letter_editor.pdf")
pdf_subset("response to reviewers.pdf", pages = 2:7, output = "response_reviewers.pdf" )


## Third submission
pdf_subset("response_letter-reviewers.pdf", pages = 1, output = "revision_231118/response_letter.pdf")
pdf_subset("response_letter-reviewers.pdf", pages = 2:4, output = "revision_231118/response_reviewer.pdf")


## Fourth submission
pdf_subset("Response_Feb2024version.pdf", pages = 1, output = "revision_240225/response_letter.pdf")
pdf_subset("Response_Feb2024version.pdf", pages = 2:4, output = "revision_240225/response_reviewer.pdf")
