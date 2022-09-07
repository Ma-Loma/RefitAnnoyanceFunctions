
# erstellen eines docx --------------------------------------
# mit dem Namen "beispiel_word.docx" das Dokument verwendet als Referenz ein Word mit der Bezeichnung "Vorlage QS.docx"
rmarkdown::render("RefitReport.rmd",
                  officedown::rdocx_document(
                    # plots = list(caption = list(style = "Image Caption", # Definition das Abbildungsuntertitel mit der Abbkürzung Abb eingeleitet werden
                    #                             pre = "Abb ",
                    #                             sep = ": ")),
                    reference_docx = "templates/Mittelteil.docx"
                  ), 
                  output_file = "export/RefitReport.docx")

# erstellen eines html dokuments ------------------------------
rmarkdown::render("RefitReport.rmd",
                  rmarkdown::html_document(),
                  output_file = "export/RefitReport.html")

# erstellen eines pdf dokuments --------------------------------------
rmarkdown::render("report_folder/report_template.rmd",
                  rmarkdown::pdf_document(),
                  output_file = "export/beispiel_pdf.pdf")
